(* Web Worker for DEM and CLC decompression *)
module Console = Brr.Console

(* Shared CLC Palette *)
module Clc_palette = Clc_palette

module Wasm = struct
  let dem_instance = ref None
  let clc_instance = ref None

  let memory =
    let opts =
      Jv.obj [| ("initial", Jv.of_int 300); ("maximum", Jv.of_int 4096) |]
    in
    Jv.new' (Jv.get (Jv.get Jv.global "WebAssembly") "Memory") [| opts |]

  let ensure_memory_bytes needed =
    let current_bytes =
      Jv.get (Jv.get memory "buffer") "byteLength" |> Jv.to_int
    in
    if needed > current_bytes then
      let current_pages = (current_bytes + 65535) / 65536 in
      let needed_pages = (needed + 65535) / 65536 in
      let delta = needed_pages - current_pages in
      if delta > 0 then ignore (Jv.call memory "grow" [| Jv.of_int delta |])

  let get_ta () =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8
      (Brr.Tarray.Buffer.of_jv (Jv.get memory "buffer"))

  let get_ba () = Brr.Tarray.to_bigarray1 (get_ta ())

  let init_palette offset =
    let palette_ba = get_ba () in
    for i = 0 to 1023 do
      Bigarray.Array1.set palette_ba (offset + i) 0
    done;
    Array.iteri
      (fun idx m ->
        if m.Clc_palette.code < 1024 then
          Bigarray.Array1.set palette_ba (offset + m.Clc_palette.code) idx)
      Clc_palette.materials

  let to_lwt f =
    let t, u = Lwt.task () in
    ( Fut.await f @@ fun v ->
      match v with
      | Ok v -> Lwt.wakeup u v
      | Error err -> Lwt.wakeup_exn u (Jv.Error err) );
    t

  let load_wasm url =
    let open Lwt.Syntax in
    let imports = Jv.obj [| ("env", Jv.obj [| ("memory", memory) |]) |] in
    let* resp = to_lwt @@ Brr_io.Fetch.url (Jstr.v url) in
    let* res =
      to_lwt
        (Fut.of_promise
           ~ok:(fun x -> x)
           (Jv.call
              (Jv.get Jv.global "WebAssembly")
              "instantiateStreaming"
              [| Brr_io.Fetch.Response.to_jv resp; imports |]))
    in
    Lwt.return (Jv.get res "instance")

  let load_dem () =
    let open Lwt.Syntax in
    match !dem_instance with
    | Some inst -> Lwt.return inst
    | None ->
        let* inst = load_wasm "decompress_tile.wasm" in
        dem_instance := Some inst;
        Lwt.return inst

  (* The ring blend ([Blend_wasm]), on the same memory: a worker takes one
     request at a time. Loaded in the worker kept for blends as soon as it
     starts ([Worker_pool]'s "preload_blend"): on its first blend, the fetch
     and compile took 1.4-1.8 s off the first ring. *)
  let blend_instance =
    lazy
      (Lwt.catch
         (fun () -> Lwt.map Option.some (load_wasm "blend.wasm"))
         (fun _ -> Lwt.return None))

  let load_clc () =
    let open Lwt.Syntax in
    match !clc_instance with
    | Some inst -> Lwt.return inst
    | None ->
        let* inst = load_wasm "decode_clc.wasm" in
        clc_instance := Some inst;
        Lwt.return inst
end

(* External inflate functions *)
external inflate_into :
  Brr.Tarray.uint8 -> Brr.Tarray.uint8 -> int -> Jv.Promise.t = "inflate_into"

external inflate : Brr.Tarray.uint8 -> Jv.Promise.t = "inflate"

let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with
    | Ok v -> Lwt.wakeup u v
    | Error err -> Lwt.wakeup_exn u (Jv.Error err) );
  t

(* Read uint32 LE from bigarray *)
let get_uint32_le_ba ba offset =
  let b0 = Bigarray.Array1.get ba offset in
  let b1 = Bigarray.Array1.get ba (offset + 1) in
  let b2 = Bigarray.Array1.get ba (offset + 2) in
  let b3 = Bigarray.Array1.get ba (offset + 3) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let decode_dataset ~inst msg =
  let open Lwt.Syntax in
  let data_jv = Jv.get msg "data" in
  let data_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv data_jv)
  in
  let data_ba = Brr.Tarray.to_bigarray1 data_ta in

  let header_size = 28 in
  (* Validate the header: the -500..9000 m elevation range is hardcoded in
     [Dem_loader.get_height], the sea-fill value and the terrain shaders, so
     a tile encoded with a different range must fail loudly here rather than
     silently decode to wrong heights. *)
  let magic =
    if Bigarray.Array1.dim data_ba < header_size then ""
    else String.init 4 (fun i -> Char.chr (Bigarray.Array1.get data_ba i))
  in
  (* HDT1, our own high-resolution tiles (Hd_dem.Own_tiles), shares the layout
     but carries a scale of its own in place of the range, which the page
     applies: only the residuals are undone here. *)
  if magic <> "DEM1" && magic <> "HDT1" then failwith "DEM tile: bad magic";
  let get_f32_le offset =
    let b i = Int32.of_int (Bigarray.Array1.get data_ba (offset + i)) in
    Int32.float_of_bits
      (Int32.logor (b 0)
         (Int32.logor
            (Int32.shift_left (b 1) 8)
            (Int32.logor
               (Int32.shift_left (b 2) 16)
               (Int32.shift_left (b 3) 24))))
  in
  let min_elev = get_f32_le 12 in
  let max_elev = get_f32_le 16 in
  if magic = "DEM1" && (min_elev <> -500.0 || max_elev <> 9000.0) then
    failwith
      (Printf.sprintf
         "DEM tile: elevation range %g..%g does not match the expected \
          -500..9000"
         min_elev max_elev);
  let width = get_uint32_le_ba data_ba 4 in
  let height = get_uint32_le_ba data_ba 8 in
  let high_size = get_uint32_le_ba data_ba 20 in
  let low_size = get_uint32_le_ba data_ba 24 in

  let high_compressed =
    Brr.Tarray.sub data_ta ~start:header_size ~stop:(header_size + high_size)
  in
  let low_compressed =
    Brr.Tarray.sub data_ta ~start:(header_size + high_size)
      ~stop:(header_size + high_size + low_size)
  in

  let decomp_size = width * height in
  let high_ptr = 0 in
  let low_ptr = high_ptr + decomp_size in
  let target_ptr = low_ptr + decomp_size in
  let scratch_ptr = target_ptr + (decomp_size * 2) in
  (* An HDT1 tile's heights go to float32 after the scratch area, for as many
     samples as [scale_hdt] writes: a multiple of 8. *)
  let hdt_ptr = scratch_ptr + 65536 in
  let hdt_samples = (decomp_size + 7) land lnot 7 in
  let total_needed =
    if magic = "HDT1" then hdt_ptr + (hdt_samples * 4) else hdt_ptr
  in

  Wasm.ensure_memory_bytes total_needed;
  let mem_ta = Wasm.get_ta () in

  let* _ =
    to_lwt
      (Fut.of_promise
         ~ok:(fun x -> x)
         (inflate_into high_compressed mem_ta high_ptr))
  in
  let* _ =
    to_lwt
      (Fut.of_promise
         ~ok:(fun x -> x)
         (inflate_into low_compressed mem_ta low_ptr))
  in

  let func = Jv.get (Jv.get inst "exports") "decompress_simd" in
  ignore
    (Jv.apply func
       [|
         Jv.of_int high_ptr;
         Jv.of_int low_ptr;
         Jv.of_int target_ptr;
         Jv.of_int scratch_ptr;
         Jv.of_int 0;
         Jv.of_int 0;
         Jv.of_int width;
         Jv.of_int height;
         Jv.of_int width;
         Jv.of_int width;
         Jv.of_int height;
       |]);

  let kind, result =
    if magic = "HDT1" then begin
      (* An HDT1 tile's heights, scaled here rather than on the page, where
         every bigarray element access is a call under wasm_of_ocaml, and by
         [scale_hdt] (decompress_tile.wat) rather than a JavaScript loop: on
         a Pixel 8a, that loop was the largest item of the decode workers'
         time in a switch. The range fields hold the tile's base and step;
         the arithmetic is the page's, so the same float32 heights. The
         result is copied out of the wasm memory, which is reused. *)
      let base = min_elev and step = max_elev in
      ignore
        (Jv.call (Jv.get inst "exports") "scale_hdt"
           [|
             Jv.of_int target_ptr;
             Jv.of_int hdt_ptr;
             Jv.of_int hdt_samples;
             Jv.of_float base;
             Jv.of_float step;
           |]);
      let heights =
        Brr.Tarray.of_buffer Brr.Tarray.Float32 ~byte_offset:hdt_ptr
          ~length:decomp_size
          (Brr.Tarray.Buffer.of_jv (Jv.get Wasm.memory "buffer"))
      in
      let copy : Brr.Tarray.float32 =
        Brr.Tarray.of_jv (Jv.call (Brr.Tarray.to_jv heights) "slice" [||])
      in
      ("result_hdt", Brr.Tarray.buffer copy)
    end
    else begin
      let result_ta = Brr.Tarray.create Brr.Tarray.Uint8 (decomp_size * 2) in
      let result_ba = Brr.Tarray.to_bigarray1 result_ta in
      let target_ba = Wasm.get_ba () in
      Bigarray.Array1.blit
        (Bigarray.Array1.sub target_ba target_ptr (decomp_size * 2))
        result_ba;
      ("result_dem", Brr.Tarray.buffer result_ta)
    end
  in
  let msg =
    Jv.obj
      [|
        ("type", Jv.of_string kind); ("data", Brr.Tarray.Buffer.to_jv result);
      |]
  in
  Brr_webworkers.Worker.G.post msg
    ~opts:
      (Brr_io.Message.opts
         ~transfer:[ Brr.Tarray.Buffer.to_jv result |> Obj.magic ]
         ());
  Lwt.return ()

(* A CLC6 tile (see Corine/clc_file.ml): the land cover and the water decoded
   by decode_clc.wasm, the POI streams inflated for the page to read. *)
let decode_clc_dataset ~inst msg =
  let open Lwt.Syntax in
  (* The POI streams only when asked for ([Clc_loader]): a land-cover
     sub-cell's are copies of its POI tile's, which the page reads instead --
     4 inflates a tile, a third of its decode on a Pixel 8a. *)
  let want_pois =
    let v = Jv.get msg "pois" in
    Jv.is_none v || Jv.to_bool v
  in
  let data_jv = Jv.get msg "data" in
  let data_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv data_jv)
  in
  let data_ba = Brr.Tarray.to_bigarray1 data_ta in

  let read_i32_be i =
    let b0 = Bigarray.Array1.get data_ba i in
    let b1 = Bigarray.Array1.get data_ba (i + 1) in
    let b2 = Bigarray.Array1.get data_ba (i + 2) in
    let b3 = Bigarray.Array1.get data_ba (i + 3) in
    (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
  in

  let magic =
    String.init 4 (fun i -> Char.chr (Bigarray.Array1.get data_ba i))
  in
  if Bigarray.Array1.dim data_ba < 112 || magic <> "CLC6" then
    failwith "CLC tile: bad magic";
  let count = read_i32_be 4 in
  let total_verts = read_i32_be 8 in
  let total_indices = read_i32_be 12 in
  let water_count = read_i32_be 16 in
  let water_verts = read_i32_be 20 in
  let water_indices = read_i32_be 24 in
  let poi_count = read_i32_be 28 in
  let arcs = read_i32_be 96 in
  let walk = read_i32_be 100 in
  let water_arcs = read_i32_be 104 in
  let water_walk = read_i32_be 108 in

  (* The 22 streams: offset of the compressed data, its length, the length
     inflated *)
  let streams =
    let offset = ref 112 in
    Array.init 22 (fun _ ->
        let len = get_uint32_le_ba data_ba !offset in
        let raw = get_uint32_le_ba data_ba (!offset + 4) in
        let s = (!offset + 8, len, raw) in
        offset := !offset + 8 + len;
        s)
  in

  let stream_ptr = ref 0 in
  let alloc size =
    let p = !stream_ptr in
    stream_ptr := !stream_ptr + ((size + 15) land lnot 15);
    p
  in
  (* The land cover's eight streams and the water's ten, inflated *)
  let inputs =
    Array.init 18 (fun i ->
        let _, _, raw = streams.(i) in
        alloc raw)
  in
  let m_out_pos = alloc (total_verts * 4) in
  let m_out_col = alloc total_verts in
  let m_out_ebo = alloc (total_indices * 4) in
  let m_out_wpos = alloc (water_verts * 8) in
  let m_out_wcol = alloc water_verts in
  let m_out_webo = alloc (water_indices * 4) in
  let m_palette = alloc 1024 in
  let walk = max walk water_walk in
  let m_arcs = alloc (max arcs water_arcs * 12) in
  let m_rings = alloc (walk * 8) in
  let m_walk = alloc (walk * 4) in
  let m_stack = alloc (walk * 4) in

  Wasm.ensure_memory_bytes (!stream_ptr + 1024);
  Wasm.init_palette m_palette;
  let mem_ta = Wasm.get_ta () in

  let* () =
    Lwt.join
      (List.init 18 (fun i ->
           let off, len, _ = streams.(i) in
           if len = 0 then Lwt.return ()
           else
             let* _ =
               to_lwt
                 (Fut.of_promise
                    ~ok:(fun x -> x)
                    (inflate_into
                       (Brr.Tarray.sub data_ta ~start:off ~stop:(off + len))
                       mem_ta inputs.(i)))
             in
             Lwt.return ()))
  in

  let exports = Jv.get inst "exports" in
  let decode ~count ~wide ~first ~pos ~col ~ebo ~verts ~indices =
    let s i = Jv.of_int inputs.(first + i) in
    let coords =
      if wide then [| s 3; s 4; s 5; s 6; s 7; s 8 |]
      else [| s 3; Jv.of_int 0; s 4; s 5; Jv.of_int 0; s 6 |]
    in
    ignore
      (Jv.apply
         (Jv.get exports "start_layer")
         (Array.concat
            [
              [|
                Jv.of_int count;
                Jv.of_int (if wide then 1 else 0);
                s 0;
                s 1;
                s 2;
              |];
              coords;
              [|
                s (if wide then 9 else 7);
                Jv.of_int pos;
                Jv.of_int col;
                Jv.of_int ebo;
                Jv.of_int m_palette;
                Jv.of_int m_arcs;
                Jv.of_int m_rings;
                Jv.of_int m_walk;
                Jv.of_int m_stack;
              |];
            ]));
    (* In calls of 256 features: the engine runs the first ones on code
       compiled in haste, and only a call to come can have better *)
    while
      Jv.to_int
        (Jv.apply (Jv.get exports "decode_features") [| Jv.of_int 256 |])
      > 0
    do
      ()
    done;
    let written = Jv.to_int (Jv.apply (Jv.get exports "indices") [||]) in
    let vertices = Jv.to_int (Jv.apply (Jv.get exports "vertices") [||]) in
    if written <> indices || vertices <> verts then
      failwith
        (Printf.sprintf
           "CLC tile: decoded %d vertices and %d indices for %d and %d" vertices
           written verts indices)
  in
  decode ~count ~wide:false ~first:0 ~pos:m_out_pos ~col:m_out_col
    ~ebo:m_out_ebo ~verts:total_verts ~indices:total_indices;
  if water_count > 0 then
    decode ~count:water_count ~wide:true ~first:8 ~pos:m_out_wpos
      ~col:m_out_wcol ~ebo:m_out_webo ~verts:water_verts ~indices:water_indices;

  let copy_mem ptr size =
    let ba = Wasm.get_ba () in
    let ta = Brr.Tarray.create Brr.Tarray.Uint8 size in
    Bigarray.Array1.blit
      (Bigarray.Array1.sub ba ptr size)
      (Brr.Tarray.to_bigarray1 ta);
    Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer ta)
  in

  let pos_buf = copy_mem m_out_pos (total_verts * 4) in
  let col_buf = copy_mem m_out_col total_verts in
  let idx_buf = copy_mem m_out_ebo (total_indices * 4) in
  let water_pos_buf, water_col_buf, water_idx_buf =
    if water_count > 0 then
      ( copy_mem m_out_wpos (water_verts * 8),
        copy_mem m_out_wcol water_verts,
        copy_mem m_out_webo (water_indices * 4) )
    else (Jv.null, Jv.null, Jv.null)
  in

  let read_and_inflate i =
    let off, len, _ = streams.(i) in
    let* inflated =
      to_lwt
        (Fut.of_promise
           ~ok:(fun x -> x)
           (inflate (Brr.Tarray.sub data_ta ~start:off ~stop:(off + len))))
    in
    let inflated_ta = Brr.Tarray.of_jv inflated in
    Lwt.return (Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer inflated_ta))
  in

  let* poi_names, poi_coords, poi_elevs, poi_types =
    if poi_count > 0 && want_pois then
      let* n = read_and_inflate 18 in
      let* c = read_and_inflate 19 in
      let* e = read_and_inflate 20 in
      let* t = read_and_inflate 21 in
      Lwt.return (n, c, e, t)
    else Lwt.return (Jv.null, Jv.null, Jv.null, Jv.null)
  in

  let result_obj =
    Jv.obj
      [|
        ("pos", pos_buf);
        ("col", col_buf);
        ("idx", idx_buf);
        ("water_pos", water_pos_buf);
        ("water_col", water_col_buf);
        ("water_idx", water_idx_buf);
        ("poi_names", poi_names);
        ("poi_coords", poi_coords);
        ("poi_elevs", poi_elevs);
        ("poi_types", poi_types);
      |]
  in

  let transfer_list = ref [ pos_buf; col_buf; idx_buf ] in
  if water_count > 0 then
    transfer_list :=
      water_pos_buf :: water_col_buf :: water_idx_buf :: !transfer_list;
  if poi_count > 0 && want_pois then
    transfer_list :=
      poi_names :: poi_coords :: poi_elevs :: poi_types :: !transfer_list;

  let msg =
    Jv.obj [| ("type", Jv.of_string "result_clc"); ("data", result_obj) |]
  in
  Brr_webworkers.Worker.G.post msg
    ~opts:(Brr_io.Message.opts ~transfer:(Obj.magic !transfer_list) ());
  Lwt.return ()

(* A chain of ring blends ([Blend_wasm.run_chain_in_worker]): the plans come
   made, so this moves the bytes in and out around the calls the page would
   have made, and stands each ring on the one before. *)
let blend_dataset msg =
  let open Lwt.Syntax in
  let post ?(transfer = []) type_ data =
    Brr_webworkers.Worker.G.post
      (Jv.obj [| ("type", Jv.of_string type_); ("data", data) |])
      ~opts:
        (Brr_io.Message.opts
           ~transfer:(List.map Brr_io.Message.transfer transfer)
           ())
  in
  let d = Jv.get msg "data" in
  let rings = Jv.to_jv_list (Jv.get d "rings") in
  let* inst = Lazy.force Wasm.blend_instance in
  match inst with
  | None -> Lwt.return (post "blend_unavailable" Jv.null)
  | Some inst
    when List.exists (fun r -> not (Jv.is_none (Jv.get r "warp"))) rings
         && Jv.is_none (Jv.get (Jv.get inst "exports") "set_warp") ->
      (* A module from before it took turned axes, out of a stale cache. *)
      Lwt.return (post "blend_unavailable" Jv.null)
  | Some inst ->
      let exports = Jv.get inst "exports" in
      let u8 buf ~off ~len =
        Brr.Tarray.of_buffer Brr.Tarray.Uint8 ~byte_offset:off ~length:len
          (Brr.Tarray.Buffer.of_jv buf)
      in
      (* [prev]: the output of the ring before, and its height scale and
         offset. [None] for the first ring, whose window comes made. *)
      let rec chain prev acc = function
        | [] -> List.rev acc
        | r :: rest ->
            let layout = Jv.to_jv_array (Jv.get r "layout") in
            let at i = Jv.to_int layout.(i) in
            let n = at 0 and win_len = at 1 and samples_ptr = at 2 in
            let win_ptr = at 3 and out_ptr = at 4 and result_ptr = at 5 in
            let args = Jv.to_jv_array (Jv.get r "args") in
            let arg i = Jv.to_int args.(i) in
            Wasm.ensure_memory_bytes (at 6);
            (* After the growth, which detaches the buffer it replaces. *)
            let buffer = Jv.get Wasm.memory "buffer" in
            Brr.Tarray.set_tarray
              (Brr.Tarray.of_buffer Brr.Tarray.Float32 ~byte_offset:samples_ptr
                 ~length:n
                 (Brr.Tarray.Buffer.of_jv buffer))
              ~dst:0
              (Brr.Tarray.of_jv (Jv.get r "samples"));
            let win = u8 buffer ~off:win_ptr ~len:win_len in
            (match prev with
            | None ->
                let w = Jv.get d "win" in
                Brr.Tarray.set_tarray win ~dst:0
                  (u8 w ~off:0 ~len:(Jv.to_int (Jv.get w "byteLength")))
            | Some (out, scale, offset) ->
                (* As [Hd_dem.source_window] cuts it, from the grid below. *)
                let src_size = arg 7
                and win_cols = arg 8
                and win_rows = arg 9 in
                let col_lo = arg 10 and row_lo = arg 11 in
                let stride = win_cols * 2 in
                for row = 0 to win_rows - 1 do
                  Brr.Tarray.set_tarray win ~dst:(row * stride)
                    (u8 out
                       ~off:((((row_lo + row) * src_size) + col_lo) * 2)
                       ~len:stride)
                done;
                args.(18) <- scale;
                args.(19) <- offset);
            let warp = Jv.get r "warp" in
            if not (Jv.is_none warp) then
              ignore
                (Jv.apply (Jv.get exports "set_warp") (Jv.to_jv_array warp));
            let t0 = Brr.Performance.now_ms Brr.G.performance in
            let ok = Jv.to_int (Jv.apply (Jv.get exports "blend") args) in
            let ms = Brr.Performance.now_ms Brr.G.performance -. t0 in
            if ok = 0 then
              (* No valid sample: the plans after this one stood on it. *)
              List.rev (Jv.null :: acc)
            else
              let out =
                Jv.call buffer "slice"
                  [| Jv.of_int out_ptr; Jv.of_int (out_ptr + (n * 2)) |]
              in
              let f i =
                Jv.Jarray.get
                  (Brr.Tarray.to_jv
                     (Brr.Tarray.of_buffer Brr.Tarray.Float64
                        ~byte_offset:result_ptr ~length:3
                        (Brr.Tarray.Buffer.of_jv buffer)))
                  i
              in
              let res =
                Jv.obj
                  [|
                    ("data", out);
                    ("height_scale", f 0);
                    ("height_offset", f 1);
                    ("range", f 2);
                    ("ms", Jv.of_float ms);
                  |]
              in
              chain (Some (out, f 0, f 1)) (res :: acc) rest
      in
      let results = chain None [] rings in
      Lwt.return
        (post
           ~transfer:
             (List.filter_map
                (fun r -> if Jv.is_none r then None else Some (Jv.get r "data"))
                results)
           "result_blend" (Jv.of_jv_list results))

let on_message ~dem_inst ~clc_inst e =
  let open Lwt.Syntax in
  let data = Brr.Ev.as_type e |> Brr_io.Message.Ev.data in
  let type_ = Jv.get data "type" |> Jv.to_string in
  Lwt.catch
    (fun () ->
      match type_ with
      | "decode_dem" ->
          let* inst = dem_inst in
          let* _ = decode_dataset ~inst data in
          Lwt.return ()
      | "decode_clc" ->
          let* inst = clc_inst in
          let* _ = decode_clc_dataset ~inst data in
          Lwt.return ()
      | "blend" -> blend_dataset data
      | "preload_blend" ->
          ignore (Lazy.force Wasm.blend_instance);
          Lwt.return ()
      | _ -> Lwt.return ())
    (fun exn ->
      let err_msg = Printexc.to_string exn in
      Console.error [ Jstr.v "Worker Error"; Jv.of_string err_msg ];
      let msg =
        Jv.obj
          [|
            ("type", Jv.of_string "error"); ("message", Jv.of_string err_msg);
          |]
      in
      Brr_webworkers.Worker.G.post msg;
      Lwt.return ())

let () =
  let dem_inst = Wasm.load_dem () in
  let clc_inst = Wasm.load_clc () in
  (* One message after the other: a decode awaits its inflates, and two at once
     would share the wasm memory's fixed offsets -- and reply out of order,
     where [Worker_pool] matches replies to requests by order. The page may
     send several ([Worker_pool.decode_depth]); this keeps them in line.
     [on_message] never fails, so the chain does not break. *)
  let last = ref Lwt.return_unit in
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message
       (fun e ->
         let prev = !last in
         let t = Lwt.bind prev (fun () -> on_message ~dem_inst ~clc_inst e) in
         last := t;
         Lwt.async (fun () -> t))
       (Brr.Ev.target_of_jv (Jv.get Jv.global "self")))
