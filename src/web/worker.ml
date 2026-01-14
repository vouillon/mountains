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
      match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
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
    match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
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
  let total_needed = scratch_ptr + 65536 in

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

  let result_ta = Brr.Tarray.create Brr.Tarray.Uint8 (decomp_size * 2) in
  let result_ba = Brr.Tarray.to_bigarray1 result_ta in
  let target_ba = Wasm.get_ba () in
  Bigarray.Array1.blit
    (Bigarray.Array1.sub target_ba target_ptr (decomp_size * 2))
    result_ba;

  let msg =
    Jv.obj
      [|
        ("type", Jv.of_string "result_dem");
        ("data", Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer result_ta));
      |]
  in
  Brr_webworkers.Worker.G.post msg
    ~opts:
      (Brr_io.Message.opts
         ~transfer:
           [
             Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer result_ta) |> Obj.magic;
           ]
         ());
  Lwt.return ()

let decode_clc_dataset ~inst msg =
  let open Lwt.Syntax in
  let data_jv = Jv.get msg "data" in
  let data_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv data_jv)
  in
  let data_ba = Brr.Tarray.to_bigarray1 data_ta in

  let read_i32_be ba i =
    let b0 = Bigarray.Array1.get ba i in
    let b1 = Bigarray.Array1.get ba (i + 1) in
    let b2 = Bigarray.Array1.get ba (i + 2) in
    let b3 = Bigarray.Array1.get ba (i + 3) in
    (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
  in

  let magic =
    String.init 4 (fun i -> Char.chr (Bigarray.Array1.get data_ba i))
  in
  let is_clc4 = magic = "CLC4" in
  let is_clc5 = magic = "CLC5" in

  let count = read_i32_be data_ba 4 in
  let total_verts = read_i32_be data_ba 8 in
  let total_indices = read_i32_be data_ba 12 in
  let water_count, water_verts, water_indices, poi_count, header_end =
    if is_clc5 then
      ( read_i32_be data_ba 16,
        read_i32_be data_ba 20,
        read_i32_be data_ba 24,
        read_i32_be data_ba 28,
        32 )
    else if is_clc4 then
      ( read_i32_be data_ba 16,
        read_i32_be data_ba 20,
        read_i32_be data_ba 24,
        0,
        28 )
    else (0, 0, 0, 0, 16)
  in

  let offset = ref (header_end + 32) in
  (* min_lon, lat (16) + scale_x, y (16) *)
  if is_clc4 || is_clc5 then offset := !offset + 16;
  if is_clc5 then offset := !offset + 16;

  let stream_ptr = ref 0 in
  let alloc size =
    let p = !stream_ptr in
    stream_ptr := !stream_ptr + ((size + 15) land lnot 15);
    p
  in

  let m_meta = alloc (count * 6) in
  let m_hi_x = alloc total_verts in
  let m_lo_x = alloc total_verts in
  let m_hi_y = alloc total_verts in
  let m_lo_y = alloc total_verts in
  let m_hi_i = alloc total_indices in
  let m_lo_i = alloc total_indices in
  let m_out_pos = alloc (total_verts * 4) in
  let m_out_col = alloc total_verts in
  let m_out_ebo = alloc (total_indices * 4) in
  let m_palette = alloc 1024 in

  let m_mid_x, m_mid_y, m_out_wpos =
    if water_verts > 0 then
      (alloc water_verts, alloc water_verts, alloc (water_verts * 8))
    else (0, 0, 0)
  in

  Wasm.ensure_memory_bytes (!stream_ptr + 1024);
  Wasm.init_palette m_palette;
  let mem_ta = Wasm.get_ta () in

  let read_stream data_ta off dest_ptr =
    let len = get_uint32_le_ba (Brr.Tarray.to_bigarray1 data_ta) !off in
    let compressed =
      Brr.Tarray.sub data_ta ~start:(!off + 4) ~stop:(!off + 4 + len)
    in
    let* _ =
      to_lwt
        (Fut.of_promise
           ~ok:(fun x -> x)
           (inflate_into compressed mem_ta dest_ptr))
    in
    off := !off + 4 + len;
    Lwt.return ()
  in

  let* () = read_stream data_ta offset m_meta in
  let* () = read_stream data_ta offset m_hi_x in
  let* () = read_stream data_ta offset m_lo_x in
  let* () = read_stream data_ta offset m_hi_y in
  let* () = read_stream data_ta offset m_lo_y in
  let* () = read_stream data_ta offset m_hi_i in
  let* () = read_stream data_ta offset m_lo_i in

  let func_main = Jv.get (Jv.get inst "exports") "decode_clc" in
  ignore
    (Jv.apply func_main
       [|
         Jv.of_int count;
         Jv.of_int total_verts;
         Jv.of_int total_indices;
         Jv.of_int m_meta;
         Jv.of_int m_hi_x;
         Jv.of_int m_lo_x;
         Jv.of_int m_hi_y;
         Jv.of_int m_lo_y;
         Jv.of_int m_hi_i;
         Jv.of_int m_lo_i;
         Jv.of_int m_out_pos;
         Jv.of_int m_out_col;
         Jv.of_int m_out_ebo;
         Jv.of_int m_palette;
       |]);

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

  let* water_pos_buf, water_col_buf, water_idx_buf =
    if water_count > 0 then (
      let* () = read_stream data_ta offset m_meta in
      let* () = read_stream data_ta offset m_hi_x in
      let* () = read_stream data_ta offset m_mid_x in
      let* () = read_stream data_ta offset m_lo_x in
      let* () = read_stream data_ta offset m_hi_y in
      let* () = read_stream data_ta offset m_mid_y in
      let* () = read_stream data_ta offset m_lo_y in
      let* () = read_stream data_ta offset m_hi_i in
      let* () = read_stream data_ta offset m_lo_i in
      let func_w = Jv.get (Jv.get inst "exports") "decode_water" in
      ignore
        (Jv.apply func_w
           [|
             Jv.of_int water_count;
             Jv.of_int water_verts;
             Jv.of_int water_indices;
             Jv.of_int m_meta;
             Jv.of_int m_hi_x;
             Jv.of_int m_mid_x;
             Jv.of_int m_lo_x;
             Jv.of_int m_hi_y;
             Jv.of_int m_mid_y;
             Jv.of_int m_lo_y;
             Jv.of_int m_hi_i;
             Jv.of_int m_lo_i;
             Jv.of_int m_out_wpos;
             Jv.of_int m_out_col;
             Jv.of_int m_out_ebo;
             Jv.of_int m_palette;
           |]);
      let wp = copy_mem m_out_wpos (water_verts * 8) in
      let wc = copy_mem m_out_col water_verts in
      let wi = copy_mem m_out_ebo (water_indices * 4) in
      Lwt.return (wp, wc, wi))
    else Lwt.return (Jv.null, Jv.null, Jv.null)
  in

  let read_and_inflate data_ta off =
    let len = get_uint32_le_ba (Brr.Tarray.to_bigarray1 data_ta) !off in
    let compressed =
      Brr.Tarray.sub data_ta ~start:(!off + 4) ~stop:(!off + 4 + len)
    in
    let* inflated =
      to_lwt (Fut.of_promise ~ok:(fun x -> x) (inflate compressed))
    in
    let inflated_ta = Brr.Tarray.of_jv inflated in
    off := !off + 4 + len;
    Lwt.return (Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer inflated_ta))
  in

  let* poi_names, poi_coords, poi_elevs, poi_types =
    if is_clc5 && poi_count > 0 then
      let* n = read_and_inflate data_ta offset in
      let* c = read_and_inflate data_ta offset in
      let* e = read_and_inflate data_ta offset in
      let* t = read_and_inflate data_ta offset in
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
  if poi_count > 0 then
    transfer_list :=
      poi_names :: poi_coords :: poi_elevs :: poi_types :: !transfer_list;

  let msg =
    Jv.obj [| ("type", Jv.of_string "result_clc"); ("data", result_obj) |]
  in
  Brr_webworkers.Worker.G.post msg
    ~opts:(Brr_io.Message.opts ~transfer:(Obj.magic !transfer_list) ());
  Lwt.return ()

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
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message
       (fun e -> Lwt.async (fun () -> on_message ~dem_inst ~clc_inst e))
       (Brr.Ev.target_of_jv (Jv.get Jv.global "self")))
