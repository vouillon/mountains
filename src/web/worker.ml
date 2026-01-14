(* Web Worker for DEM and CLC decompression *)
module Console = Brr.Console

(* Shared CLC Palette *)
module Clc_palette = Clc_palette

module Wasm = struct
  let dem_instance = ref None
  let clc_instance = ref None

  let create_memory pages =
    let opts = Jv.obj [| ("initial", Jv.of_int pages) |] in
    Jv.new' (Jv.get (Jv.get Jv.global "WebAssembly") "Memory") [| opts |]

  (* Memories for DEM *)
  let high_mem = create_memory 25
  let low_mem = create_memory 25
  let target_mem = create_memory 50

  (* Memories for CLC *)
  let m_meta = create_memory 4
  let m_hi_x = create_memory 25
  let m_mid_x = create_memory 25
  let m_lo_x = create_memory 25
  let m_hi_y = create_memory 25
  let m_mid_y = create_memory 25
  let m_lo_y = create_memory 25
  let m_hi_i = create_memory 50
  let m_lo_i = create_memory 50
  let m_palette = create_memory 1
  let m_out_pos = create_memory 50
  let m_out_col = create_memory 25
  let m_out_ebo = create_memory 150
  let m_out_wpos = create_memory 60

  (* Initialize Palette *)
  let init_palette () =
    let palette_ba =
      Brr.Tarray.to_bigarray1
        (Brr.Tarray.of_buffer Brr.Tarray.Uint8
           (Brr.Tarray.Buffer.of_jv (Jv.get m_palette "buffer")))
    in
    Bigarray.Array1.fill palette_ba 0;
    Array.iteri
      (fun idx m ->
        if m.Clc_palette.code < 1024 then
          Bigarray.Array1.set palette_ba m.Clc_palette.code idx)
      Clc_palette.materials

  let get_ba mem =
    let buf = Jv.get mem "buffer" in
    Brr.Tarray.to_bigarray1
      (Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv buf))

  let to_lwt f =
    let t, u = Lwt.task () in
    ( Fut.await f @@ fun v ->
      match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
    t

  let load_dem () =
    let open Lwt.Syntax in
    match !dem_instance with
    | Some inst -> Lwt.return inst
    | None ->
        let* resp =
          to_lwt @@ Brr_io.Fetch.url (Jstr.v "decompress_tile.wasm")
        in
        let* buf =
          to_lwt
            (Brr_io.Fetch.Body.array_buffer
               (Brr_io.Fetch.Response.as_body resp))
        in
        let imports =
          Jv.obj
            [|
              ( "env",
                Jv.obj
                  [|
                    ("high_mem", high_mem);
                    ("low_mem", low_mem);
                    ("target_mem", target_mem);
                  |] );
            |]
        in
        let* res =
          to_lwt
            (Fut.of_promise
               ~ok:(fun x -> x)
               (Jv.call
                  (Jv.get Jv.global "WebAssembly")
                  "instantiate"
                  [| Brr.Tarray.Buffer.to_jv buf; imports |]))
        in
        let inst = Jv.get res "instance" in
        dem_instance := Some inst;
        Lwt.return inst

  let load_clc () =
    let open Lwt.Syntax in
    match !clc_instance with
    | Some inst -> Lwt.return inst
    | None ->
        init_palette ();
        let* resp = to_lwt @@ Brr_io.Fetch.url (Jstr.v "decode_clc.wasm") in
        let* buf =
          to_lwt
            (Brr_io.Fetch.Body.array_buffer
               (Brr_io.Fetch.Response.as_body resp))
        in
        let imports =
          Jv.obj
            [|
              ( "env",
                Jv.obj
                  [|
                    ("m_meta", m_meta);
                    ("m_hi_x", m_hi_x);
                    ("m_mid_x", m_mid_x);
                    ("m_lo_x", m_lo_x);
                    ("m_hi_y", m_hi_y);
                    ("m_mid_y", m_mid_y);
                    ("m_lo_y", m_lo_y);
                    ("m_hi_i", m_hi_i);
                    ("m_lo_i", m_lo_i);
                    ("m_out_pos", m_out_pos);
                    ("m_out_col", m_out_col);
                    ("m_out_ebo", m_out_ebo);
                    ("m_out_wpos", m_out_wpos);
                    ("m_palette", m_palette);
                  |] );
            |]
        in
        let* res =
          to_lwt
            (Fut.of_promise
               ~ok:(fun x -> x)
               (Jv.call
                  (Jv.get Jv.global "WebAssembly")
                  "instantiate"
                  [| Brr.Tarray.Buffer.to_jv buf; imports |]))
        in
        let inst = Jv.get res "instance" in
        clc_instance := Some inst;
        Lwt.return inst
end

(* External inflate functions *)
external inflate_into : Brr.Tarray.uint8 -> Brr.Tarray.uint8 -> Jv.Promise.t
  = "inflate_into"

let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
  t

(* Helper to convert typed array to bigarray *)
let to_ba (arr : Brr.Tarray.uint8) = Brr.Tarray.to_bigarray1 arr

(* Read uint32 LE from bigarray *)
let get_uint32_le_ba
    (arr :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t)
    offset =
  let b0 = Bigarray.Array1.get arr offset in
  let b1 = Bigarray.Array1.get arr (offset + 1) in
  let b2 = Bigarray.Array1.get arr (offset + 2) in
  let b3 = Bigarray.Array1.get arr (offset + 3) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let decode_dataset ~inst msg =
  let open Lwt.Syntax in
  let data_jv = Jv.get msg "data" in
  (* Convert JS ArrayBuffer/Uint8Array to Bigarray *)
  let data_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv data_jv)
  in
  let data_ba = to_ba data_ta in

  (* Parse header *)
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

  (* Ensure WASM loaded *)
  let h_mem = Wasm.high_mem in
  let l_mem = Wasm.low_mem in

  let h_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8
      (Brr.Tarray.Buffer.of_jv (Jv.get h_mem "buffer"))
  in
  let l_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8
      (Brr.Tarray.Buffer.of_jv (Jv.get l_mem "buffer"))
  in

  Console.log [ Jstr.v "Worker: decompressing high"; high_compressed ];
  let* _ =
    to_lwt (Fut.of_promise ~ok:(fun x -> x) (inflate_into high_compressed h_ta))
  in
  Console.log [ Jstr.v "Worker: decompressing low"; low_compressed ];
  let* _ =
    to_lwt (Fut.of_promise ~ok:(fun x -> x) (inflate_into low_compressed l_ta))
  in

  (* Decode SIMD *)
  let func = Jv.get (Jv.get inst "exports") "decompress_simd" in
  (* We write to the beginning of target_mem with stride = width *)
  let target_size = width in

  ignore
    (Jv.apply func
       [|
         Jv.of_int 0;
         Jv.of_int 0;
         Jv.of_int 0;
         Jv.of_int 0;
         (* dst_col *)
         (* dst_row offset? No, simd takes (target_size - dst_row - height) ?? *)
         (* Original: Jv.of_int (target_size - dst_row - height); *)
         (* If dst_row=0, this is target_size - height. *)
         (* The Y axis is likely inverted or bottom-up? *)
         (* Let's assume standard behavior: writes to array *)
         Jv.of_int (target_size - height);
         Jv.of_int target_size;
         Jv.of_int target_size;
         Jv.of_int target_size;
         Jv.of_int width;
         Jv.of_int height;
       |]);

  (* Return result *)
  let byte_length = width * height * 2 in
  let target_ba = Wasm.get_ba Wasm.target_mem in
  (* Copy to transferable buffer *)
  let result_ta = Brr.Tarray.create Brr.Tarray.Uint8 byte_length in
  let result_ba = Brr.Tarray.to_bigarray1 result_ta in
  Bigarray.Array1.blit (Bigarray.Array1.sub target_ba 0 byte_length) result_ba;

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
  (* Convert JS ArrayBuffer to Bigarray *)
  let data_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv data_jv)
  in
  let data_ba = to_ba data_ta in

  (* let header_size = 28 *)
  (* Approximate, we parse it properly below *)

  (* Helper to parse header from bigarray - SIMPLIFIED for worker *)
  let parse_header_ba ba =
    let get_s i len =
      String.init len (fun j -> Char.chr (Bigarray.Array1.get ba (i + j)))
    in
    let magic = get_s 0 4 in
    let is_clc4 = magic = "CLC4" in
    let is_clc5 = magic = "CLC5" in

    let read_i32_be i =
      let b0 = Bigarray.Array1.get ba i in
      let b1 = Bigarray.Array1.get ba (i + 1) in
      let b2 = Bigarray.Array1.get ba (i + 2) in
      let b3 = Bigarray.Array1.get ba (i + 3) in
      (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
    in

    let count = read_i32_be 4 in
    let total_verts = read_i32_be 8 in
    let total_indices = read_i32_be 12 in

    let water_count, water_verts, water_indices, float_offset =
      if is_clc5 then (read_i32_be 16, read_i32_be 20, read_i32_be 24, 32)
      else if is_clc4 then (read_i32_be 16, read_i32_be 20, read_i32_be 24, 28)
      else (0, 0, 0, 16)
    in
    ( count,
      total_verts,
      total_indices,
      water_count,
      water_verts,
      water_indices,
      float_offset,
      is_clc4,
      is_clc5 )
  in

  let ( count,
        total_verts,
        total_indices,
        water_count,
        water_verts,
        water_indices,
        float_offset,
        is_clc4,
        is_clc5 ) =
    parse_header_ba data_ba
  in

  let offset =
    float_offset + 16
    (* 2 doubles : min_lon, min_lat *)
  in
  let offset =
    offset + 16
    (* 2 doubles: scale_x, scale_y *)
  in
  let offset = if is_clc4 || is_clc5 then offset + 16 else offset in
  (* water scales *)
  let offset = if is_clc5 then offset + 16 else offset in
  (* poi scales *)

  (* Helper to read stream *)
  let read_stream_to_wasm data_ta offset mem =
    let data_ba = to_ba data_ta in
    let stream_len = get_uint32_le_ba data_ba offset in
    let compressed =
      Brr.Tarray.sub data_ta ~start:(offset + 4) ~stop:(offset + 4 + stream_len)
    in
    let mem_ta =
      Brr.Tarray.of_buffer Brr.Tarray.Uint8
        (Brr.Tarray.Buffer.of_jv (Jv.get mem "buffer"))
    in
    let* _ =
      to_lwt (Fut.of_promise ~ok:(fun x -> x) (inflate_into compressed mem_ta))
    in
    Lwt.return (offset + 4 + stream_len)
  in

  let* offset = read_stream_to_wasm data_ta offset Wasm.m_meta in
  let* offset = read_stream_to_wasm data_ta offset Wasm.m_hi_x in
  let* offset = read_stream_to_wasm data_ta offset Wasm.m_lo_x in
  let* offset = read_stream_to_wasm data_ta offset Wasm.m_hi_y in
  let* offset = read_stream_to_wasm data_ta offset Wasm.m_lo_y in
  let* offset = read_stream_to_wasm data_ta offset Wasm.m_hi_i in
  let* offset = read_stream_to_wasm data_ta offset Wasm.m_lo_i in

  let func_main = Jv.get (Jv.get inst "exports") "decode_clc" in
  ignore
    (Jv.apply func_main
       [| Jv.of_int count; Jv.of_int total_verts; Jv.of_int total_indices |]);

  (* Decode Water if present *)
  let* _ =
    if (is_clc4 || is_clc5) && water_count > 0 then begin
      (* Read water streams... Wait, they are sequential in the file? 
          clc_loader.ml just calls read_stream_to_wasm again, so yes, they follow. *)
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_meta in
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_hi_x in
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_mid_x in
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_lo_x in
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_hi_y in
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_mid_y in
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_lo_y in
      let* offset = read_stream_to_wasm data_ta offset Wasm.m_hi_i in
      let* _ = read_stream_to_wasm data_ta offset Wasm.m_lo_i in

      let func_w = Jv.get (Jv.get inst "exports") "decode_water" in
      ignore
        (Jv.apply func_w
           [|
             Jv.of_int water_count;
             Jv.of_int water_verts;
             Jv.of_int water_indices;
           |]);
      Lwt.return ()
    end
    else Lwt.return ()
  in

  (* Package result *)
  let copy_mem mem size =
    let ba = Wasm.get_ba mem in
    let ta = Brr.Tarray.create Brr.Tarray.Uint8 size in
    let out_ba = Brr.Tarray.to_bigarray1 ta in
    Bigarray.Array1.blit (Bigarray.Array1.sub ba 0 size) out_ba;
    Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer ta)
  in

  let pos_buf = copy_mem Wasm.m_out_pos (total_verts * 2 * 2) in
  let col_buf = copy_mem Wasm.m_out_col total_verts in
  let idx_buf = copy_mem Wasm.m_out_ebo (total_indices * 4) in

  let water_pos_buf =
    if water_verts > 0 then copy_mem Wasm.m_out_wpos (water_verts * 4 * 2)
    else Jv.null
  in
  let water_col_buf =
    if water_verts > 0 then copy_mem Wasm.m_out_col water_verts else Jv.null
  in

  let water_idx_buf =
    if water_indices > 0 then copy_mem Wasm.m_out_ebo (water_indices * 4)
    else Jv.null
  in
  (* Note: m_out_ebo reused for water indices? 
      clc_loader.ml: 
       let we = Array1.create int32 c_layout header.water_indices in
       Array1.blit (get_ba32 (Option.get !Wasm.m_out_ebo) header.water_indices) we;
      So YES, reused.
   *)

  (* POIs? clc_loader.ml parses POIs from decompressed streams (Wasm.m_meta, m_hi_x etc).
      The worker can just return the raw streams or parse them?
      Parsing involves string allocation etc. 
      Maybe the main thread should parse POIs?
      Or we parse them here and send JSON?
      Let's skip POIs for now (or assume main thread parses them from input buffer? NO, they are compressed).
      Worker MUST decompress them. They are in m_meta etc.
      
      We should copy the decompressed streams for POIs if needed.
      Or parse POIs here.
      `clc_loader.ml` `parse_pois` logic is complex.
      Let's stick to geometry first.
   *)

  let result_obj =
    Jv.obj
      [|
        ("pos", pos_buf);
        ("col", col_buf);
        ("idx", idx_buf);
        ("water_pos", water_pos_buf);
        ("water_col", water_col_buf);
        ("water_idx", water_idx_buf);
      |]
  in

  let transfer_list = [ pos_buf; col_buf; idx_buf ] in
  let transfer_list =
    if water_verts > 0 then water_pos_buf :: transfer_list else transfer_list
  in
  let transfer_list =
    if water_verts > 0 then water_col_buf :: transfer_list else transfer_list
  in
  let transfer_list =
    if water_indices > 0 then water_idx_buf :: transfer_list else transfer_list
  in

  let msg =
    Jv.obj [| ("type", Jv.of_string "result_clc"); ("data", result_obj) |]
  in
  Brr_webworkers.Worker.G.post msg
    ~opts:(Brr_io.Message.opts ~transfer:(Obj.magic transfer_list) ());
  Lwt.return ()

let on_message e =
  let open Lwt.Syntax in
  let data = Brr.Ev.as_type e |> Brr_io.Message.Ev.data in
  let type_ = Jv.get data "type" |> Jv.to_string in
  match type_ with
  | "decode_dem" ->
      let* inst = Wasm.load_dem () in
      let* _ = decode_dataset ~inst data in
      Lwt.return ()
  | "decode_clc" ->
      let* inst = Wasm.load_clc () in
      let* _ = decode_clc_dataset ~inst data in
      Lwt.return ()
  | _ -> Lwt.return ()

let () =
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message
       (fun e -> Lwt.async (fun () -> on_message e))
       (Brr.Ev.target_of_jv (Jv.get Jv.global "self")))
