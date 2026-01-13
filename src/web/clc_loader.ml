(* clc_loader.ml - Load and rasterize CLC tiles for web viewer *)

open Bigarray

(* CLC tile header info *)
type clc_header = {
  count : int;
  total_verts : int;
  total_indices : int;
  water_count : int; (* CLC4+ only *)
  water_verts : int; (* CLC4+ only *)
  water_indices : int; (* CLC4+ only *)
  poi_count : int; (* CLC5 only *)
  min_lon : float;
  min_lat : float;
  scale_x : float;
  scale_y : float;
  water_scale_x : float; (* CLC4+ only *)
  water_scale_y : float; (* CLC4+ only *)
  poi_scale_x : float; (* CLC5 only *)
  poi_scale_y : float; (* CLC5 only *)
  is_clc4 : bool;
  is_clc5 : bool;
}

(* POI data *)
type poi_type = Peak | Saddle

type poi = {
  name : string;
  lat : float;
  lon : float;
  elevation : int;
  poi_type : poi_type;
}

(* Parsed CLC tile data ready for rasterization *)
type clc_tile = {
  header : clc_header;
  positions : (int, int16_unsigned_elt, c_layout) Array1.t; (* x,y pairs U16 *)
  colors : (int, int8_unsigned_elt, c_layout) Array1.t; (* palette indices *)
  indices : (int32, int32_elt, c_layout) Array1.t;
  (* Water layer (CLC4+ only) - stored as int32 for full 24-bit precision *)
  water_positions : (int32, int32_elt, c_layout) Array1.t;
  water_colors : (int, int8_unsigned_elt, c_layout) Array1.t;
  water_indices : (int32, int32_elt, c_layout) Array1.t;
  (* POI data (CLC5 only) *)
  pois : poi list;
}

(* Helper to parse header from bigarray *)
let parse_header_ba ba =
  let get_s i len =
    String.init len (fun j -> Char.chr (Array1.get ba (i + j)))
  in
  let magic = get_s 0 4 in
  let is_clc4 = magic = "CLC4" in
  let is_clc5 = magic = "CLC5" in

  let read_i32_be i =
    let b0 = Array1.get ba i in
    let b1 = Array1.get ba (i + 1) in
    let b2 = Array1.get ba (i + 2) in
    let b3 = Array1.get ba (i + 3) in
    (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
  in

  let read_f64_le i =
    let rec loop j acc =
      if j < 0 then acc
      else
        loop (j - 1)
          (Int64.logor (Int64.shift_left acc 8)
             (Int64.of_int (Array1.get ba (i + j))))
    in
    Int64.float_of_bits (loop 7 0L)
  in

  let count = read_i32_be 4 in
  let total_verts = read_i32_be 8 in
  let total_indices = read_i32_be 12 in

  let water_count, water_verts, water_indices, poi_count, float_offset =
    if is_clc5 then
      (read_i32_be 16, read_i32_be 20, read_i32_be 24, read_i32_be 28, 32)
    else if is_clc4 then (read_i32_be 16, read_i32_be 20, read_i32_be 24, 0, 28)
    else (0, 0, 0, 0, 16)
  in

  let min_lon = read_f64_le float_offset in
  let min_lat = read_f64_le (float_offset + 8) in
  let scale_x = read_f64_le (float_offset + 16) in
  let scale_y = read_f64_le (float_offset + 24) in

  let water_scale_x, water_scale_y, poi_scale_x, poi_scale_y =
    if is_clc5 then
      ( read_f64_le (float_offset + 32),
        read_f64_le (float_offset + 40),
        read_f64_le (float_offset + 48),
        read_f64_le (float_offset + 56) )
    else if is_clc4 then
      ( read_f64_le (float_offset + 32),
        read_f64_le (float_offset + 40),
        0.0,
        0.0 )
    else (0.0, 0.0, 0.0, 0.0)
  in

  {
    count;
    total_verts;
    total_indices;
    water_count;
    water_verts;
    water_indices;
    poi_count;
    min_lon;
    min_lat;
    scale_x;
    scale_y;
    water_scale_x;
    water_scale_y;
    poi_scale_x;
    poi_scale_y;
    is_clc4;
    is_clc5;
  }

(* Get CLC tile name from lat/lon *)
let tile_name lat lon =
  let lat_int = int_of_float (floor lat) in
  let lon_int = int_of_float (floor lon) in
  let lat_str = if lat_int >= 0 then "N" else "S" in
  let lon_str = if lon_int >= 0 then "E" else "W" in
  Printf.sprintf "%s%02d%s%03d.clc" lat_str (abs lat_int) lon_str (abs lon_int)

(* Tile path for fetching *)
let tile_path lat lon = "data/clc/" ^ tile_name lat lon

(* Convert Lwt future to Lwt promise *)
let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
  t

(* WASM decoder integration *)
module Wasm = struct
  let instance = ref None
  let m_meta = ref None
  let m_hi_x = ref None
  let m_mid_x = ref None
  let m_lo_x = ref None
  let m_hi_y = ref None
  let m_mid_y = ref None
  let m_lo_y = ref None
  let m_hi_i = ref None
  let m_lo_i = ref None
  let m_out_pos = ref None
  let m_out_col = ref None
  let m_out_ebo = ref None
  let m_out_wpos = ref None
  let m_palette = ref None

  let create_memory pages =
    let props = Jv.obj [| ("initial", Jv.of_int pages) |] in
    Jv.new' (Jv.get (Jv.get Jv.global "WebAssembly") "Memory") [| props |]

  let load () =
    let open Lwt.Syntax in
    match !instance with
    | Some inst -> Lwt.return inst
    | None ->
        let* resp = to_lwt @@ Brr_io.Fetch.url (Jstr.v "decode_clc.wasm") in
        let* buf =
          to_lwt
            (Brr_io.Fetch.Body.array_buffer
               (Brr_io.Fetch.Response.as_body resp))
        in

        (* Metadata and streams: 16-128 pages each (1MB - 8MB) *)
        m_meta := Some (create_memory 16);
        m_hi_x := Some (create_memory 32);
        m_mid_x := Some (create_memory 32);
        m_lo_x := Some (create_memory 32);
        m_hi_y := Some (create_memory 32);
        m_mid_y := Some (create_memory 32);
        m_lo_y := Some (create_memory 32);
        m_hi_i := Some (create_memory 64);
        m_lo_i := Some (create_memory 64);
        m_palette := Some (create_memory 1);

        (* Output arrays: Up to 512 pages each (32MB) *)
        m_out_pos := Some (create_memory 512);
        m_out_col := Some (create_memory 128);
        (* Only 1 byte per vertex *)
        m_out_ebo := Some (create_memory 512);
        m_out_wpos := Some (create_memory 512);

        (* Prefill code mapping table in m_palette *)
        let palette_ba =
          Brr.Tarray.to_bigarray1
            (Brr.Tarray.of_buffer Brr.Tarray.Uint8
               (Brr.Tarray.Buffer.of_jv
                  (Jv.get (Option.get !m_palette) "buffer")))
        in
        Bigarray.Array1.fill palette_ba 0;
        Array.iteri
          (fun idx m ->
            if m.Clc_palette.code < 1024 then
              Bigarray.Array1.set palette_ba m.Clc_palette.code idx)
          Clc_palette.materials;

        let imports =
          Jv.obj
            [|
              ( "env",
                Jv.obj
                  [|
                    ("m_meta", Option.get !m_meta);
                    ("m_hi_x", Option.get !m_hi_x);
                    ("m_mid_x", Option.get !m_mid_x);
                    ("m_lo_x", Option.get !m_lo_x);
                    ("m_hi_y", Option.get !m_hi_y);
                    ("m_mid_y", Option.get !m_mid_y);
                    ("m_lo_y", Option.get !m_lo_y);
                    ("m_hi_i", Option.get !m_hi_i);
                    ("m_lo_i", Option.get !m_lo_i);
                    ("m_out_pos", Option.get !m_out_pos);
                    ("m_out_col", Option.get !m_out_col);
                    ("m_out_ebo", Option.get !m_out_ebo);
                    ("m_out_wpos", Option.get !m_out_wpos);
                    ("m_palette", Option.get !m_palette);
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
        instance := Some inst;
        Lwt.return inst

  let get_ta mem =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8
      (Brr.Tarray.Buffer.of_jv (Jv.get mem "buffer"))
end

let decode_mutex = Lwt_mutex.create ()

(* Read a compressed stream from data at offset, decompress into WASM memory *)
let read_stream_to_wasm data_ta offset wasm_mem =
  let open Lwt.Syntax in
  let data_ba = Brr.Tarray.to_bigarray1 data_ta in
  let read_i32_le ba i =
    let b0 = Array1.get ba i in
    let b1 = Array1.get ba (i + 1) in
    let b2 = Array1.get ba (i + 2) in
    let b3 = Array1.get ba (i + 3) in
    b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
  in
  let comp_len = read_i32_le data_ba offset in
  let compressed =
    Brr.Tarray.sub data_ta ~start:(offset + 4) ~stop:(offset + 4 + comp_len)
  in
  let target_ta = Wasm.get_ta wasm_mem in
  let* _ =
    to_lwt
      (Fut.of_promise
         ~ok:(fun x -> x)
         (Reader.inflate_into compressed target_ta))
  in
  Lwt.return (offset + 4 + comp_len)

let get_ba16 mem size =
  let buf = Jv.get mem "buffer" in
  let ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint16 (Brr.Tarray.Buffer.of_jv buf)
  in
  let ba = Brr.Tarray.to_bigarray1 ta in
  Bigarray.Array1.sub ba 0 size

let get_ba32 mem size =
  let buf = Jv.get mem "buffer" in
  let ta =
    Brr.Tarray.of_buffer Brr.Tarray.Int32 (Brr.Tarray.Buffer.of_jv buf)
  in
  let ba = Brr.Tarray.to_bigarray1 ta in
  Bigarray.Array1.sub ba 0 size

let get_ba8 mem size =
  let buf = Jv.get mem "buffer" in
  let ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv buf)
  in
  let ba = Brr.Tarray.to_bigarray1 ta in
  Bigarray.Array1.sub ba 0 size

(* Full CLC tile loading: decompress all streams and decode geometry *)
let load_full_clc_tile path_str =
  let open Lwt.Syntax in
  let* resp = to_lwt (Brr_io.Fetch.url (Jstr.v path_str)) in
  let* buf =
    to_lwt (Brr_io.Fetch.Body.array_buffer (Brr_io.Fetch.Response.as_body resp))
  in
  let data = Brr.Tarray.of_buffer Brr.Tarray.Uint8 buf in
  let data_ba = Brr.Tarray.to_bigarray1 data in
  let header = parse_header_ba data_ba in

  let offset =
    if header.is_clc5 then 96 else if header.is_clc4 then 76 else 48
  in

  Lwt_mutex.with_lock decode_mutex (fun () ->
      let* inst = Wasm.load () in

      (* Decompress CLC streams directly into WASM memory *)
      let* offset = read_stream_to_wasm data offset (Option.get !Wasm.m_meta) in
      let* offset = read_stream_to_wasm data offset (Option.get !Wasm.m_hi_x) in
      let* offset = read_stream_to_wasm data offset (Option.get !Wasm.m_lo_x) in
      let* offset = read_stream_to_wasm data offset (Option.get !Wasm.m_hi_y) in
      let* offset = read_stream_to_wasm data offset (Option.get !Wasm.m_lo_y) in
      let* offset = read_stream_to_wasm data offset (Option.get !Wasm.m_hi_i) in
      let* offset = read_stream_to_wasm data offset (Option.get !Wasm.m_lo_i) in

      (* Decode CLC layers *)
      let func = Jv.get (Jv.get inst "exports") "decode_clc" in
      ignore
        (Jv.apply func
           [|
             Jv.of_int header.count;
             Jv.of_int header.total_verts;
             Jv.of_int header.total_indices;
           |]);

      (* Copy results out of WASM memory *)
      let positions =
        Array1.create int16_unsigned c_layout (header.total_verts * 2)
      in
      Array1.blit
        (get_ba16 (Option.get !Wasm.m_out_pos) (header.total_verts * 2))
        positions;

      let colors = Array1.create int8_unsigned c_layout header.total_verts in
      Array1.blit
        (get_ba8 (Option.get !Wasm.m_out_col) header.total_verts)
        colors;

      let indices = Array1.create int32 c_layout header.total_indices in
      Array1.blit
        (get_ba32 (Option.get !Wasm.m_out_ebo) header.total_indices)
        indices;

      (* Water layers *)
      let* water_positions, water_colors, water_indices, pois =
        if (header.is_clc4 || header.is_clc5) && header.water_count > 0 then begin
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_meta)
          in
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_hi_x)
          in
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_mid_x)
          in
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_lo_x)
          in
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_hi_y)
          in
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_mid_y)
          in
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_lo_y)
          in
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_hi_i)
          in
          (* Fix offset threading *)
          let* offset =
            read_stream_to_wasm data offset (Option.get !Wasm.m_lo_i)
          in

          let func_w = Jv.get (Jv.get inst "exports") "decode_water" in
          ignore
            (Jv.apply func_w
               [|
                 Jv.of_int header.water_count;
                 Jv.of_int header.water_verts;
                 Jv.of_int header.water_indices;
               |]);

          let wp = Array1.create int32 c_layout (header.water_verts * 2) in
          Array1.blit
            (get_ba32 (Option.get !Wasm.m_out_wpos) (header.water_verts * 2))
            wp;

          let wc = Array1.create int8_unsigned c_layout header.water_verts in
          Array1.blit
            (get_ba8 (Option.get !Wasm.m_out_col) header.water_verts)
            wc;

          let we = Array1.create int32 c_layout header.water_indices in
          Array1.blit
            (get_ba32 (Option.get !Wasm.m_out_ebo) header.water_indices)
            we;

          (* Parse POIs from decompressed streams *)
          let parse_pois count (min_lon, min_lat) (scale_x, scale_y) =
            let ba_names = get_ba8 (Option.get !Wasm.m_meta) (count * 64) in
            let ba_coords = get_ba8 (Option.get !Wasm.m_hi_x) (count * 6) in
            let ba_elevs = get_ba16 (Option.get !Wasm.m_mid_x) count in
            let ba_types = get_ba8 (Option.get !Wasm.m_lo_x) count in

            let rec loop i name_sub_offset acc =
              if i >= count then List.rev acc
              else
                (* Parse Name *)
                let name_len = ba_names.{name_sub_offset} in
                let name =
                  String.init name_len (fun j ->
                      Char.chr ba_names.{name_sub_offset + 1 + j})
                in
                let next_name_offset = name_sub_offset + 1 + name_len in

                (* Parse Coords *)
                let base_c = i * 6 in
                let b0 = ba_coords.{base_c} in
                let b1 = ba_coords.{base_c + 1} in
                let b2 = ba_coords.{base_c + 2} in
                let qx = b0 lor (b1 lsl 8) lor (b2 lsl 16) in

                let b3 = ba_coords.{base_c + 3} in
                let b4 = ba_coords.{base_c + 4} in
                let b5 = ba_coords.{base_c + 5} in
                let qy = b3 lor (b4 lsl 8) lor (b5 lsl 16) in

                let lon = min_lon +. (float qx /. scale_x) in
                let lat = min_lat +. (float qy /. scale_y) in

                (* Parse Elevation *)
                let e_raw = ba_elevs.{i} in
                let elevation =
                  if e_raw >= 32768 then e_raw - 65536 else e_raw
                in

                (* Parse Type *)
                let t_raw = ba_types.{i} in
                let poi_type = if t_raw = 0 then Peak else Saddle in

                loop (i + 1) next_name_offset
                  ({ name; lat; lon; elevation; poi_type } :: acc)
            in
            loop 0 0 []
          in

          let* pois =
            if header.is_clc5 && header.poi_count > 0 then begin
              let* offset =
                read_stream_to_wasm data offset (Option.get !Wasm.m_meta)
              in
              let* offset =
                read_stream_to_wasm data offset (Option.get !Wasm.m_hi_x)
              in
              let* offset =
                read_stream_to_wasm data offset (Option.get !Wasm.m_mid_x)
              in
              let* _offset =
                read_stream_to_wasm data offset (Option.get !Wasm.m_lo_x)
              in

              Lwt.return
                (parse_pois header.poi_count
                   (header.min_lon, header.min_lat)
                   (header.poi_scale_x, header.poi_scale_y))
            end
            else Lwt.return []
          in

          Lwt.return (wp, wc, we, pois)
        end
        else
          Lwt.return
            ( Array1.create int32 c_layout 0,
              Array1.create int8_unsigned c_layout 0,
              Array1.create int32 c_layout 0,
              [] )
      in

      Lwt.return
        {
          header;
          positions;
          colors;
          indices;
          water_positions;
          water_colors;
          water_indices;
          pois;
        })

(* Load CLC tiles in parallel *)
let load_tiles ~lat ~lon ~size =
  let open Lwt.Syntax in
  let size_deg = float size /. 3600. in
  let dem_min_lat = lat -. (size_deg /. 2.) in
  let dem_max_lat = lat +. (size_deg /. 2.) in
  let dem_min_lon = lon -. (size_deg /. 2.) in
  let dem_max_lon = lon +. (size_deg /. 2.) in
  let dem_range_lon = dem_max_lon -. dem_min_lon in
  let dem_range_lat = dem_max_lat -. dem_min_lat in

  let min_tile_lat = int_of_float (floor dem_min_lat) in
  let max_tile_lat = int_of_float (floor dem_max_lat) in
  let min_tile_lon = int_of_float (floor dem_min_lon) in
  let max_tile_lon = int_of_float (floor dem_max_lon) in

  let tasks = ref [] in
  for tile_lat = min_tile_lat to max_tile_lat do
    for tile_lon = min_tile_lon to max_tile_lon do
      let path = tile_path (float tile_lat +. 0.5) (float tile_lon +. 0.5) in
      let task =
        Lwt.catch
          (fun () ->
            let* tile = load_full_clc_tile path in
            let tile_range_lon = 65535. /. tile.header.scale_x in
            let tile_range_lat = 65535. /. tile.header.scale_y in
            Lwt.return (Some (tile, tile_range_lon, tile_range_lat)))
          (fun _exn -> Lwt.return None)
      in
      tasks := task :: !tasks
    done
  done;

  let* results = Lwt.all !tasks in
  let tiles = List.filter_map (fun x -> x) results in

  Lwt.return (dem_min_lon, dem_min_lat, dem_range_lon, dem_range_lat, tiles)
