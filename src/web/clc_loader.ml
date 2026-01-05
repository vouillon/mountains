(* clc_loader.ml - Load and rasterize CLC tiles for web viewer *)

open Bigarray

let ( >>= ) = Lwt.bind

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
  (* Water layer (CLC4+ only) - stored as U16 like CLC after scaling *)
  water_positions : (int, int16_unsigned_elt, c_layout) Array1.t;
  water_colors : (int, int8_unsigned_elt, c_layout) Array1.t;
  water_indices : (int32, int32_elt, c_layout) Array1.t;
  (* POI data (CLC5 only) *)
  pois : poi list;
}

(* Read big-endian 32-bit int from string at offset (OCaml's input_binary_int format) *)
let read_i32_be s offset =
  let b0 = Char.code s.[offset] in
  let b1 = Char.code s.[offset + 1] in
  let b2 = Char.code s.[offset + 2] in
  let b3 = Char.code s.[offset + 3] in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

(* Read little-endian 32-bit int from string at offset (for stream lengths) *)
let read_i32_le s offset =
  let b0 = Char.code s.[offset] in
  let b1 = Char.code s.[offset + 1] in
  let b2 = Char.code s.[offset + 2] in
  let b3 = Char.code s.[offset + 3] in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

(* Read little-endian 64-bit float from string *)
let read_f64_le s offset =
  let b0 = Int64.of_int (Char.code s.[offset]) in
  let b1 = Int64.of_int (Char.code s.[offset + 1]) in
  let b2 = Int64.of_int (Char.code s.[offset + 2]) in
  let b3 = Int64.of_int (Char.code s.[offset + 3]) in
  let b4 = Int64.of_int (Char.code s.[offset + 4]) in
  let b5 = Int64.of_int (Char.code s.[offset + 5]) in
  let b6 = Int64.of_int (Char.code s.[offset + 6]) in
  let b7 = Int64.of_int (Char.code s.[offset + 7]) in
  let bits =
    Int64.logor b0
      (Int64.logor (Int64.shift_left b1 8)
         (Int64.logor (Int64.shift_left b2 16)
            (Int64.logor (Int64.shift_left b3 24)
               (Int64.logor (Int64.shift_left b4 32)
                  (Int64.logor (Int64.shift_left b5 40)
                     (Int64.logor (Int64.shift_left b6 48)
                        (Int64.shift_left b7 56)))))))
  in
  Int64.float_of_bits bits

(* ZigZag decode - for delta-encoded coordinates *)
let zigzag_decode n = (n lsr 1) lxor -(n land 1)

(* Parse CLC file header - supports CLC3, CLC4, and CLC5 formats *)
let parse_header s =
  let magic = String.sub s 0 4 in
  let is_clc4 = magic = "CLC4" in
  let is_clc5 = magic = "CLC5" in
  if magic <> "CLC3" && magic <> "CLC4" && magic <> "CLC5" then
    failwith ("Invalid CLC magic: " ^ magic);

  let count = read_i32_be s 4 in
  let total_verts = read_i32_be s 8 in
  let total_indices = read_i32_be s 12 in

  (* CLC4/5 has additional water counts, CLC5 also has POI count *)
  let water_count, water_verts, water_indices, poi_count, float_offset =
    if is_clc5 then
      ( read_i32_be s 16,
        read_i32_be s 20,
        read_i32_be s 24,
        read_i32_be s 28,
        32 )
    else if is_clc4 then
      (read_i32_be s 16, read_i32_be s 20, read_i32_be s 24, 0, 28)
    else (0, 0, 0, 0, 16)
  in

  let min_lon = read_f64_le s float_offset in
  let min_lat = read_f64_le s (float_offset + 8) in
  let scale_x = read_f64_le s (float_offset + 16) in
  let scale_y = read_f64_le s (float_offset + 24) in

  (* CLC4/5 has water scales, CLC5 also has POI scales *)
  let water_scale_x, water_scale_y, poi_scale_x, poi_scale_y =
    if is_clc5 then
      ( read_f64_le s (float_offset + 32),
        read_f64_le s (float_offset + 40),
        read_f64_le s (float_offset + 48),
        read_f64_le s (float_offset + 56) )
    else if is_clc4 then
      ( read_f64_le s (float_offset + 32),
        read_f64_le s (float_offset + 40),
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

(* Read a compressed stream from data at offset, return (decompressed, new_offset) *)
let read_stream data offset =
  let open Lwt.Syntax in
  let comp_len = read_i32_le data offset in
  let compressed = String.sub data (offset + 4) comp_len in
  (* Convert string to uint8 array for pako *)
  let tarray = Reader.uint8_of_string compressed in
  (* Decompress and return string directly *)
  let* decompressed = Reader.inflate_to_string tarray in
  Lwt.return (decompressed, offset + 4 + comp_len)

(* Decode CLC vertices and triangles from streams *)
let decode_clc_streams header meta_str high_x low_x high_y low_y high_indices
    low_indices =
  let n_verts = header.total_verts in
  let n_indices = header.total_indices in
  let arr_pos = Array1.create int16_unsigned c_layout (n_verts * 2) in
  let arr_col = Array1.create int8_unsigned c_layout n_verts in
  let arr_ebo = Array1.create int32 c_layout n_indices in

  let meta_pos = ref 0 in
  let v_pos = ref 0 in
  let i_pos = ref 0 in
  let global_v_offset = ref 0 in
  let global_i_offset = ref 0 in

  let read_u16 str pos =
    let b0 = Char.code str.[!pos] in
    let b1 = Char.code str.[!pos + 1] in
    pos := !pos + 2;
    b0 lor (b1 lsl 8)
  in

  for _ = 1 to header.count do
    let code = read_u16 meta_str meta_pos in
    let v_count = read_u16 meta_str meta_pos in
    let t_count = read_u16 meta_str meta_pos in
    let code_idx = Clc_palette.get_index code in
    let base_v = !global_v_offset in

    (* Decode vertices *)
    let prev_x = ref 0 in
    let prev_y = ref 0 in
    for k = 0 to v_count - 1 do
      let idx = !v_pos + k in
      let hx = Char.code high_x.[idx] in
      let lx = Char.code low_x.[idx] in
      let zx = lx lor (hx lsl 8) in
      let sdx = zigzag_decode zx in
      let qx = (!prev_x + sdx) land 0xFFFF in
      prev_x := qx;

      let hy = Char.code high_y.[idx] in
      let ly = Char.code low_y.[idx] in
      let zy = ly lor (hy lsl 8) in
      let sdy = zigzag_decode zy in
      let qy = (!prev_y + sdy) land 0xFFFF in
      prev_y := qy;

      let out_idx = base_v + k in
      Array1.set arr_pos (out_idx * 2) qx;
      Array1.set arr_pos ((out_idx * 2) + 1) qy;
      Array1.set arr_col out_idx code_idx
    done;
    v_pos := !v_pos + v_count;
    global_v_offset := !global_v_offset + v_count;

    (* Decode indices *)
    let prev_idx = ref 0 in
    let num_indices = t_count * 3 in
    for k = 0 to num_indices - 1 do
      let idx = !i_pos + k in
      let hi = Char.code high_indices.[idx] in
      let li = Char.code low_indices.[idx] in
      let zi = li lor (hi lsl 8) in
      let sdi = zigzag_decode zi in
      let qi = (!prev_idx + sdi) land 0xFFFF in
      prev_idx := qi;
      Array1.set arr_ebo (!global_i_offset + k) (Int32.of_int (base_v + qi))
    done;
    i_pos := !i_pos + num_indices;
    global_i_offset := !global_i_offset + num_indices
  done;
  (arr_pos, arr_col, arr_ebo)

(* Decode water vertices and triangles from CLC4 streams (3-byte coords) *)
let decode_water_streams header meta_str high_x mid_x low_x high_y mid_y low_y
    high_indices low_indices =
  let n_verts = header.water_verts in
  let n_indices = header.water_indices in
  let arr_pos = Array1.create int16_unsigned c_layout (n_verts * 2) in
  let arr_col = Array1.create int8_unsigned c_layout n_verts in
  let arr_ebo = Array1.create int32 c_layout n_indices in

  if n_verts = 0 then (arr_pos, arr_col, arr_ebo)
  else begin
    let meta_pos = ref 0 in
    let v_pos = ref 0 in
    let i_pos = ref 0 in
    let global_v_offset = ref 0 in
    let global_i_offset = ref 0 in

    let read_u16 str pos =
      let b0 = Char.code str.[!pos] in
      let b1 = Char.code str.[!pos + 1] in
      pos := !pos + 2;
      b0 lor (b1 lsl 8)
    in

    for _ = 1 to header.water_count do
      let code = read_u16 meta_str meta_pos in
      let v_count = read_u16 meta_str meta_pos in
      let t_count = read_u16 meta_str meta_pos in
      let code_idx = Clc_palette.get_index code in
      let base_v = !global_v_offset in

      (* Decode 3-byte coordinates and scale to 16-bit *)
      let prev_x = ref 0 in
      let prev_y = ref 0 in
      for k = 0 to v_count - 1 do
        let idx = !v_pos + k in
        let hx = Char.code high_x.[idx] in
        let mx = Char.code mid_x.[idx] in
        let lx = Char.code low_x.[idx] in
        let zx = lx lor (mx lsl 8) lor (hx lsl 16) in
        let sdx = zigzag_decode zx in
        let qx = (!prev_x + sdx) land 0xFFFFFF in
        prev_x := qx;

        let hy = Char.code high_y.[idx] in
        let my = Char.code mid_y.[idx] in
        let ly = Char.code low_y.[idx] in
        let zy = ly lor (my lsl 8) lor (hy lsl 16) in
        let sdy = zigzag_decode zy in
        let qy = (!prev_y + sdy) land 0xFFFFFF in
        prev_y := qy;

        (* Scale from water range (0-220000) to u16 range (0-65535) 
           Use float to avoid overflow on 32-bit JS integers *)
        let scaled_x = int_of_float (float_of_int qx *. 65535. /. 220000.) in
        let scaled_y = int_of_float (float_of_int qy *. 65535. /. 220000.) in

        let out_idx = base_v + k in
        Array1.set arr_pos (out_idx * 2) scaled_x;
        Array1.set arr_pos ((out_idx * 2) + 1) scaled_y;
        Array1.set arr_col out_idx code_idx
      done;
      v_pos := !v_pos + v_count;
      global_v_offset := !global_v_offset + v_count;

      (* Decode indices (same as CLC - 16-bit) *)
      let prev_idx = ref 0 in
      let num_indices = t_count * 3 in
      for k = 0 to num_indices - 1 do
        let idx = !i_pos + k in
        let hi = Char.code high_indices.[idx] in
        let li = Char.code low_indices.[idx] in
        let zi = li lor (hi lsl 8) in
        let sdi = zigzag_decode zi in
        let qi = (!prev_idx + sdi) land 0xFFFF in
        prev_idx := qi;
        Array1.set arr_ebo (!global_i_offset + k) (Int32.of_int (base_v + qi))
      done;
      i_pos := !i_pos + num_indices;
      global_i_offset := !global_i_offset + num_indices
    done;
    (arr_pos, arr_col, arr_ebo)
  end

(* Full CLC tile loading: decompress all streams and decode geometry *)
let load_full_clc_tile path =
  let open Lwt.Syntax in
  let* data = Reader.read_file path in
  let header = parse_header data in
  (* Header size varies by format: CLC3=48, CLC4=76, CLC5=92 *)
  let offset =
    if header.is_clc5 then 96 else if header.is_clc4 then 76 else 48
  in

  (* Read CLC streams (7 for CLC3, same for CLC4/5) *)
  let* meta_str, offset = read_stream data offset in
  let* high_x, offset = read_stream data offset in
  let* low_x, offset = read_stream data offset in
  let* high_y, offset = read_stream data offset in
  let* low_y, offset = read_stream data offset in
  let* high_indices, offset = read_stream data offset in
  let* low_indices, offset = read_stream data offset in

  (* Decode CLC geometry *)
  let clc_pos, clc_col, clc_ebo =
    decode_clc_streams header meta_str high_x low_x high_y low_y high_indices
      low_indices
  in

  (* Read and decode water streams if CLC4/5 *)
  let* water_pos, water_col, water_ebo, offset =
    if (header.is_clc4 || header.is_clc5) && header.water_count > 0 then begin
      let* w_meta, offset = read_stream data offset in
      let* w_high_x, offset = read_stream data offset in
      let* w_mid_x, offset = read_stream data offset in
      let* w_low_x, offset = read_stream data offset in
      let* w_high_y, offset = read_stream data offset in
      let* w_mid_y, offset = read_stream data offset in
      let* w_low_y, offset = read_stream data offset in
      let* w_high_idx, offset = read_stream data offset in
      let* w_low_idx, offset = read_stream data offset in
      let wp, wc, we =
        decode_water_streams header w_meta w_high_x w_mid_x w_low_x w_high_y
          w_mid_y w_low_y w_high_idx w_low_idx
      in
      Lwt.return (wp, wc, we, offset)
    end
    else begin
      (* Empty water arrays *)
      Lwt.return
        ( Array1.create int16_unsigned c_layout 0,
          Array1.create int8_unsigned c_layout 0,
          Array1.create int32 c_layout 0,
          offset )
    end
  in

  (* Read and decode POI streams if CLC5 *)
  let* pois =
    if header.is_clc5 && header.poi_count > 0 then begin
      let* names_str, offset = read_stream data offset in
      let* coords_str, offset = read_stream data offset in
      let* elevs_str, offset = read_stream data offset in
      let* types_str, _ = read_stream data offset in

      (* Decode POIs *)
      let pois = ref [] in
      let names_pos = ref 0 in
      let coords_pos = ref 0 in
      let elevs_pos = ref 0 in
      let types_pos = ref 0 in

      for _ = 1 to header.poi_count do
        (* Read name (length-prefixed) *)
        let name_len = Char.code names_str.[!names_pos] in
        incr names_pos;
        let name = String.sub names_str !names_pos name_len in
        names_pos := !names_pos + name_len;

        (* Read 3-byte coords (not delta-encoded for POIs, just raw quantized) *)
        let lx = Char.code coords_str.[!coords_pos] in
        let mx = Char.code coords_str.[!coords_pos + 1] in
        let hx = Char.code coords_str.[!coords_pos + 2] in
        let qx = lx lor (mx lsl 8) lor (hx lsl 16) in
        coords_pos := !coords_pos + 3;

        let ly = Char.code coords_str.[!coords_pos] in
        let my = Char.code coords_str.[!coords_pos + 1] in
        let hy = Char.code coords_str.[!coords_pos + 2] in
        let qy = ly lor (my lsl 8) lor (hy lsl 16) in
        coords_pos := !coords_pos + 3;

        (* Convert back to degrees *)
        let lon = header.min_lon +. (float qx /. header.poi_scale_x) in
        let lat = header.min_lat +. (float qy /. header.poi_scale_y) in

        (* Read signed 16-bit elevation *)
        let el = Char.code elevs_str.[!elevs_pos] in
        let eh = Char.code elevs_str.[!elevs_pos + 1] in
        let elev = el lor (eh lsl 8) in
        let elev = if elev >= 0x8000 then elev - 0x10000 else elev in
        elevs_pos := !elevs_pos + 2;

        (* Read type *)
        let typ = Char.code types_str.[!types_pos] in
        incr types_pos;
        let poi_type = if typ = 0 then Peak else Saddle in

        pois := { name; lat; lon; elevation = elev; poi_type } :: !pois
      done;
      Lwt.return (List.rev !pois)
    end
    else Lwt.return []
  in

  Lwt.return
    {
      header;
      positions = clc_pos;
      colors = clc_col;
      indices = clc_ebo;
      water_positions = water_pos;
      water_colors = water_col;
      water_indices = water_ebo;
      pois;
    }

(* Load CLC tiles.
   Returns: (dem_min_lon, dem_min_lat, dem_range_lon, dem_range_lat, tiles list)
   Each tile in the list has: (tile_data, tile_range_lon, tile_range_lat) *)
let load_tiles ~lat ~lon ~size =
  let open Lwt.Syntax in
  (* Calculate DEM geographic bounds in degrees *)
  let size_deg = float size /. 3600. in
  let dem_min_lat = lat -. (size_deg /. 2.) in
  let dem_max_lat = lat +. (size_deg /. 2.) in
  let dem_min_lon = lon -. (size_deg /. 2.) in
  let dem_max_lon = lon +. (size_deg /. 2.) in
  let dem_range_lon = dem_max_lon -. dem_min_lon in
  let dem_range_lat = dem_max_lat -. dem_min_lat in

  (* Determine which CLC tiles contribute *)
  (* Tiles are named by lower-left corner: N45 covers [45,46), so use floor for all *)
  let min_tile_lat = int_of_float (floor dem_min_lat) in
  let max_tile_lat = int_of_float (floor dem_max_lat) in
  let min_tile_lon = int_of_float (floor dem_min_lon) in
  let max_tile_lon = int_of_float (floor dem_max_lon) in

  (* Accumulate loaded tiles *)
  let tiles = ref [] in

  let rec load_tiles tile_lat tile_lon =
    if tile_lat > max_tile_lat then Lwt.return ()
    else if tile_lon > max_tile_lon then load_tiles (tile_lat + 1) min_tile_lon
    else begin
      let path = tile_path (float tile_lat +. 0.5) (float tile_lon +. 0.5) in
      Lwt.catch
        (fun () ->
          let* tile = load_full_clc_tile path in
          (* Calculate tile range in degrees (for shader uniforms) *)
          let tile_range_lon = 65535. /. tile.header.scale_x in
          let tile_range_lat = 65535. /. tile.header.scale_y in
          tiles := (tile, tile_range_lon, tile_range_lat) :: !tiles;
          Lwt.return ())
        (fun _exn -> Lwt.return ())
      >>= fun () -> load_tiles tile_lat (tile_lon + 1)
    end
  in
  let* () = load_tiles min_tile_lat min_tile_lon in

  (* Reverse to get correct draw order (smaller features first) *)
  Lwt.return
    (dem_min_lon, dem_min_lat, dem_range_lon, dem_range_lat, List.rev !tiles)
