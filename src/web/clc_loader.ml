(* clc_loader.ml - Load and rasterize CLC tiles for web viewer *)

open Bigarray
open Brr

(* CLC tile header info *)
type clc_header = {
  count : int;
  total_verts : int;
  total_indices : int;
  min_lon : float;
  min_lat : float;
  scale_x : float;
  scale_y : float;
}

(* Parsed CLC tile data ready for rasterization *)
type clc_tile = {
  header : clc_header;
  positions : (int, int16_unsigned_elt, c_layout) Array1.t; (* x,y pairs U16 *)
  colors : (int, int8_unsigned_elt, c_layout) Array1.t; (* palette indices *)
  indices : (int32, int32_elt, c_layout) Array1.t;
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

(* Parse CLC file header (48 bytes: 4 magic + 12 ints + 32 floats) *)
let parse_header s =
  let magic = String.sub s 0 4 in
  if magic <> "CLC3" then failwith ("Invalid CLC magic: " ^ magic);
  let count = read_i32_be s 4 in
  let total_verts = read_i32_be s 8 in
  let total_indices = read_i32_be s 12 in
  let min_lon = read_f64_le s 16 in
  let min_lat = read_f64_le s 24 in
  let scale_x = read_f64_le s 32 in
  let scale_y = read_f64_le s 40 in
  { count; total_verts; total_indices; min_lon; min_lat; scale_x; scale_y }

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
  (* Estimate uncompressed size - will be resized by inflate *)
  let est_size = max (comp_len * 10) 65536 in
  let buf = Bytes.create est_size in
  let tarray = Tarray.of_jstr (Jstr.of_string compressed) in
  let* () = Reader.inflate tarray buf in
  Lwt.return (Bytes.to_string buf, offset + 4 + comp_len)

(* Decode vertices and triangles from CLC streams *)
let decode_clc_data header meta_str high_x low_x high_y low_y high_indices
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

  let read_u16_meta () =
    let b0 = Char.code meta_str.[!meta_pos] in
    let b1 = Char.code meta_str.[!meta_pos + 1] in
    meta_pos := !meta_pos + 2;
    b0 lor (b1 lsl 8)
  in

  for _ = 1 to header.count do
    let code = read_u16_meta () in
    let v_count = read_u16_meta () in
    let t_count = read_u16_meta () in
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
  { header; positions = arr_pos; colors = arr_col; indices = arr_ebo }

(* Load and parse CLC tile from file path *)
let load_clc_tile path =
  let open Lwt.Syntax in
  let* data = Reader.read_file path in
  let header = parse_header data in
  Console.(
    log
      [
        Jstr.v ("Loaded CLC: " ^ path ^ " with ");
        Jstr.v (string_of_int header.count);
        Jstr.v " polygons";
      ]);
  Lwt.return header

(* Edge function for triangle rasterization *)
let edge_function x1 y1 x2 y2 px py =
  ((px - x1) * (y2 - y1)) - ((py - y1) * (x2 - x1))

(* Rasterize a single triangle to the texture *)
let rasterize_triangle data size x0 y0 x1 y1 x2 y2 color_idx =
  (* Convert normalized U16 coords (0-65535) to texture coords (0-size) *)
  let tx0 = x0 * size / 65536 in
  let ty0 = y0 * size / 65536 in
  let tx1 = x1 * size / 65536 in
  let ty1 = y1 * size / 65536 in
  let tx2 = x2 * size / 65536 in
  let ty2 = y2 * size / 65536 in

  (* Bounding box *)
  let min_x = max 0 (min tx0 (min tx1 tx2)) in
  let max_x = min (size - 1) (max tx0 (max tx1 tx2)) in
  let min_y = max 0 (min ty0 (min ty1 ty2)) in
  let max_y = min (size - 1) (max ty0 (max ty1 ty2)) in

  (* Rasterize using edge functions *)
  for py = min_y to max_y do
    for px = min_x to max_x do
      let w0 = edge_function tx1 ty1 tx2 ty2 px py in
      let w1 = edge_function tx2 ty2 tx0 ty0 px py in
      let w2 = edge_function tx0 ty0 tx1 ty1 px py in
      (* Check if point is inside triangle (all same sign) *)
      if (w0 >= 0 && w1 >= 0 && w2 >= 0) || (w0 <= 0 && w1 <= 0 && w2 <= 0) then
        Array1.set data ((py * size) + px) color_idx
    done
  done

(* Rasterize CLC tile to a texture of given size using CPU *)
let rasterize_clc_tile tile size =
  let data = Array1.create int8_unsigned c_layout (size * size) in
  (* Fill with default (fallback color) *)
  Array1.fill data 0;

  let positions = tile.positions in
  let colors = tile.colors in
  let indices = tile.indices in
  let n_triangles = Array1.dim indices / 3 in

  for t = 0 to n_triangles - 1 do
    let i0 = Int32.to_int (Array1.get indices (t * 3)) in
    let i1 = Int32.to_int (Array1.get indices ((t * 3) + 1)) in
    let i2 = Int32.to_int (Array1.get indices ((t * 3) + 2)) in

    let x0 = Array1.get positions (i0 * 2) in
    let y0 = Array1.get positions ((i0 * 2) + 1) in
    let x1 = Array1.get positions (i1 * 2) in
    let y1 = Array1.get positions ((i1 * 2) + 1) in
    let x2 = Array1.get positions (i2 * 2) in
    let y2 = Array1.get positions ((i2 * 2) + 1) in

    (* Use color from first vertex *)
    let color_idx = Array1.get colors i0 in

    rasterize_triangle data size x0 y0 x1 y1 x2 y2 color_idx
  done;
  data

(* Placeholder for non-decoded tiles *)
let rasterize_to_texture _ctx _header size =
  let data = Array1.create int8_unsigned c_layout (size * size) in
  let grass_idx = Clc_palette.get_index 321 in
  for i = 0 to (size * size) - 1 do
    data.{i} <- grass_idx
  done;
  data
