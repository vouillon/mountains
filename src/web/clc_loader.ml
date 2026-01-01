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

(* Read little-endian 32-bit int from string at offset *)
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

[@@@warning "-32"]

let _ = zigzag_decode

(* Parse CLC file header *)
let parse_header s =
  let magic = String.sub s 0 4 in
  if magic <> "CLC3" then failwith ("Invalid CLC magic: " ^ magic);
  let count = read_i32_le s 4 in
  let total_verts = read_i32_le s 8 in
  let total_indices = read_i32_le s 12 in
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

(* Load and parse CLC tile from file path *)
let load_clc_tile path =
  let open Lwt.Syntax in
  let* data = Reader.read_file path in
  let header = parse_header data in
  Console.(
    log
      [
        str ("Loaded CLC: " ^ path ^ " with ");
        str (string_of_int header.count);
        str " polygons";
      ]);
  Lwt.return header

(* Rasterize CLC tile to a texture of given size *)
(* For now, returns a simple solid grassland texture as placeholder *)
let rasterize_to_texture _ctx _header size =
  let data = Array1.create int8_unsigned c_layout (size * size) in
  (* Fill with natural grassland ID (index 26 = code 321) for now *)
  let grass_idx = Clc_palette.get_index 321 in
  for i = 0 to (size * size) - 1 do
    data.{i} <- grass_idx
  done;
  data
