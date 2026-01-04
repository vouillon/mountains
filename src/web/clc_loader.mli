type clc_header = {
  count : int;
  total_verts : int;
  total_indices : int;
  water_count : int; (* CLC4 only *)
  water_verts : int; (* CLC4 only *)
  water_indices : int; (* CLC4 only *)
  min_lon : float;
  min_lat : float;
  scale_x : float;
  scale_y : float;
  water_scale_x : float; (* CLC4 only *)
  water_scale_y : float; (* CLC4 only *)
  is_clc4 : bool;
}

open Bigarray

type clc_tile = {
  header : clc_header;
  positions : (int, int16_unsigned_elt, c_layout) Array1.t; (* x,y pairs U16 *)
  colors : (int, int8_unsigned_elt, c_layout) Array1.t; (* palette indices *)
  indices : (int32, int32_elt, c_layout) Array1.t;
  (* Water layer (CLC4 only) - stored as U16 like CLC after scaling *)
  water_positions : (int, int16_unsigned_elt, c_layout) Array1.t;
  water_colors : (int, int8_unsigned_elt, c_layout) Array1.t;
  water_indices : (int32, int32_elt, c_layout) Array1.t;
}

val load_tiles :
  lat:float ->
  lon:float ->
  size:int ->
  (float * float * float * float * (clc_tile * float * float) list) Lwt.t
(** Load CLC tiles. Returns: (dem_min_lon, dem_min_lat, dem_range_lon,
    dem_range_lat, tiles list) Each tile in the list has: (tile_data,
    tile_range_lon, tile_range_lat) *)
