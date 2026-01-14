type clc_header = {
  count : int;  (** Number of polygons *)
  total_verts : int;  (** Total number of vertices *)
  total_indices : int;  (** Total number of indices *)
  water_count : int;  (** Number of water polygons (CLC4+ only) *)
  water_verts : int;  (** Total water vertices (CLC4+ only) *)
  water_indices : int;  (** Total water indices (CLC4+ only) *)
  poi_count : int;  (** Number of POIs (CLC5 only) *)
  min_lon : float;  (** Tile bounding box min longitude *)
  min_lat : float;  (** Tile bounding box min latitude *)
  scale_x : float;  (** Longitude scale factor *)
  scale_y : float;  (** Latitude scale factor *)
  water_scale_x : float;  (** Water longitude scale factor (CLC4+ only) *)
  water_scale_y : float;  (** Water latitude scale factor (CLC4+ only) *)
  poi_scale_x : float;  (** POI longitude scale factor (CLC5 only) *)
  poi_scale_y : float;  (** POI latitude scale factor (CLC5 only) *)
  is_clc4 : bool;  (** Whether the tile contains water data *)
  is_clc5 : bool;  (** Whether the tile contains POI data *)
}
(** CLC tile metadata *)

type poi_type = Peak | Saddle  (** Type of Point of Interest *)

type poi = {
  name : string;  (** Name of the POI *)
  lat : float;  (** Latitude *)
  lon : float;  (** Longitude *)
  elevation : int;  (** Elevation in meters *)
  poi_type : poi_type;  (** Peak or Saddle *)
}
(** Point of Interest data *)

open Bigarray

type clc_tile = {
  header : clc_header;
  positions : (int, int16_unsigned_elt, c_layout) Array1.t; (* x,y pairs U16 *)
  colors : (int, int8_unsigned_elt, c_layout) Array1.t; (* palette indices *)
  indices : (int32, int32_elt, c_layout) Array1.t;
  (* Water layer (CLC4+ only) *)
  water_positions : (int32, int32_elt, c_layout) Array1.t;
  water_colors : (int, int8_unsigned_elt, c_layout) Array1.t;
  water_indices : (int32, int32_elt, c_layout) Array1.t;
  (* POI data (CLC5 only) *)
  pois : poi list;
}
(** Decoded CLC tile data *)

val load_tiles :
  lat:float ->
  lon:float ->
  size:int ->
  (float * float * float * float * (clc_tile * float * float) list) Lwt.t
(** Load CLC tiles for the given area. Returns: (dem_min_lon, dem_min_lat,
    dem_range_lon, dem_range_lat, tiles list) Each tile in the list has:
    (tile_data, tile_range_lon, tile_range_lat) *)

val prefetch : size:int -> lat:float -> lon:float -> unit Lwt.t
(** Prefetch CLC tiles for the given range to warm browser cache. *)
