open Bigarray

type t = { data : (int, int8_unsigned_elt, c_layout) Array2.t; size : int }
(** DEM tile data with heightmap and size *)

val get_height : t -> int -> int -> float
(** Get height at (row, col) with bounds checking *)

val get_texture_data : t -> (int, int8_unsigned_elt, c_layout) Array2.t
(** Get raw heightmap data for texture upload *)

val load : lat:float -> lon:float -> size:int -> t Lwt.t
(** Load a DEM tile of the given size at the specified latitude/longitude. Uses
    Web Workers for decompression. *)

val prefetch : lat:float -> lon:float -> size:int -> unit Lwt.t
(** Proactively fetch DEM tiles into browser cache. *)

val in_range :
  size:int ->
  min_lat:int ->
  max_lat:int ->
  min_lon:int ->
  max_lon:int ->
  lat:float ->
  lon:float ->
  bool
(** Check if a tile at (lat, lon) is within the specified bounding box. *)
