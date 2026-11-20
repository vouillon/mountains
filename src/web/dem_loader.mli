open Bigarray

type t = { data : (int, int8_unsigned_elt, c_layout) Array2.t; size : int }
(** DEM tile data with heightmap and size *)

val get_height : t -> int -> int -> float
(** Get height at (row, col) with bounds checking *)

val get_texture_data : t -> (int, int8_unsigned_elt, c_layout) Array2.t
(** Get raw heightmap data for texture upload *)

val load :
  ?step:int ->
  ?cover:bool ->
  lat:float ->
  lon:float ->
  size:int ->
  unit ->
  t Lwt.t
(** Load a DEM tile of [size] samples a side around the given latitude and
    longitude, on the grid of [step] arcseconds: 1 by default, the base tiles; 4
    for the far field's, whose column [j] is centred
    [4 (lon_arcsec / 4 - size / 2 + j) + 1.5] arcseconds east and row [i]
    [4 ((lat_arcsec - 1) / 4 + 1 - size / 2 + i) - 1.5] north, the divisions
    rounding down. With [cover] the tiles are the far field's land cover, on the
    same samples: each is a palette index in its low byte, and {!get_height}
    means nothing. Uses Web Workers for decompression. *)

val load_progressive :
  lat:float ->
  lon:float ->
  size:int ->
  half_rows:int ->
  half_cols:int ->
  (t * ((int * int * int * int) * (unit -> unit Lwt.t)) option) Lwt.t
(** [load_progressive ~lat ~lon ~size ~half_rows ~half_cols] is {!load} of the
    base tiles, but when some of the tiles are not in the cache, only those
    under the [2 half_rows + 1] by [2 half_cols + 1] samples about the centre
    are fetched before it returns: with [Some (valid, rest)], where [valid] is
    the rectangle of samples they filled -- first row, first column, last row,
    last column -- and [rest ()] fetches the others into the same heightmap. The
    rest of it is at sea level until then. [None] when every tile is in. *)

val prefetch :
  ?step:int ->
  ?cover:bool ->
  lat:float ->
  lon:float ->
  size:int ->
  unit ->
  unit Lwt.t
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
