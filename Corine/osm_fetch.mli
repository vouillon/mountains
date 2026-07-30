(** Utility module for fetching water polygons from Overpass API *)

type point = { x : float; y : float }
type ring = point list
type polygon = ring list
type water_feature = { id : int; clc_code : int; polygons : polygon list }

val fetch_water_polygons :
  min_lat:float ->
  min_lon:float ->
  max_lat:float ->
  max_lon:float ->
  water_feature list
(** Fetch water polygons for the given bounding box from OpenStreetMap. *)

val response_error : string -> string option
(** [response_error body] returns a description of why an Overpass response is
    unusable (not JSON, malformed, or carrying a server "remark"), or [None] if
    it looks complete. Exposed for tests. *)

val parse_overpass_elements : string -> water_feature list
(** Parse an Overpass JSON response into water features. Exposed for tests. *)

val feature_to_flat_arrays :
  water_feature -> (int * float array * float array array) list
(** Convert water features to flat vertex arrays suitable for the tile pipeline.
*)
