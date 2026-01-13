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

val feature_to_flat_arrays :
  water_feature -> (int * float array * float array array) list
(** Convert water features to flat vertex arrays suitable for the tile pipeline.
*)
