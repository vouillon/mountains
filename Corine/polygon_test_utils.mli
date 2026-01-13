(** Helper functions for polygon tests *)

val list_test_polygons : unit -> string list

val load_polygon_json :
  string -> string * string * float * float * float array * float array array

val save_polygon_json :
  tile:string ->
  feature_type:string ->
  expected_area:float ->
  actual_area:float ->
  outer:float array ->
  holes:float array array ->
  ?validation_errors:string list ->
  unit ->
  string
