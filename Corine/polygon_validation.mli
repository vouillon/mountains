(** Validation tool for polygons *)

type error =
  | DuplicatePoint of int * (float * float)
  | SelfIntersection of (int * int) * (int * int)
  | HoleNotContained of int

val validate_cheap : float array -> Geometry_types.polygon -> error list
val validate_expensive : float array -> Geometry_types.polygon -> error list
val check_self_intersection_indices : float array -> int array -> error list
val string_of_error : error -> string
