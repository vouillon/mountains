(** Validation tool for polygons *)

(** Validation tool for polygons *)

type error =
  | DuplicatePoint of int * (float * float)
  | DegenerateEdge of int * (float * float)
  | WrongWindingOrder of bool
  | SelfIntersection of (int * int) * (int * int)
  | HoleNotContained of int
  | HolesIntersect of int * int

val validate_cheap : float array -> Geometry_types.polygon -> error list
val validate_expensive : float array -> Geometry_types.polygon -> error list
val check_self_intersection_indices : float array -> int array -> error list
val is_self_intersecting : float array -> int array -> bool
val is_hole_contained : float array -> int array -> int array -> bool
val string_of_error : error -> string
