(** Robust Ear Clipping polygon triangulation *)

type point = { x : float; y : float }

val triangulate : point list list -> int list
(** Triangulate a polygon (with holes) into a list of vertex indices. Each
    triplet of indices forms a triangle. *)
