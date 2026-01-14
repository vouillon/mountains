module Triangulator : sig
  val triangulate_multi :
    ?tile:string ->
    ?feature_type:string ->
    float array ->
    Geometry_types.polygon array ->
    int array
  (** Triangulate multiple polygons sharing a vertex array. *)

  val polygon_area : float array -> Geometry_types.polygon -> float
  (** Compute the signed area of a polygon. *)

  val verbose : bool ref
  (** Enable verbose logging for debugging triangulation issues. *)

  val get_last_normalized_coords : unit -> float array
  val get_bridges : unit -> (int * int) list
  val clear_bridges : unit -> unit
  val get_stalled_loops : unit -> int list list
  val clear_stalled_loops : unit -> unit
end
