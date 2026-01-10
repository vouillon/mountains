module Triangulator : sig
  val triangulate_multi :
    ?tile:string ->
    ?feature_type:string ->
    float array ->
    Geometry_types.polygon array ->
    int array

  val polygon_area : float array -> Geometry_types.polygon -> float
  val verbose : bool ref
  val get_last_normalized_coords : unit -> float array
  val get_bridges : unit -> (int * int) list
  val clear_bridges : unit -> unit
  val get_stalled_loops : unit -> int list list
  val clear_stalled_loops : unit -> unit
end
