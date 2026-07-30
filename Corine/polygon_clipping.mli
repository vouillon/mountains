(** Sutherland-Hodgman polygon clipping against a rectangle *)

open Geometry_types

module Clipper : sig
  val clip_polygon :
    float array -> polygon -> rect -> (float array * polygon) option
  (** Clip a single polygon against a rectangle. Returns the new vertex array
      and the clipped polygon structure, or [None] if the outer ring clips to
      fewer than 3 vertices. Holes that clip to fewer than 3 vertices are
      dropped. Output rings may contain duplicate vertices on the rectangle
      boundary or have zero area; consumers must tolerate both (the triangulator
      does). *)

  val clip_multipolygon :
    float array -> polygon array -> rect -> float array * polygon array
  (** Clip multiple polygons against a rectangle. Resulting vertices are
      aggregated into a single new vertex array. Polygons whose outer ring clips
      to fewer than 3 vertices are omitted; see {!clip_polygon} for the other
      output caveats. *)
end
