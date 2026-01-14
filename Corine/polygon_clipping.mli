(** Sutherland-Hodgman polygon clipping against a rectangle *)

open Geometry_types

module Clipper : sig
  val clip_polygon :
    float array -> polygon -> rect -> (float array * polygon) option
  (** Clip a single polygon against a rectangle. Returns the new vertex array
      and the clipped polygon structure. *)

  val clip_multipolygon :
    float array -> polygon array -> rect -> float array * polygon array
  (** Clip multiple polygons against a rectangle. Resulting vertices are
      aggregated into a single new vertex array. *)
end
