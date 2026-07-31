val test :
  (int -> int -> float) ->
  ?src_h:float ->
  src_x:int ->
  src_y:int ->
  dst_x:int ->
  dst_y:int ->
  unit ->
  bool
(** Test visibility between two points by whole-pixel sampling along the line.
    [src_h] is used as given, with no eye-height clamp ([test_precise] delegates
    mid-ray with exact ray heights); it defaults to the source terrain + 2 m.
    Terrain within min(10 pixels, 6% of the distance) of the destination does
    not occlude, so a summit sticks out of its own massif.
    @param get_height function to get height at (row, col)
    @param src_h source elevation
    @param src_x source column
    @param src_y source row
    @param dst_x destination column
    @param dst_y destination row
    @return true if destination is visible from source *)

val curvature_drop : float -> float
(** [curvature_drop d2] is the drop of the Earth's surface below the observer's
    tangent plane, [d2] square metres from the observer, with standard
    atmospheric refraction folded in (effective radius ~7320 km). *)

val bilinear_height : (int -> int -> float) -> x:float -> y:float -> float
(** Bilinear interpolation of a height grid at fractional coordinates. Reads the
    (x+1, y+1) neighbours, so the caller must keep them inside the grid. *)

val test_precise :
  (int -> int -> float) ->
  ?src_h:float ->
  ?curvature:float * float ->
  ?fine:(float -> float -> float option) ->
  off_x:float ->
  off_y:float ->
  src_x:int ->
  src_y:int ->
  dst_x:int ->
  dst_y:int ->
  unit ->
  bool
(** Precise visibility test. Tests visibility from (src_x + off_x, src_y +
    off_y) to (dst_x, dst_y). Walks the ray in 0.02-pixel steps (~0.6 m) with
    bilinear interpolation for the first 6 pixels (~185 m), then delegates to
    [test]. The source's 1-pixel neighbourhood must lie inside the height grid.
    @param get_height function to get height at (row, col)
    @param src_h
      optional override for source elevation (defaults to terrain + 2m)
    @param curvature
      metres per pixel (x, y): when given, heights are evaluated in the
      observer-anchored frame lowered by the Earth-curvature drop, in which
      sight lines are straight (see {!curvature_drop})
    @param fine
      [fine x y] is the terrain height at the fractional grid position (x, y) as
      read from a finer grid covering part of the ray, or [None] outside it.
      Heights are raw: the curvature drop is applied here. Used by the bilinear
      phase only, which is exactly where the ray hugs the terrain.
    @param off_x fractional X offset from src_x (in pixels/meters)
    @param off_y fractional Y offset from src_y (in pixels/meters)
    @param src_x source column (integer)
    @param src_y source row (integer)
    @param dst_x destination column
    @param dst_y destination row
    @return true if destination is visible from source *)
