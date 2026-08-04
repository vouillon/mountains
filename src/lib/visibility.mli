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

type refinement = {
  sample : float -> float -> float option;
      (** height at a fractional base-grid position, [None] outside this grid *)
  step : float;
      (** finest useful walk step over this grid, in base-grid pixels: half its
          sample spacing. Keeps a grazing ray from being walked far finer than
          the data it is reading. *)
}
(** One finer grid covering part of the ray (see [Hd_dem]). *)

val test_precise :
  (int -> int -> float) ->
  ?src_h:float ->
  ?curvature:float * float ->
  ?fine:refinement list ->
  off_x:float ->
  off_y:float ->
  src_x:int ->
  src_y:int ->
  dst_x:int ->
  dst_y:int ->
  unit ->
  bool
(** Precise visibility test. Tests visibility from (src_x + off_x, src_y +
    off_y) to (dst_x, dst_y).

    Marches the ray with bilinear interpolation, taking each step from the
    clearance between the ray and the terrain below it: tight where the ray
    grazes the ground, long where it flies high, bounded by one pixel so it is
    never coarser than the whole-pixel walk it replaces. The source's 1-pixel
    neighbourhood must lie inside the height grid.
    @param get_height function to get height at (row, col)
    @param src_h
      optional override for source elevation (defaults to terrain + 2m)
    @param curvature
      metres per pixel (x, y): when given, heights are evaluated in the
      observer-anchored frame lowered by the Earth-curvature drop, in which
      sight lines are straight (see {!curvature_drop})
    @param fine
      finer grids covering part of the ray, finest first. Heights are raw: the
      curvature drop is applied here. Consulted along the whole ray, so a
      near-field grid contributes to occlusion everywhere it reaches; each one's
      [step] bounds how finely it is walked. Past them all the walk hands over
      to {!test}.
    @param off_x fractional X offset from src_x (in pixels/meters)
    @param off_y fractional Y offset from src_y (in pixels/meters)
    @param src_x source column (integer)
    @param src_y source row (integer)
    @param dst_x destination column
    @param dst_y destination row
    @return true if destination is visible from source *)
