val test :
  (int -> int -> float) ->
  ?src_h:float ->
  src_x:int ->
  src_y:int ->
  dst_x:int ->
  dst_y:int ->
  unit ->
  bool
(** Test visibility between two points.
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

val test_precise :
  (int -> int -> float) ->
  ?src_h:float ->
  ?curvature:float * float ->
  off_x:float ->
  off_y:float ->
  src_x:int ->
  src_y:int ->
  dst_x:int ->
  dst_y:int ->
  unit ->
  bool
(** Precise visibility test for short distances using bilinear interpolation.
    Tests visibility from (src_x + off_x, src_y + off_y) to (dst_x, dst_y). Uses
    small fixed steps (1 pixel) for accurate sampling up to a few hundred
    meters.
    @param get_height function to get height at (row, col)
    @param src_h
      optional override for source elevation (defaults to terrain + 2m)
    @param off_x fractional X offset from src_x (in pixels/meters)
    @param off_y fractional Y offset from src_y (in pixels/meters)
    @param src_x source column (integer)
    @param src_y source row (integer)
    @param dst_x destination column
    @param dst_y destination row
    @return true if destination is visible from source *)
