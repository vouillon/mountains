(** The arithmetic core of {!Hd_dem.blend}: one refinement grid mixed into the
    surface beneath it, with no IO and no browser dependency.

    Split out so that the boundary is explicit and narrow -- plain arrays and
    scalars -- because a hand-written wasm implementation has to reproduce it
    exactly and be differentially tested against this one (see PLAN.md,
    "blend.wat in the worker"). The source arrives as an extracted {!geometry}
    window rather than a whole tile, which is both what a worker would be sent
    and what keeps the two implementations indexing the same bytes. *)

open Bigarray

val nodata_limit : float
(** Samples below this are "no data": IGN's -9999 outside coverage, and the
    -99999 {!Hd_dem} pre-fills a block with so a request that never arrives
    reads the same way. *)

type params = {
  size : int;  (** samples per side of the refinement *)
  to_src : Affine.t;
      (** a refinement sample index to a fractional source sample index *)
  src_size : int;  (** samples per side of the surface beneath *)
  src_height_scale : float;  (** metres per u16 step of the source *)
  src_height_offset : float;  (** metres at source u16 zero *)
  fade_x : float;  (** fade width, in refinement samples, per axis *)
  fade_y : float;
}
(** Rows count from the south on both grids, matching {!Dem_loader}; the
    refinement's own samples are north-up, which {!run} accounts for.

    {!to_src} is one map rather than an origin and a spacing per axis because a
    refinement served in its own projected CRS sits on a grid whose axes are
    turned from the source's -- see [Hd_dem.frame]. *)

val axis_aligned : params -> bool
(** Whether columns depend only on the column index and rows only on the row
    index, so a resampler may hoist one source row pair per source row and
    precompute the column indices once. False only where a projected grid meets
    a graticule-aligned one; [Blend_wasm] tests it to decide whether
    [blend.wat], which assumes it, can be used at all. *)

type geometry = {
  col_lo : int;
  row_lo : int;
  n_cols : int;  (** columns spanned by the bilinear window *)
  n_rows : int;
  win_cols : int;  (** columns to copy, from [col_lo]; at least [n_cols] *)
  win_rows : int;
}
(** The rectangle of the source a blend reads: the bilinear window plus the one
    row and column beyond it that the range scan reaches. *)

val geometry : params -> geometry
(** Cheap: a handful of flops. {!run} recomputes it rather than taking it, so a
    caller cannot pass one that disagrees with the window it extracted. *)

type result = {
  data : (int, int8_unsigned_elt, c_layout) Array1.t;
      (** [size] rows of [size] little-endian u16, row 0 southernmost *)
  height_scale : float;  (** metres per u16 step of {!data} *)
  height_offset : float;  (** metres at u16 zero *)
  range : float;  (** metres spanned by the bound, for logging *)
}

val run :
  params ->
  samples:(float, float32_elt, c_layout) Array1.t ->
  win:(int, int8_unsigned_elt, c_layout) Array1.t ->
  result option
(** [run p ~samples ~win] resamples the source window onto the refinement's grid
    and mixes [samples] into it, fading the refinement out over
    [fade_x]/[fade_y] at the edge of the extent and around nodata, so the result
    equals the source upsample wherever the correction is absent.

    [samples] is [size * size] metres, north-up. [win] is [win_rows * win_cols]
    little-endian u16 taken from the source at [(row_lo, col_lo)]. The result
    carries its own quantisation, bounded rather than measured, so it must not
    be read through {!Dem_loader.get_height}.

    [None] when [samples] holds no valid value at all (a location outside French
    coverage), which keeps such locations on the source alone rather than on a
    bilinear upsample of it. *)
