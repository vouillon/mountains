(** Near-field high-resolution elevation (IGN RGE ALTI, live WMTS fetch).

    The grid is a square block of level-13 WGS84G tiles, {!size} samples per
    side at {!px_arcsec} arcseconds per sample, centred on the tile holding the
    location. Its position is not tied to the anchor arcsecond, so consumers
    must use the origin returned by {!blend}. *)

val size : int
(** Number of samples per side. *)

val px_arcsec : float
(** Arcseconds per sample: the one spacing constant of the grid. *)

type raw
(** Raw elevations as returned by the service, in metres, north-up, with the
    tiles that could not be fetched left at nodata. *)

val fetch : lat:float -> lon:float -> raw option Lwt.t
(** Fetch the high-resolution block around [lat]/[lon]. Never fails: [None] (not
    a single tile obtained, whether the requests failed or the deadline expired
    first) means the caller must carry on with the base DEM alone. Individual
    tiles that fail -- including the 404 of a tile outside coverage, and those
    still in flight when the deadline expires -- are nodata, which {!blend}
    turns back into the base. *)

type t = {
  grid : Dem_loader.t;  (** blended heights, row 0 southernmost *)
  origin_x : float;  (** arcseconds from the anchor to sample column 0 *)
  origin_y : float;  (** arcseconds from the anchor to the southernmost row *)
}

val prefetch : lat:float -> lon:float -> unit Lwt.t
(** Warm the persistent tile cache for offline use with a block twice the
    extent's width around [lat]/[lon] (~40 MB), so every location within about
    10 km keeps its full near-field refinement offline. Tiles already cached (a
    cached 404 included) are not requested again; failures are ignored. *)

val blend : lat:float -> base:Dem_loader.t -> raw -> t option
(** [blend ~lat ~base raw] resamples [base] onto the high-resolution grid and
    mixes the high-resolution correction into it, fading it out at the edge of
    the extent and around nodata. The result equals the base upsample wherever
    the correction is absent. [None] when the patch holds no valid sample at all
    (outside French coverage). *)
