(** Near-field high-resolution elevation (IGN RGE ALTI, live WMTS fetch).

    A grid is a square block of WGS84G tiles, {!layer.size} samples per side at
    {!layer.px_arcsec} arcseconds per sample, centred on the tile holding the
    location. Its position is not tied to the anchor arcsecond, so consumers
    must use the origin returned by {!blend}. Several layers can be nested, so
    nothing here is a global constant: read the spacing and the sample count off
    the {!layer} carried by the grid in hand. *)

type layer = {
  matrix_level : int;  (** WGS84G tile matrix level *)
  tiles_per_axis : int;  (** tiles across 360 degrees at that level *)
  block_tiles : int;  (** block is [block_tiles] x [block_tiles] tiles *)
  px_arcsec : float;  (** sample spacing; derived, never a power of two *)
  size : int;  (** samples per side; derived *)
  fade_metres : float;
      (** width of the annulus over which the refinement is faded back into the
          coarser surface, at the edge of the extent and around nodata *)
}
(** One refinement ring. *)

val l13 : layer
(** Level 13, 8 x 8 tiles: 0.309 arcsec (9.5 m N-S), 2048 samples per side, 19.5
    x 13.6 km at 46 degrees, ~13.7 MB on the wire. *)

type raw
(** Raw elevations as returned by the service, in metres, north-up, with the
    tiles that could not be fetched left at nodata. Carries the {!layer} it was
    fetched for, so {!blend} cannot be handed a mismatched one. *)

val fetch : layer -> lat:float -> lon:float -> raw option Lwt.t
(** Fetch [layer]'s block around [lat]/[lon]. Never fails: [None] (not a single
    tile obtained, whether the requests failed or the deadline expired first)
    means the caller must carry on without this layer. Individual tiles that
    fail -- including the 404 of a tile outside coverage, and those still in
    flight when the deadline expires -- are nodata, which {!blend} turns back
    into the coarser surface. *)

type t = {
  layer : layer;  (** the layer this grid came from *)
  grid : Dem_loader.t;  (** blended heights, row 0 southernmost *)
  origin_x : float;  (** arcseconds from the anchor to sample column 0 *)
  origin_y : float;  (** arcseconds from the anchor to the southernmost row *)
}

val prefetch : layer -> lat:float -> lon:float -> unit Lwt.t
(** Warm the persistent tile cache for offline use with a block twice the
    extent's width around [lat]/[lon] (~40 MB for {!l13}), so every location
    within about half an extent keeps its full near field offline. Tiles already
    cached (a cached 404 included) are not requested again; failures are
    ignored. *)

type source
(** The surface a refinement is mixed into: any u16 grid with a known origin and
    spacing, so that layers can be chained (base -> {!l13} -> finer). *)

val base_source : Dem_loader.t -> source
(** The base DEM as a source: one arcsecond per sample, anchor arcsecond at
    index [size / 2] on both axes. *)

val as_source : t -> source
(** A blended grid used in turn as the surface for a finer layer. *)

val blend : lat:float -> source:source -> raw -> t option
(** [blend ~lat ~source raw] resamples [source] onto [raw]'s grid and mixes the
    refinement into it, fading it out at the edge of the extent and around
    nodata. The result equals the source upsample wherever the correction is
    absent, which is what lets the renderer switch between layers on a plain
    extent test. [None] when the patch holds no valid sample at all (outside
    French coverage). *)
