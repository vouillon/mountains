(** Near-field high-resolution elevation (IGN RGE ALTI, live WMTS fetch).

    A grid is a square block of WGS84G tiles, {!layer.size} samples per side at
    {!layer.px_arcsec} arcseconds per sample, centred on the tile holding the
    location. Its position is not tied to the anchor arcsecond, so consumers
    must use the origin returned by {!blend}. Several layers can be nested, so
    nothing here is a global constant: read the spacing and the sample count off
    the {!layer} carried by the grid in hand. *)

type kind =
  | Wmts of { matrix_level : int; tiles_per_axis : int; block_tiles : int }
      (** A block of tiles from a WGS84G matrix level, centred on the tile
          holding the location. *)
  | Wms of { wms_name : string; step_arcsec : float; steps : int; split : int }
      (** GetMap over a bbox aligned to a [step_arcsec] grid and spanning
          [steps] steps, centred on the grid corner nearest the location.
          Resolution is free-form: the matrix levels do not bound it. Fetched as
          [split] x [split] pieces, so one failed request costs a quadrant
          rather than the ring; exact, because a piece has the same pixel pitch
          as the whole. *)

type layer = {
  kind : kind;
  px_arcsec : float;  (** sample spacing; derived, never a power of two *)
  size : int;  (** samples per side *)
  fade_metres : float;
      (** width of the annulus over which the refinement is faded back into the
          coarser surface, at the edge of the extent and around nodata *)
}
(** One refinement ring. Rings nest: each is blended onto the next coarser
    surface, so the renderer can pick between them on a plain extent test. *)

val l13 : layer
(** RGE ALTI over WMTS level 13, 8 x 8 tiles: 0.309 arcsec (9.5 m N-S), 2048
    samples per side, 19.5 x 13.6 km at 46 degrees, ~13.7 MB on the wire. *)

val lidar_5m : layer
(** LIDAR HD bare earth over WMS: 0.1545 arcsec (4.77 m N-S, 3.31 m E-W at 46
    degrees), 1024 samples over 4 x 4 level-14 footprints, i.e. +-2.44 x 1.70
    km. 2 x 2 GetMaps of 512^2, 4.0 MB in total. Nests inside {!l13};
    {!lidar_2m} nests inside it, exactly box-consistent with it. *)

val lidar_2m : layer
(** LIDAR HD bare earth over WMS: 0.0772 arcsec (2.38 m N-S, 1.66 m E-W at 46
    degrees), 1024 samples over 2 x 2 level-14 footprints, i.e. +-1.22 x 0.87
    km. One GetMap, 4.0 MB (the WMS endpoint does not compress). Nests inside
    {!l13}. *)

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

val block_id : layer -> lat:float -> lon:float -> string
(** Identity of the block [fetch] would ask for at [lat]/[lon]: two locations
    share it exactly when the requests are the same, i.e. when they are asking
    for the same ground. What lets a nearby location keep a ring instead of
    fetching and blending it again (see {!reanchor}). *)

type t = {
  layer : layer;  (** the layer this grid came from *)
  grid : Dem_loader.t;  (** blended heights, row 0 southernmost *)
  origin_x : float;  (** arcseconds from the anchor to sample column 0 *)
  origin_y : float;  (** arcseconds from the anchor to the southernmost row *)
  height_scale : float;
      (** metres per u16 step of {!grid}. Each grid is quantised over only the
          height range it holds -- a few hundred metres rather than the base
          tile's 9500 -- because the relief bake differentiates it and divides
          by a spacing several times finer, so the base scale showed in the
          normals. {!grid} must therefore not be read through
          [Dem_loader.get_height]: use {!get_height}. *)
  height_offset : float;  (** metres at u16 zero *)
}

val get_height : t -> int -> int -> float
(** [get_height t row col] in metres, decoded with this grid's own quantisation.
*)

val reanchor : t -> old_anchor:int * int -> anchor:int * int -> t
(** The same grid with its origins stated against another anchor arcsecond
    (latitude, longitude, in arcseconds). The samples are absolute ground, so
    this is exact for any location holding the same {!block_id}; it is not a
    resampling and touches nothing but the two origins. *)

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
