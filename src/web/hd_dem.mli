(** Near-field high-resolution elevation (IGN RGE ALTI, live WMTS fetch).

    A grid is a square block of {!layer.size} samples per side, positioned by
    the {!frame} that {!blend} returns. Its axes are not necessarily north and
    east and its position is not tied to the anchor arcsecond, so consumers must
    go through that frame rather than assume either. Several layers can be
    nested, so nothing here is a global constant: read the frame and the sample
    count off the grid in hand. *)

type kind
(** How a block is fetched: a WGS84G tile matrix, a geographic GetMap, or a
    GetMap in the CRS the product is stored in. Opaque -- only the layers below
    build one, and consumers have no reason to tell them apart. *)

type layer = {
  kind : kind;
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
(** LIDAR HD bare earth, in the CRS the product is stored in: 4 m square, 1024
    samples, 4096 m per side. 2 x 2 GetMaps of 512^2, 4.0 MB in total. Nests
    inside {!l13}; {!lidar_2m} nests inside it, exactly box-consistent with it.
*)

val lidar_2m : layer
(** LIDAR HD bare earth at the product's own 1 m, so nothing resamples it: 2048
    samples, 2048 m per side. 2 x 2 GetMaps of 1024^2, 16.0 MB (this endpoint
    does not compress). Exactly box-consistent with {!lidar_5m}, both being
    resolutions of one layer.

    Absent, like {!lidar_5m}, wherever {!Projection} has no CRS for the
    location: {!fetch} answers [None] and the blend falls back to {!l13}. *)

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

type frame = {
  to_index : Affine.t;
      (** arcseconds from the anchor arcsecond to a fractional (column, row) *)
  of_index : Affine.t;  (** and back *)
  arcsec_step : float;
      (** sample pitch in arcseconds: the mip level the mesh reads and how
          finely a sight line is walked are both measured against the
          one-arcsecond base grid *)
  step_x_m : float;  (** metres on the ground per column step *)
  step_y_m : float;
}
(** Where a refinement's samples sit. Rows count from the south, as everywhere
    else.

    For a graticule-aligned block this is exactly the
    [(offset - origin) / px_arcsec] that consumers used to compute themselves.
    For a block served in its own projected CRS the off-diagonal terms are not
    zero, because its axes are turned from north by that CRS's grid convergence
    -- 2.44 degrees at 6.36 E. Reading the frame rather than an origin and a
    spacing is what lets both kinds share one path. *)

type t = {
  layer : layer;  (** the layer this grid came from *)
  grid : Dem_loader.t;  (** blended heights, row 0 southernmost *)
  frame : frame;  (** where those samples sit *)
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
