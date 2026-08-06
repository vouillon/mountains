(* Near-field high-resolution elevation from IGN's RGE ALTI (bare-earth LIDAR
   DTM covering France), fetched live per location from the geoplateforme WMTS
   as raw little-endian float32 tiles (FORMAT=image/x-bil;bits=32).

   The result is not used as a layer of its own: [blend] resamples the base
   Copernicus tile onto the high-resolution grid and mixes the two, so the grid
   returned to the renderer *equals the base upsample* wherever RGE ALTI is
   missing or faded out. Everything downstream (relief bake, vertex LOD,
   fragment normals, POI anchoring) can therefore switch to it on a plain
   extent test and still degrade exactly to today's rendering at the edges, on
   a fetch failure, or outside France. *)

open Bigarray

let ( let* ) = Lwt.bind

(* WGS84G tiles are always 256 x 256 samples, at every matrix level. *)
let tile_px = 256

(* One refinement ring. Several can be nested (see PLAN.md), so nothing here is
   a module-level constant any more: every consumer reads the spacing and the
   sample count off the layer that produced the grid it is holding.

   [px_arcsec] and [size] are derived, never passed in, so they cannot drift
   from [matrix_level] and [block_tiles]. The spacing is *not* a power-of-two
   ratio to the 1-arcsecond base grid, which is why consumers must read it
   rather than assume one. *)
type kind =
  | Wmts of {
      matrix_level : int;
      tiles_per_axis : int;
      block_tiles : int;
      px_arcsec : float;
    }
      (** A block of tiles from a WGS84G matrix level, centred on the tile
          holding the location. *)
  | Wms_projected of { stem : string; metres : float; split : int }
      (** GetMap in the CRS the product is actually stored in, so that the
          service never reprojects it. That is worth a grid whose axes are not
          north and east: asked for the same ground in WGS84G, the geoplateforme
          returns something 4.29 degrees of normal error away from a clean
          resample of its own data (PLAN.md), a woven corduroy that is plainly
          visible once the procedural detail is off.

          [stem] is the layer name without its CRS suffix; {!Projection}
          supplies the suffix, the [CRS=] code and the projection, one per
          territory. The bbox spans [metres * size] and is aligned to half that,
          in the CRS's own metres, so nearby locations share URLs exactly as
          they share a WMTS tile. Square by construction, and 1 m needs no
          reprojection at all. *)

type layer = {
  kind : kind;
  size : int; (* samples per side of the block *)
  fade_metres : float;
      (* width of the annulus over which the refinement is faded back into the
         coarser surface, both at the edge of the extent and around nodata. Held
         well inside the extent so that the renderer's hard switch at the
         boundary lands where the two agree. *)
}

(* Where a refinement's samples sit, as an affine map both ways between
   arcseconds from the anchor arcsecond and a fractional sample index. Rows count
   from the south, as everywhere else.

   Both directions are stored because both are wanted and neither is cheap to
   keep in step by hand: the mesh, the shadow pass, POI anchoring, visibility and
   the eye height all want an index from an offset, while [Blend_core] wants an
   offset from an index so it can compose with the frame of the surface beneath.
   Deriving one from the other in a single constructor is what stops them
   drifting.

   For a graticule-aligned grid this is exactly the [(offset - origin) /
   px_arcsec] it replaces, with the off-diagonal terms zero. For a grid served in
   its own projected CRS they are not zero: its axes are turned from north by
   that CRS's grid convergence -- 2.44 degrees at 6.36 E, 0.55 at La Reunion. *)
type frame = {
  to_index : Affine.t;
  of_index : Affine.t;
  arcsec_step : float;
      (* sample pitch in arcseconds: sets the mip level the mesh reads and how
         finely a sight line is walked, both of which are measured against the
         one-arcsecond base grid *)
  step_x_m : float; (* metres on the ground per column step *)
  step_y_m : float;
}

(* Ground metres per step along each axis, from the frame itself rather than
   passed in beside it, so a projected grid's scale factor -- its projected metre
   is not quite a ground metre -- is accounted for without anything special. *)
let frame_of_index of_index ~arcsec_step ~lat =
  let deltax, deltay, _ = Render_state.compute_deltas ~lat in
  let axis u v =
    let x, y = Affine.apply of_index u v
    and ox, oy = Affine.apply of_index 0. 0. in
    Float.hypot ((x -. ox) *. deltax) ((y -. oy) *. deltay)
  in
  {
    to_index = Affine.inverse of_index;
    of_index;
    arcsec_step;
    step_x_m = axis 1. 0.;
    step_y_m = axis 0. 1.;
  }

(* A graticule-aligned block: one scale for both axes, in arcseconds. *)
let geographic_frame ~px_arcsec ~origin_x ~origin_y ~lat =
  frame_of_index
    (Affine.diagonal ~sx:px_arcsec ~sy:px_arcsec ~tx:origin_x ~ty:origin_y)
    ~arcsec_step:px_arcsec ~lat

let wmts_layer ~matrix_level ~block_tiles ~fade_metres =
  (* Level L has 2^(L+1) tiles across 360 degrees: level 0 is two tiles wide. *)
  let tiles_per_axis = 1 lsl (matrix_level + 1) in
  {
    kind =
      Wmts
        {
          matrix_level;
          tiles_per_axis;
          block_tiles;
          px_arcsec = 360. /. float tiles_per_axis /. float tile_px *. 3600.;
        };
    size = tile_px * block_tiles;
    fade_metres;
  }

let wms_projected_layer ~stem ~metres ~split ~size ~fade_metres =
  assert (size mod split = 0);
  { kind = Wms_projected { stem; metres; split }; size; fade_metres }

(* Level 13, 8 x 8 tiles: 0.309 arcseconds per sample (9.5 m north-south, a bit
   over three times the base grid), 2048 samples per side -- a power of two, so
   the relief pyramid keeps its usual shape -- spanning 633 arcseconds, i.e.
   19.5 km north-south and 13.6 km east-west at 46 degrees.

   Measured: 214 kB on the wire per tile (the service deflates the payload),
   64 tiles in ~4 s with browser-level parallelism over one HTTP/2
   connection. *)
let l13 = wmts_layer ~matrix_level:13 ~block_tiles:8 ~fade_metres:1500.

(* The two LIDAR HD rings, fetched in the CRS the product is stored in --
   Lambert-93 over metropolitan France, RGR92 / UTM 40S over La Reunion -- rather
   than in the WGS84G reprojection the service also offers. Measured (PLAN.md):
   the reprojection costs 4.29 degrees of normal error against a clean resample of
   IGN's own data, a woven corduroy that dominates the shading once the procedural
   detail is off, and it is absent from the native grid. It also makes the
   graticule-aligned +-1.22 x 0.87 km anisotropic grid a square one.

   [stem] is shared: one product, two resolutions of it, and the coarser output is
   the exact 2 x 2 box mean of the finer, so the inner ring's fade onto the middle
   one hides no product difference at all. Only the middle ring's fade onto l13
   crosses products, and it has 600 m of annulus for a 3 m worst case.

   The middle ring: 5 m, 1024 samples, so 5120 m per side. Sized by reach rather
   than by spacing: with the alignment rule below it covers at least 1920 m in
   every direction, against the 1832 m worst case of the +-2.44 x 1.70 km ring it
   replaces, and what lies beyond it falls through to l13 at 9.5 m. 2 x 2 GetMaps
   of 512^2, 4.0 MB. *)
let lidar_5m =
  wms_projected_layer ~stem:"IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE"
    ~metres:5.0 ~split:2 ~size:1024 ~fade_metres:600.

(* The innermost ring: 1 m -- the product's own resolution, so no resampling of it
   happens anywhere -- 2048 samples, 2048 m per side against the +-1.22 x 0.87 km
   it replaces. 2 x 2 GetMaps of 1024^2, 16.0 MB (this endpoint does not
   compress).

   [fade_metres] is 300 rather than [l13]'s 1500: the extent is a sixteenth of the
   width and the surface underneath is the same product, so the annulus has to
   hide nothing at all here, only the quantisation change.

   RGE ALTI cannot serve this: below level 14 its WMS output is
   nearest-neighbour replication of the 4.77 m grid (100% of 4x4 blocks
   bit-identical), so LIDAR HD is the only real sub-5 m bare-earth source. Its
   water is unrectified, which does not matter because terrain.frag replaces the
   DEM normal with the procedural water normal wherever the cover map says
   water. *)
let lidar_2m =
  wms_projected_layer ~stem:"IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE"
    ~metres:1.0 ~split:2 ~size:2048 ~fade_metres:300.

(* Ceiling on the whole set of tile requests. The location cannot be published
   before this resolves, so it is also the worst-case load-time penalty;
   expiring is not an error, the block is published with whatever has arrived by
   then -- the tiles still in flight are nodata, which the blend fades back into
   the base like any other gap. Only a block without a single tile falls back to
   the base alone. *)
let timeout_s = 25.

(* The base grid's u16 quantisation, shared with the .dem pipeline
   (Corine/compress_dem.ml) and [Dem_loader.get_height]: 9500 m over 65536 steps,
   so 14.5 cm.

   A refinement covers about a kilometre of ground and a few hundred metres of
   height, so spending that range on the whole habitable envelope wastes most of
   the resolution -- and precision matters more here than for the base, because
   the relief bake differentiates the grid and divides by a spacing four to
   twelve times finer. Each blended grid therefore gets its own scale, spanning
   only what it actually holds; [base_height_scale] is what the base tile
   happens to use. *)
let base_height_scale = 9500. /. 65535.
let base_height_offset = -500.

let wmts_url ~matrix_level ~row ~col =
  Printf.sprintf
    "https://data.geopf.fr/wmts?SERVICE=WMTS&VERSION=1.0.0&REQUEST=GetTile&LAYER=ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES&STYLE=normal&FORMAT=image/x-bil;bits=32&TILEMATRIXSET=WGS84G&TILEMATRIX=%d&TILEROW=%d&TILECOL=%d"
    matrix_level row col

(* WMS 1.3.0 takes BBOX in the CRS's own axis order. EPSG:2154 and EPSG:2975 both
   declare easting first, unlike EPSG:4326 above. Three decimals of a metre keeps
   the string stable while leaving room for an alignment grid that is not a whole
   number of metres. *)
let wms_projected_url ~stem ~crs ~size ~x_min ~y_min ~x_max ~y_max =
  Printf.sprintf
    "https://data.geopf.fr/wms-r/wms?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=%s.%s&STYLES=&CRS=%s&FORMAT=image/x-bil;bits=32&BBOX=%.3f,%.3f,%.3f,%.3f&WIDTH=%d&HEIGHT=%d"
    stem (Projection.name crs) (Projection.epsg crs) x_min y_min x_max y_max
    size size

let to_lwt f =
  let t, u = Lwt.task () in
  (Fut.await f @@ fun v -> Lwt.wakeup u v);
  t

(* The tile of a matrix level holding the location, on which a WMTS block is
   centred. Anchoring on the *tile* rather than on the location makes every
   location within one tile share the same URLs, which is what makes the service
   worker's cache-first rule worth having. *)
let anchor_tile ~tiles_per_axis ~lat ~lon =
  let alat = Web_utils.arcsec_floor lat and alon = Web_utils.arcsec_floor lon in
  let span = 360. /. float tiles_per_axis in
  let row = int_of_float (floor ((90. -. (float alat /. 3600.)) /. span)) in
  let col = int_of_float (floor (((float alon /. 3600.) +. 180.) /. span)) in
  (alat, alon, row, col)

(* One fetch feeding a [px] x [px] square of the block at [dst_row]/[dst_col].
   Both kinds reduce to a list of these, so the request loop, the deadline and
   the partial-block handling below are written once. *)
type request = { url : string; px : int; dst_row : int; dst_col : int }

(* Where the block sits, as a {!frame}, and what to fetch for it. Sample centres
   sit half a sample inside the block's edge on both axes, on every kind.

   [None] when the layer cannot serve this location at all, which today means a
   projected layer outside every territory {!Projection} knows: the caller treats
   it exactly like a block whose every request failed, and the blend falls back
   to the coarser surface. *)
let plan layer ~lat ~lon =
  match layer.kind with
  | Wmts { matrix_level; tiles_per_axis; block_tiles; px_arcsec } ->
      let alat, alon, anchor_row, anchor_col =
        anchor_tile ~tiles_per_axis ~lat ~lon
      in
      let row0 = anchor_row - (block_tiles / 2) in
      let col0 = anchor_col - (block_tiles / 2) in
      let origin_x =
        -648000. +. ((float (col0 * tile_px) +. 0.5) *. px_arcsec) -. float alon
      in
      let origin_y =
        324000.
        -. ((float ((row0 * tile_px) + layer.size - 1) +. 0.5) *. px_arcsec)
        -. float alat
      in
      let reqs =
        List.concat_map
          (fun br ->
            List.map
              (fun bc ->
                {
                  url = wmts_url ~matrix_level ~row:(row0 + br) ~col:(col0 + bc);
                  px = tile_px;
                  dst_row = br * tile_px;
                  dst_col = bc * tile_px;
                })
              (List.init block_tiles Fun.id))
          (List.init block_tiles Fun.id)
      in
      Some (geographic_frame ~px_arcsec ~origin_x ~origin_y ~lat, reqs)
  | Wms_projected { stem; metres; split } -> (
      match Projection.of_location ~lat ~lon with
      | None -> None
      | Some crs ->
          let alat = Web_utils.arcsec_floor lat
          and alon = Web_utils.arcsec_floor lon in
          let anchor_lat = float alat /. 3600.
          and anchor_lon = float alon /. 3600. in
          let xa, ya = Projection.forward crs ~lat:anchor_lat ~lon:anchor_lon in
          let span = float layer.size *. metres in
          (* A quarter of the span, so the anchor lands within an eighth of it
             of the centre and the block reaches at least three eighths of a span
             in every direction. Measured the hard way: at half a span the anchor
             can sit a quarter off centre, and the ring then reached 1292 m north
             of a camera looking north -- against the 1832 m worst case of the
             layer it replaces -- so the mid field fell through to l13 at 9.5 m
             and visibly lost its detail. Still one bbox per quarter-span, so
             nearby locations share URLs. *)
          let align = span /. 4. in
          let corner v = Float.round (v /. align) *. align in
          let x0 = corner xa -. (span /. 2.)
          and y0 = corner ya -. (span /. 2.) in
          (* Arcseconds from the anchor to projected metres from the anchor, from
             the local derivative of the projection: conformal, so over a couple
             of kilometres this is a rotation and a scale to within centimetres
             (0.008 degrees of turn and a part in a million of scale across the
             block). *)
          let j11, j12, j21, j22 =
            Projection.jacobian crs ~lat:anchor_lat ~lon:anchor_lon
          in
          let per_arcsec =
            {
              Affine.a = j11 /. 3600.;
              b = j12 /. 3600.;
              c = 0.;
              d = j21 /. 3600.;
              e = j22 /. 3600.;
              f = 0.;
            }
          in
          let of_index =
            Affine.compose
              (Affine.inverse per_arcsec)
              (Affine.diagonal ~sx:metres ~sy:metres
                 ~tx:(x0 -. xa +. (0.5 *. metres))
                 ~ty:(y0 -. ya +. (0.5 *. metres)))
          in
          let px = layer.size / split in
          let piece = span /. float split in
          let reqs =
            List.concat_map
              (fun qr ->
                List.map
                  (fun qc ->
                    (* Raster row 0 is the northernmost, so the piece at
                       [dst_row] 0 is the top band and takes the highest
                       northings. *)
                    let x = x0 +. (float qc *. piece) in
                    let y = y0 +. (float (split - 1 - qr) *. piece) in
                    {
                      url =
                        wms_projected_url ~stem ~crs ~size:px ~x_min:x ~y_min:y
                          ~x_max:(x +. piece) ~y_max:(y +. piece);
                      px;
                      dst_row = qr * px;
                      dst_col = qc * px;
                    })
                  (List.init split Fun.id))
              (List.init split Fun.id)
          in
          (* One sample covers [metres] square in projected metres; the pitch in
             arcseconds is what the mip selection and the ray walk want, and the
             area per sample gives it without picking an axis. *)
          Some
            ( frame_of_index of_index
                ~arcsec_step:(sqrt (Float.abs (Affine.det of_index)))
                ~lat,
              reqs ))

type raw = {
  layer : layer;
  samples : (float, float32_elt, c_layout) Array1.t;
      (* [layer.size] x [layer.size], row 0 northernmost *)
  frame : frame;
  missing : int;
}

(* One tile. A tile fully outside RGE ALTI coverage answers 404 with an
   exception report; that -- like any other per-tile failure -- just leaves its
   region at nodata, which the blend turns back into the base. *)
let fetch_one req =
  let open Brr_io.Fetch in
  let* res = to_lwt (url (Jstr.v req.url)) in
  match res with
  | Error e -> Lwt.fail (Failure (Jstr.to_string (Jv.Error.message e)))
  | Ok resp when not (Response.ok resp) ->
      Lwt.fail (Failure (Printf.sprintf "HTTP %d" (Response.status resp)))
  | Ok resp -> (
      let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
      match buf with
      | Error e -> Lwt.fail (Failure (Jstr.to_string (Jv.Error.message e)))
      | Ok buf ->
          let expected = req.px * req.px * 4 in
          if Brr.Tarray.Buffer.byte_length buf <> expected then
            Lwt.fail
              (Failure
                 (Printf.sprintf "expected %d bytes, got %d" expected
                    (Brr.Tarray.Buffer.byte_length buf)))
          else Lwt.return (Brr.Tarray.of_buffer Brr.Tarray.Float32 buf))

(* Fetch only: the blend needs the base tile, which is being fetched in
   parallel. Never fails: [None] means "render from the base tile alone". *)
let fetch layer ~lat ~lon : raw option Lwt.t =
  let size = layer.size in
  match plan layer ~lat ~lon with
  | None -> Lwt.return None
  | Some (frame, reqs) ->
      let full = Brr.Tarray.create Brr.Tarray.Float32 (size * size) in
      (* Below [Blend_core.nodata_limit], so a request that never arrives reads
     exactly like ground outside IGN's coverage (which answers -9999): both are
     faded back into the surface beneath, and a viewpoint on the frontier
     degrades by the same path as one whose fetch half failed. *)
      Brr.Tarray.fill (-99999.) full;
      let total = List.length reqs in
      (* Counting the tiles that arrived, not the ones that failed: on the timeout
     path the requests still in flight are neither, and it is the arrived count
     that says whether the block is worth publishing. *)
      let arrived = ref 0 in
      (* Set as soon as [full] is handed out. A tile arriving after that must not be
     blitted: the raw block is published by reference, and the late write would
     land in a buffer the caller has already started reading. The request itself
     is not cancelled, so its response still reaches the service worker's cache
     and the next visit to this location gets it for free. *)
      let published = ref false in
      let t0 = Brr.(Performance.now_ms G.performance) in
      (* Every request is issued at once: the browser multiplexes them over one
     HTTP/2 connection. *)
      let tasks =
        List.map
          (fun req ->
            Lwt.catch
              (fun () ->
                let* tile = fetch_one req in
                if not !published then begin
                  (* Blit row by row: the response is contiguous, the destination is
                 strided by the width of the whole block. *)
                  for i = 0 to req.px - 1 do
                    Brr.Tarray.set_tarray full
                      ~dst:(((req.dst_row + i) * size) + req.dst_col)
                      (Brr.Tarray.sub tile ~start:(i * req.px)
                         ~stop:((i + 1) * req.px))
                  done;
                  incr arrived
                end;
                Lwt.return ())
              (fun _ -> Lwt.return ()))
          reqs
      in
      let publish () =
        published := true;
        if !arrived = 0 then None
        else
          Some
            {
              layer;
              samples = Brr.Tarray.to_bigarray1 full;
              frame;
              missing = total - !arrived;
            }
      in
      let all =
        let* () = Lwt.join tasks in
        (* The timeout got there first; [Lwt.choose] discards this. *)
        if !published then Lwt.return None
        else begin
          if !arrived = 0 then
            Brr.Console.error
              [ Jstr.v "High-resolution elevation: no tile could be fetched" ]
          else
            Format.eprintf "RGE ALTI: %d/%d tiles in %.0f ms@." !arrived total
              (Brr.(Performance.now_ms G.performance) -. t0);
          Lwt.return (publish ())
        end
      in
      let timeout =
        let t, w = Lwt.task () in
        ignore
          (Brr.G.set_timeout
             ~ms:(truncate (timeout_s *. 1000.))
             (fun () ->
               if not !published then begin
                 if !arrived = 0 then
                   Brr.Console.error
                     [ Jstr.v "High-resolution elevation: request timed out" ]
                 else
                   Format.eprintf
                     "RGE ALTI: timed out with %d/%d tiles after %.0f ms@."
                     !arrived total
                     (Brr.(Performance.now_ms G.performance) -. t0);
                 Lwt.wakeup_later w (publish ())
               end));
        t
      in
      Lwt.choose [ all; timeout ]

type t = {
  layer : layer;
  grid : Dem_loader.t;
  frame : frame;
  height_scale : float; (* metres per u16 step of [grid] *)
  height_offset : float; (* metres at u16 zero *)
}

(* [grid] carries its own quantisation, so it must not be read through
   [Dem_loader.get_height], which assumes the base one. *)
let get_height (t : t) row col =
  let low = t.grid.Dem_loader.data.{row, col * 2} in
  let high = t.grid.Dem_loader.data.{row, (col * 2) + 1} in
  (float_of_int ((high lsl 8) lor low) *. t.height_scale) +. t.height_offset

(* The surface a refinement is mixed into: any u16 grid with a known origin and
   spacing, so the layers can be chained (base -> L13 -> finer). The fields are
   prefixed because [t] and [raw] carry same-named ones and OCaml resolves record
   fields by the last type defined -- an unprefixed [origin_x] here would silently
   pick the wrong record.

   Rows count from the south on both sides, matching [Dem_loader]. *)
type source = {
  src_grid : Dem_loader.t;
  src_to_index : Affine.t;
      (* arcseconds from the anchor to a fractional sample index of this grid,
         the same map a [frame] carries; a source needs no more than that *)
  src_height_scale : float; (* metres per u16 step *)
  src_height_offset : float; (* metres at u16 zero *)
}

(* The base DEM: one arcsecond per sample, with the anchor arcsecond at index
   [size / 2] on both axes (see [Dem_loader.load]), i.e. sample 0 sits half the
   tile west and south of it. *)
let base_source (grid : Dem_loader.t) =
  let o = -.float (grid.Dem_loader.size / 2) in
  {
    src_grid = grid;
    src_to_index = Affine.inverse (Affine.diagonal ~sx:1.0 ~sy:1.0 ~tx:o ~ty:o);
    src_height_scale = base_height_scale;
    src_height_offset = base_height_offset;
  }

(* A blended grid used in turn as the surface for a finer layer. *)
let as_source (t : t) =
  {
    src_grid = t.grid;
    src_to_index = t.frame.to_index;
    src_height_scale = t.height_scale;
    src_height_offset = t.height_offset;
  }

(* Build the blended grid: [source] resampled onto the refinement's grid, plus
   [fade] times the refinement's correction. [fade] is 1 in the interior, ramps
   to 0 over [fade_metres] at the edge of the extent and around nodata, and is 0
   on nodata itself, so the result is exactly the source upsample there.

   Returns [None] when the patch holds no valid sample at all (a location
   outside French coverage), which keeps such locations on the source alone
   rather than on a bilinear upsample of it. *)
(* The window of [source] that [Blend_core] will read, copied out row by row.
   Only ~636^2 samples for [l13] over the base -- 0.8 MB against the tile's
   32 MB -- which is what makes handing the blend to a worker cheap. *)
let source_window (source : source) (g : Blend_core.geometry) =
  let bdata = source.src_grid.Dem_loader.data in
  let stride = g.win_cols * 2 in
  let win = Array1.create int8_unsigned c_layout (g.win_rows * stride) in
  for r = 0 to g.win_rows - 1 do
    Array1.blit
      (Array1.sub
         (Array2.slice_left bdata (g.row_lo + r))
         (g.col_lo * 2) stride)
      (Array1.sub win (r * stride) stride)
  done;
  win

(* Two rings in one projected CRS do share axes, but the composition below cannot
   quite say so: it goes out through one frame and back in through the other, and
   a projection composed with a numerical inverse of itself leaves off-diagonal
   terms of order 1e-17 instead of zero. Snapped here, where the rounding is
   introduced, so that [Blend_core.axis_aligned] stays an exact test rather than a
   tolerance shared by two implementations -- and so the fast path is not lost to
   noise. The term dropped displaces a sample by under 1e-6 of one, against the
   ~20 samples a genuine 2.44-degree turn displaces. *)
let snap_axes (m : Affine.t) ~size =
  let negligible v = Float.abs v *. float size < 1e-6 in
  if negligible m.Affine.b && negligible m.Affine.d then
    { m with Affine.b = 0.; d = 0. }
  else m

let blend ~lat:_ ~(source : source) (raw : raw) =
  let { size; fade_metres; _ } = raw.layer in
  let t0 = Brr.(Performance.now_ms G.performance) in
  let p =
    {
      Blend_core.size;
      (* Refinement index to source index: out to arcseconds through the
         refinement's own frame, back in through the source's. Both being
         affine, so is the composition, whether or not the two grids share
         axes. *)
      to_src =
        snap_axes (Affine.compose source.src_to_index raw.frame.of_index) ~size;
      src_size = source.src_grid.Dem_loader.size;
      src_height_scale = source.src_height_scale;
      src_height_offset = source.src_height_offset;
      fade_x = fade_metres /. raw.frame.step_x_m;
      fade_y = fade_metres /. raw.frame.step_y_m;
    }
  in
  let win = source_window source (Blend_core.geometry p) in
  match Blend_wasm.run p ~samples:raw.samples ~win with
  | None -> None
  | Some { data; height_scale; height_offset; range } ->
      Format.eprintf
        "RGE ALTI blended in %.0f ms (missing tiles: %d, %.0f m range, %.2f cm \
         steps)@."
        (Brr.(Performance.now_ms G.performance) -. t0)
        raw.missing range (height_scale *. 100.);
      Some
        {
          layer = raw.layer;
          grid =
            {
              Dem_loader.data =
                reshape_2 (genarray_of_array1 data) size (size * 2);
              size;
            };
          frame = raw.frame;
          height_scale;
          height_offset;
        }

(* Warm the persistent cache for offline use, like [Dem_loader.prefetch]: a
   block twice the extent's width centred on the same anchor gives every
   location within [block_tiles / 2] tiles (~10 km) of [lat]/[lon] its full
   extent offline, for ~40 MB on the wire. The geoplateforme exempts the WMTS
   from its rate limiting, so all the requests are issued at once, like the
   extent's own.

   The requests are made through the service worker, which stores every
   response in the persistent data cache -- a 404 included, since it means
   "outside French territory" and is served back rather than asked again.
   [Cache.add] would reject those, hence fetches rather than the direct cache
   writes of [Dem_loader.prefetch]. *)
let rec prefetch layer ~lat ~lon =
  match layer.kind with
  (* WMS blocks are deliberately not prefetched: 4 MB buys ~600 m of offline
     roaming against the ~40 MB that buys ~10 km for [l13], which is the wrong
     trade. Their absence offline is invisible -- the blend falls back to the
     coarser surface. *)
  | Wms_projected _ -> Lwt.return_unit
  | Wmts { matrix_level; tiles_per_axis; block_tiles; _ } ->
      prefetch_wmts ~matrix_level ~tiles_per_axis ~block_tiles ~lat ~lon

and prefetch_wmts ~matrix_level ~tiles_per_axis ~block_tiles ~lat ~lon =
  let _, _, anchor_row, anchor_col = anchor_tile ~tiles_per_axis ~lat ~lon in
  let n = 2 * block_tiles in
  let row0 = anchor_row - (n / 2) and col0 = anchor_col - (n / 2) in
  let* cache =
    to_lwt
      (Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ()) (Jstr.v "v1"))
  in
  match cache with
  | Error _ -> Lwt.return_unit
  | Ok cache ->
      let fetched = ref 0 in
      let tasks =
        List.concat_map
          (fun br ->
            List.map
              (fun bc ->
                Lwt.catch
                  (fun () ->
                    let url =
                      Jstr.v
                        (wmts_url ~matrix_level ~row:(row0 + br) ~col:(col0 + bc))
                    in
                    let* cached =
                      to_lwt
                        (Brr_io.Fetch.Cache.match' cache
                           (Brr_io.Fetch.Request.v url))
                    in
                    match cached with
                    | Ok (Some _) -> Lwt.return_unit
                    | Ok None | Error _ -> (
                        let* res = to_lwt (Brr_io.Fetch.url url) in
                        match res with
                        | Ok _ ->
                            incr fetched;
                            Lwt.return_unit
                        | Error _ -> Lwt.return_unit))
                  (fun _ -> Lwt.return_unit))
              (List.init n Fun.id))
          (List.init n Fun.id)
      in
      let* () = Lwt.join tasks in
      if !fetched > 0 then
        Format.eprintf "RGE ALTI: prefetched %d tiles@." !fetched;
      Lwt.return_unit
