(* Near-field high-resolution elevation from IGN's RGE ALTI (bare-earth LIDAR
   DTM covering France), fetched live per location from the geoplateforme WMTS
   as raw little-endian float32 tiles (FORMAT=image/x-bil;bits=32).

   The result is not used as a layer of its own: [blend] resamples the base
   Copernicus tile onto the high-resolution grid and mixes the two, so the grid
   returned to the renderer *equals the base upsample* wherever RGE ALTI is
   missing or faded out. Everything downstream (relief bake, vertex LOD,
   fragment normals, POI anchoring) can therefore switch to it on a plain
   extent test and still degrade exactly to today's rendering at the edges, on
   a fetch failure, or outside France.

   The LIDAR HD rings, and level 13's in their place, can instead come from our
   own tiles of the same data ([Own_tiles], see [own_tiles_url]). *)

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
  | Own_tiles of { base_url : string; product : string; metres : float }
      (** Our own tiles of IGN's LIDAR HD, built by Corine/own_tiles.py in the
          CRS each territory's layers are stored in: 256 x 256 samples at
          [metres], tile (c, r) covering x from c to c + 1 times 256 * metres
          and y likewise, as HDT1 files at
          [base_url/crs/product/metres/c_r.hdt], [crs] the {!Projection}'s name
          in lower case. The block is the one [Wms_projected] would ask for,
          which is always a whole number of tiles; a missing tile is nodata. *)

type layer = {
  kind : kind;
  size : int; (* samples per side of the block *)
  fade_metres : float;
      (* width of the annulus over which the refinement is faded back into the
         coarser surface, both at the edge of the extent and around nodata. Held
         well inside the extent so that the renderer's hard switch at the
         boundary lands where the two agree. *)
}

(* Where a refinement's samples sit, as a map both ways between arcseconds from
   the anchor arcsecond and a fractional sample index. Rows count from the south,
   as everywhere else.

   Both directions are stored because both are wanted and neither is cheap to
   keep in step by hand: the mesh, the shadow pass, POI anchoring, visibility and
   the eye height all want an index from an offset, while [Blend_core] wants an
   offset from an index so it can compose with the frame of the surface beneath.
   Deriving one from the other in a single constructor is what stops them
   drifting.

   For a graticule-aligned grid this is exactly the [(offset - origin) /
   px_arcsec] it replaces: affine, with the off-diagonal terms zero. For a grid
   served in its own projected CRS no affine map is exact. Its axes are turned
   from north by that CRS's grid convergence -- 2.44 degrees at 6.36 E, 0.55 at
   La Reunion -- and over a block of kilometres the map also curves: a second of
   longitude shortens with latitude and the convergence changes with longitude.
   The best affine fit misplaces the corners of the 5 km ring by 1.1 m, so such
   a frame is a {!Warp} fitted to the projection itself, good to a fraction of a
   millimetre (see [plan]). *)
type grid = {
  epsg : string; (* the projected CRS *)
  gx0 : float; (* projected coordinates of the centre of sample (0, 0) *)
  gy0 : float;
  metres : float; (* projected metres per sample, both axes *)
}

type frame = {
  to_index : Warp.t;
  of_index : Warp.t;
  grid : grid option;
      (* the projected grid the samples lie on, for a block served in one: two
         such blocks in the same CRS map onto each other exactly by an affine
         map in projected metres, which [blend] uses instead of going out
         through one fitted frame and back in through the other *)
  arcsec_step : float;
      (* sample pitch in arcseconds: sets the mip level the mesh reads and how
         finely a sight line is walked, both of which are measured against the
         one-arcsecond base grid *)
  axis_x : float * float;
      (* one column step as an east/north displacement in metres. Not along east
         for a projected grid: its axes are turned by the CRS's grid
         convergence, which the relief bake has to undo or the ring's normals
         come out rotated against the surface around it. Orthogonal to [axis_y]
         on the ground, these projections being conformal. Taken at the centre
         of the block, where a curved frame's axes are the average of its
         own. *)
  axis_y : float * float;
  step_x_m : float; (* their lengths: metres on the ground per step *)
  step_y_m : float;
}

(* Ground metres per step along each axis, from the frame itself rather than
   passed in beside it, so a projected grid's scale factor -- its projected metre
   is not quite a ground metre -- is accounted for without anything special. The
   pitch in arcseconds is the area per sample, which picks no axis. *)
let make_frame ~to_index ~of_index ~grid ~size ~lat =
  let deltax, deltay, _ = Render_state.compute_deltas ~lat in
  let c = 0.5 *. float (size - 1) in
  let ui, uj, vi, vj = Warp.jacobian of_index c c in
  let ((ax, ay) as axis_x) = (ui *. deltax, vi *. deltay)
  and ((bx, by) as axis_y) = (uj *. deltax, vj *. deltay) in
  {
    to_index;
    of_index;
    grid;
    arcsec_step = sqrt (Float.abs ((ui *. vj) -. (uj *. vi)));
    axis_x;
    axis_y;
    step_x_m = Float.hypot ax ay;
    step_y_m = Float.hypot bx by;
  }

(* A graticule-aligned block: one scale for both axes, in arcseconds. *)
let geographic_frame ~px_arcsec ~origin_x ~origin_y ~size ~lat =
  let of_index =
    Affine.diagonal ~sx:px_arcsec ~sy:px_arcsec ~tx:origin_x ~ty:origin_y
  in
  make_frame ~of_index:(Warp.of_affine of_index)
    ~to_index:(Warp.of_affine (Affine.inverse of_index))
    ~grid:None ~size ~lat

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

(* Where our own tiles are hosted (an R2 bucket behind Cloudflare's cache), built
   by Corine/own_tiles.py; [None] would take every ring from IGN again.
   [?own=<base url>] overrides it, to try a tile set, and [?ign] keeps IGN's
   rings, to compare the two. *)
let own_tiles_url = Some "https://tiles.vouillon.org/v1"

(* Every ring is swapped for its own-tile counterpart, keeping its name, so the
   renderer is unchanged. Not in a worker, which has no location. *)
let own_base =
  try
    let params = Brr.Uri.query_params (Brr.Window.location Brr.G.window) in
    if Brr.Uri.Params.mem (Jstr.v "ign") params then None
    else
      match Brr.Uri.Params.find (Jstr.v "own") params with
      | Some url -> Some (Jstr.to_string url)
      | None -> own_tiles_url
  with _ -> None

let own_or layer ~product ~metres ~size ~fade_metres =
  match own_base with
  | None -> layer
  | Some base_url ->
      assert (size mod tile_px = 0);
      { kind = Own_tiles { base_url; product; metres }; size; fade_metres }

(* Tile [c, r] of an own product: [c] counts eastwards, [r] northwards. *)
let own_url ~base_url ~crs ~product ~metres c r =
  Printf.sprintf "%s/%s/%s/%g/%d_%d.hdt" base_url
    (String.lowercase_ascii (Projection.name crs))
    product metres c r

(* Level 13, 8 x 8 tiles: 0.309 arcseconds per sample (9.5 m north-south, a bit
   over three times the base grid), 2048 samples per side -- a power of two, so
   the relief pyramid keeps its usual shape -- spanning 633 arcseconds, i.e.
   19.5 km north-south and 13.6 km east-west at 46 degrees.

   Measured: 214 kB on the wire per tile (the service deflates the payload),
   64 tiles in ~4 s with browser-level parallelism over one HTTP/2
   connection. *)
let l13 =
  own_or
    (wmts_layer ~matrix_level:13 ~block_tiles:8 ~fade_metres:1500.)
    ~product:"ground" ~metres:8. ~size:2048 ~fade_metres:1500.

(* The two LIDAR HD rings, fetched in the CRS the product is stored in --
   Lambert-93 over metropolitan France, RGR92 / UTM 40S over La Reunion -- rather
   than in the WGS84G reprojection the service also offers. Measured (PLAN.md):
   the reprojection costs 4.29 degrees of normal error against a clean resample of
   IGN's own data, a woven corduroy that dominates the shading once the procedural
   detail is off, and it is absent from the native grid. It also makes the
   graticule-aligned +-1.22 x 0.87 km anisotropic grid a square one.

   [stem] is shared: one product, two resolutions of it, so the inner ring's fade
   onto the middle one hides no product difference. It is not an exact nesting,
   though. Measured 2026-11-16 against the 1 m output over the very boxes [plan]
   asks for (three places, two pieces each): the Lambert-93 5 m output lands
   1.00 m south of where it belongs, every time, and even shifted back differs
   from a 5 x 5 box mean of the 1 m data by 0.15-0.38 m RMS -- the service builds
   it with some other filter. Left uncorrected: a fifth of a sample, inside a
   300 m fade, and a fix on IGN's side would turn a correction here into the
   same error the other way. Over La Reunion the ring is served at 4 m (see
   [served_metres]), which was an exact 4 x 4 box mean of the 1 m output (on 1
   km boxes, not on [plan]'s). Only the middle ring's fade onto l13 crosses
   products, and it has 600 m of annulus for a 3 m worst case.

   The middle ring: 5 m, 1024 samples, so 5120 m per side (4 m and 4096 m over La
   Reunion). Sized by reach rather than by spacing: with the alignment rule below
   it covers at least 1920 m in every direction (1536 m over La Reunion), against
   the 1832 m worst case of the +-2.44 x 1.70 km ring it replaces, and what lies
   beyond it falls through to l13 at 9.5 m. 2 x 2 GetMaps of 512^2, 4.0 MB. *)
let lidar_5m =
  own_or
    (wms_projected_layer
       ~stem:"IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE" ~metres:5.0
       ~split:2 ~size:1024 ~fade_metres:600.)
    ~product:"ground" ~metres:4. ~size:1024 ~fade_metres:600.

(* The innermost ring: 1 m -- the product's own resolution, so no resampling of it
   happens anywhere -- 2048 samples, 2048 m per side against the +-1.22 x 0.87 km
   it replaces. 2 x 2 GetMaps of 1024^2, 16.0 MB (this endpoint does not
   compress).

   [fade_metres] is 300 rather than [l13]'s 1500: the extent is a sixteenth of the
   width and the surface underneath is the same product, so the annulus has
   little to hide here: the quantisation change, and the middle ring's own
   filtering and 1 m offset (see [lidar_5m]).

   RGE ALTI cannot serve this: below level 14 its WMS output is
   nearest-neighbour replication of the 4.77 m grid (100% of 4x4 blocks
   bit-identical), so LIDAR HD is the only real sub-5 m bare-earth source. Its
   water is unrectified, which does not matter because terrain.frag replaces the
   DEM normal with the procedural water normal wherever the cover map says
   water. *)
let lidar_1m =
  own_or
    (wms_projected_layer
       ~stem:"IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE" ~metres:1.0
       ~split:2 ~size:2048 ~fade_metres:300.)
    ~product:"ground" ~metres:1. ~size:2048 ~fade_metres:300.

(* LIDAR HD canopy height (MNH: surface minus ground, so buildings too), over
   the 1 m ring's own footprint: the same 2048 m span and the same alignment
   rule in [plan] put it on exactly that ring's block, so the renderer reads it
   through the ring's normalised coordinate with no frame of its own. At 2 m
   rather than 1: 4 MB on the wire instead of 16, still finer than a crown. *)
let canopy =
  own_or
    (wms_projected_layer
       ~stem:"IGNF_LIDAR-HD_MNH_ELEVATION.ELEVATIONGRIDCOVERAGE" ~metres:2.0
       ~split:2 ~size:1024 ~fade_metres:0.)
    ~product:"canopy" ~metres:2. ~size:1024 ~fade_metres:0.

(* The same canopy at 5 m over the 5 m ring's block (5120 m, reaching at least
   1920 m every way), read through that ring's coordinate: where forest stands
   beyond [canopy]'s block, in place of land cover drawn to 25 ha. Too coarse
   for crowns. *)
let canopy_far =
  own_or
    (wms_projected_layer
       ~stem:"IGNF_LIDAR-HD_MNH_ELEVATION.ELEVATIONGRIDCOVERAGE" ~metres:5.0
       ~split:2 ~size:1024 ~fade_metres:0.)
    ~product:"canopy" ~metres:4. ~size:1024 ~fade_metres:0.

(* And at 20 m over 20 km, with a frame of its own (no ring shares its
   block): where forest stands out to ~10 km. The service serves the coarser
   grid close to a box mean of the finer one (0.66 m from the mean of the 2 m
   samples, 1.4 m from any one of them, measured 2026-11-10), so a sparse
   forest still reads as a low mean height -- what the EEA's tree cover
   misses. *)
let canopy_mid =
  own_or
    (wms_projected_layer
       ~stem:"IGNF_LIDAR-HD_MNH_ELEVATION.ELEVATIONGRIDCOVERAGE" ~metres:20.0
       ~split:2 ~size:1024 ~fade_metres:0.)
    ~product:"canopy" ~metres:16. ~size:1024 ~fade_metres:0.

(* The spacing actually requested for a projected ring. Over La Reunion the
   service resamples from overviews at powers of two (1, 2, 4 m...), and at a
   ratio of 1.25 to one of them -- 5 m from 4, 2.5 from 2 -- its output beats
   into a grid with a 4-pixel period: 20 m at 5 m, a waffle of lumps over every
   slope. IGN's 1 m data, and a box mean of it to 5 m, carry none of it; 4 m
   and 2 m output carry none either (measured 2026-11-10 near -20.993, 55.390,
   second differences correlated at lag 4: 0.18-0.25 at 5 m, 0.01 at 4 m).
   Lambert-93 at 5 m is clean, so only RGR92 / UTM 40S rounds down to a power
   of two -- 4 m for the middle ring, which then spans 4096 m instead of 5120,
   still reaching 1536 m in every direction. *)
let served_metres crs metres =
  if Projection.epsg crs = "EPSG:2975" then
    Float.pow 2. (Float.floor (Float.log2 metres +. 1e-9))
  else metres

(* How long a block waits for its next tile once it holds one. Expiring is not
   an error: the block is published with whatever has arrived by then -- the
   tiles still in flight are nodata, which the blend fades back into the base
   like any other gap, and the block comes again as its [rest] if they
   arrive after all. A block without a single tile has nothing to publish
   and goes on waiting, the location being on screen without it meanwhile: the
   service answers a cold request after 15 to 30 s, all its tiles at once, and
   giving up on it left the ring out -- and the trees with the 1 m one --
   until the block changed. It falls back to the base alone once every request
   has failed. *)
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

(* The tile corner of a matrix level nearest the location, on which a WMTS block
   is centred, as the row and column of the tile south-east of it. Anchoring on a
   *corner* rather than on the location makes every location within one tile's
   width of it share the same URLs, which is what makes the service worker's
   cache-first rule worth having. The nearest corner rather than the tile holding
   the location keeps the location within half a tile of the block's centre:
   centred on the tile's north-west corner instead, a block of 8 reached only 3
   tiles south and east in the worst case (5.1 km east-west at level 13), short
   of where the mesh stops resolving finer than the base grid. *)
let anchor_corner ~tiles_per_axis ~lat ~lon =
  let alat = Web_utils.arcsec_floor lat and alon = Web_utils.arcsec_floor lon in
  let span = 360. /. float tiles_per_axis in
  let row =
    int_of_float (Float.round ((90. -. (float alat /. 3600.)) /. span))
  in
  let col =
    int_of_float (Float.round (((float alon /. 3600.) +. 180.) /. span))
  in
  (alat, alon, row, col)

(* One fetch feeding a [px] x [px] square of the block at [dst_row]/[dst_col].
   Both kinds reduce to a list of these, so the request loop, the deadline and
   the partial-block handling below are written once. *)
type request = {
  url : string;
  px : int;
  dst_row : int;
  dst_col : int;
  hdt : bool; (* one of our own tiles, not raw float32 *)
}

(* The block of a ring served on a projected grid -- its frame, and the
   projected corner and span it covers -- shared by the WMS and our own
   tiles. *)
let projected_block layer crs ~metres ~lat ~lon =
  let alat = Web_utils.arcsec_floor lat and alon = Web_utils.arcsec_floor lon in
  let anchor_lat = float alat /. 3600. and anchor_lon = float alon /. 3600. in
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
  let x0 = corner xa -. (span /. 2.) and y0 = corner ya -. (span /. 2.) in
  (* The frame is fitted to the projection itself: a 9 x 9 lattice of
     offsets over the block, each projected exactly, fits a [Warp] each
     way. Only where the lattice goes is taken from the local derivative
     at the anchor -- a rotation and a scale, which places the corners to
     within a couple of metres, plenty for choosing where to sample but
     not for the frame: measured, the quadratic fit is good to 0.27 mm
     over the 5 m ring and 0.02 mm over the 1 m one, where the best
     affine map is out by 1.1 m and 0.18 m at the corners. *)
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
  let gx0 = x0 +. (0.5 *. metres) and gy0 = y0 +. (0.5 *. metres) in
  let estimate =
    Affine.compose
      (Affine.inverse per_arcsec)
      (Affine.diagonal ~sx:metres ~sy:metres ~tx:(gx0 -. xa) ~ty:(gy0 -. ya))
  in
  let samples =
    let last = float (layer.size - 1) in
    let lattice = List.init 9 (fun k -> (float k /. 8. *. 1.1) -. 0.05) in
    Array.of_list
      (List.concat_map
         (fun s ->
           List.map
             (fun t ->
               let u, v = Affine.apply estimate (s *. last) (t *. last) in
               let x, y =
                 Projection.forward crs
                   ~lat:(anchor_lat +. (v /. 3600.))
                   ~lon:(anchor_lon +. (u /. 3600.))
               in
               (u, v, (x -. gx0) /. metres, (y -. gy0) /. metres))
             lattice)
         lattice)
  in
  let to_index = Warp.fit samples in
  let of_index =
    Warp.fit (Array.map (fun (u, v, i, j) -> (i, j, u, v)) samples)
  in
  ( make_frame ~to_index ~of_index
      ~grid:(Some { epsg = Projection.epsg crs; gx0; gy0; metres })
      ~size:layer.size ~lat,
    x0,
    y0,
    span )

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
        anchor_corner ~tiles_per_axis ~lat ~lon
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
                  hdt = false;
                })
              (List.init block_tiles Fun.id))
          (List.init block_tiles Fun.id)
      in
      Some
        ( geographic_frame ~px_arcsec ~origin_x ~origin_y ~size:layer.size ~lat,
          reqs )
  | Wms_projected { stem; metres; split } -> (
      match Projection.of_location ~lat ~lon with
      | None -> None
      | Some crs ->
          let metres = served_metres crs metres in
          let frame, x0, y0, span =
            projected_block layer crs ~metres ~lat ~lon
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
                      hdt = false;
                    })
                  (List.init split Fun.id))
              (List.init split Fun.id)
          in
          Some (frame, reqs))
  | Own_tiles { base_url; product; metres } -> (
      match Projection.of_location ~lat ~lon with
      | Some crs ->
          let frame, x0, y0, _ = projected_block layer crs ~metres ~lat ~lon in
          let n = layer.size / tile_px in
          let tile_m = float tile_px *. metres in
          let c0 = int_of_float (Float.round (x0 /. tile_m))
          and r0 = int_of_float (Float.round (y0 /. tile_m)) in
          let reqs =
            List.concat_map
              (fun tr ->
                List.map
                  (fun tc ->
                    (* Raster row 0 is the northernmost, so the top band is
                       the highest tile row. *)
                    {
                      url =
                        own_url ~base_url ~crs ~product ~metres (c0 + tc)
                          (r0 + n - 1 - tr);
                      px = tile_px;
                      dst_row = tr * tile_px;
                      dst_col = tc * tile_px;
                      hdt = true;
                    })
                  (List.init n Fun.id))
              (List.init n Fun.id)
          in
          Some (frame, reqs)
      | _ -> None)

(* What [fetch] would ask for, as one string. Blocks are anchored on a tile or on
   a grid corner rather than on the location, so two nearby places usually plan
   the very same requests; when they do, they are asking for the same piece of
   ground, and a grid already blended for one of them serves the other unchanged
   (see [reanchor]). Equal ids mean equal request lists by construction -- the id
   *is* the request list -- so this cannot drift from [plan]. A place no CRS of
   the layer covers has no block and the empty id: there is nothing to fetch
   there, and nothing to keep. *)
let block_id layer ~lat ~lon =
  match plan layer ~lat ~lon with
  | Some (_, reqs) -> String.concat "\n" (List.map (fun r -> r.url) reqs)
  | None -> ""

type raw = {
  layer : layer;
  samples : (float, float32_elt, c_layout) Array1.t;
      (* [layer.size] x [layer.size], row 0 northernmost *)
  frame : frame;
  missing : int;
  rest : raw option Lwt.t;
      (* for a block handed out before all its requests had settled: the
         block again with the tiles that came in since, once they all have,
         if any did *)
}

let raw_samples (r : raw) = r.samples
let raw_frame (r : raw) = r.frame
let raw_rest (r : raw) = r.rest

(* One tile. A tile fully outside RGE ALTI coverage answers 404 with an
   exception report; that -- like any other per-tile failure -- just leaves its
   region at nodata, which the blend turns back into the base. *)
exception Outside_coverage

(* One of our own tiles (HDT1: the .dem layout with a scale of its own, see
   [Own_tiles]) to float32 heights, nodata as -99999. The residuals are undone
   by a worker, as for a base tile; the scale is read here first, the buffer
   being transferred to the worker. *)
let decode_hdt buf px =
  let dv = Brr.Tarray.Data_view.of_buffer buf in
  let u8 i = Brr.Tarray.Data_view.get_uint8 dv i in
  if
    Brr.Tarray.Buffer.byte_length buf < 28
    || u8 0 <> Char.code 'H'
    || u8 1 <> Char.code 'D'
    || u8 2 <> Char.code 'T'
    || u8 3 <> Char.code '1'
    || Int32.to_int (Brr.Tarray.Data_view.get_uint32_le dv 4) <> px
    || Int32.to_int (Brr.Tarray.Data_view.get_uint32_le dv 8) <> px
  then Lwt.fail (Failure "not an HDT1 tile of the expected size")
  else
    let base = Brr.Tarray.Data_view.get_float32_le dv 12
    and step = Brr.Tarray.Data_view.get_float32_le dv 16 in
    let* w = Worker_pool.acquire () in
    let* res =
      Lwt.finalize
        (fun () ->
          Worker_pool.post w
            (Worker_pool.Decode (Worker_pool.DEM (Brr.Tarray.Buffer.to_jv buf))))
        (fun () ->
          Worker_pool.release w;
          Lwt.return_unit)
    in
    match res with
    | Worker_pool.ResultDEM { heights } ->
        let out = Brr.Tarray.create Brr.Tarray.Float32 (px * px) in
        let o = Brr.Tarray.to_bigarray1 out in
        for i = 0 to (px * px) - 1 do
          let u =
            Array1.unsafe_get heights (2 * i)
            lor (Array1.unsafe_get heights ((2 * i) + 1) lsl 8)
          in
          Array1.unsafe_set o i
            (if u = 65535 then -99999. else base +. (step *. float u))
        done;
        Lwt.return out
    | Worker_pool.ResultCLC _ ->
        Lwt.fail (Failure "HDT1 tile: unexpected reply")
    | Worker_pool.Error e -> Lwt.fail (Failure e)

let rec fetch_one ?(retries = [ 3.; 6. ]) req =
  let open Brr_io.Fetch in
  (* A request that could not be made, or was refused for a reason that is
     not about it, is made again after a few seconds: the service answers a
     valid request with a 400 now and then (it allows 40 a second, and is
     said to refuse all for 5 s past that), and a tile given up on is a hole
     in its block for as long as the block is kept. *)
  let again e =
    match retries with
    | [] -> Lwt.fail e
    (* Not offline, where nothing will come and the load is waiting. *)
    | _ :: _
      when not (Jv.to_bool (Jv.get (Jv.get Jv.global "navigator") "onLine")) ->
        Lwt.fail e
    | s :: retries ->
        let t, u = Lwt.task () in
        ignore
          (Brr.G.set_timeout
             ~ms:(truncate (s *. 1000.))
             (fun () -> Lwt.wakeup u ()));
        let* () = t in
        fetch_one ~retries req
  in
  let* res = to_lwt (url (Jstr.v req.url)) in
  match res with
  | Error e -> again (Failure (Jstr.to_string (Jv.Error.message e)))
  | Ok resp when Response.status resp = 404 -> Lwt.fail Outside_coverage
  | Ok resp
    when let s = Response.status resp in
         s = 400 || s = 429 || s >= 500 ->
      again (Failure (Printf.sprintf "HTTP %d" (Response.status resp)))
  | Ok resp when not (Response.ok resp) ->
      Lwt.fail (Failure (Printf.sprintf "HTTP %d" (Response.status resp)))
  | Ok resp -> (
      let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
      match buf with
      | Error e -> Lwt.fail (Failure (Jstr.to_string (Jv.Error.message e)))
      | Ok buf when req.hdt -> decode_hdt buf req.px
      | Ok buf ->
          let expected = req.px * req.px * 4 in
          if Brr.Tarray.Buffer.byte_length buf <> expected then
            Lwt.fail
              (Failure
                 (Printf.sprintf "expected %d bytes, got %d" expected
                    (Brr.Tarray.Buffer.byte_length buf)))
          else Lwt.return (Brr.Tarray.of_buffer Brr.Tarray.Float32 buf))

(* A layer, for the console. *)
let describe layer =
  match layer.kind with
  | Wmts { matrix_level; _ } -> Printf.sprintf "RGE ALTI level %d" matrix_level
  | Wms_projected { stem; metres; _ } ->
      Printf.sprintf "LIDAR HD %s at %g m"
        (if String.starts_with ~prefix:"IGNF_LIDAR-HD_MNH" stem then "canopy"
         else "ground")
        metres
  | Own_tiles { product; metres; _ } ->
      Printf.sprintf "Own %s at %g m" product metres

(* Fetch only: the blend needs the base tile, which is being fetched in
   parallel. Never fails: [None] means "render from the base tile alone". *)
let fetch ?(timeout_s = timeout_s) layer ~lat ~lon : raw option Lwt.t =
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
     blitted into it: the raw block is published by reference, and the late
     write would land in a buffer the caller has already started reading. It
     goes into a copy, [late], handed out in turn as the block's [rest] once
     every request has settled. *)
      let published = ref false in
      let late = ref None in
      let rest, set_rest = Lwt.wait () in
      let t0 = Brr.(Performance.now_ms G.performance) in
      let last = ref t0 in
      (* Every request is issued at once: the browser multiplexes them over one
     HTTP/2 connection. *)
      let tasks =
        List.map
          (fun req ->
            Lwt.catch
              (fun () ->
                let* tile = fetch_one req in
                let full =
                  if not !published then full
                  else
                    match !late with
                    | Some copy -> copy
                    | None ->
                        let copy =
                          Brr.Tarray.of_jv
                            (Jv.call (Brr.Tarray.to_jv full) "slice" [||])
                        in
                        late := Some copy;
                        copy
                in
                begin
                  (* Blit row by row: the response is contiguous, the destination is
                 strided by the width of the whole block. *)
                  for i = 0 to req.px - 1 do
                    Brr.Tarray.set_tarray full
                      ~dst:(((req.dst_row + i) * size) + req.dst_col)
                      (Brr.Tarray.sub tile ~start:(i * req.px)
                         ~stop:((i + 1) * req.px))
                  done;
                  (last := Brr.(Performance.now_ms G.performance));
                  incr arrived
                end;
                Lwt.return ())
              (fun e ->
                (* Otherwise a failed tile only shows as a hole in the data.
                   Not the 404 of a tile outside coverage, which is expected. *)
                (match e with
                | Outside_coverage -> ()
                | e ->
                    Format.eprintf "High-resolution tile failed: %s (%s)@."
                      (match e with
                      | Failure m -> m
                      | e -> Printexc.to_string e)
                      req.url);
                Lwt.return ()))
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
              rest;
            }
      in
      let all =
        let* () = Lwt.join tasks in
        (* The timeout got there first; [Lwt.choose] discards this. *)
        if !published then begin
          (match !late with
          | None -> Lwt.wakeup_later set_rest None
          | Some copy ->
              Format.eprintf "%s: %d/%d tiles in %.0f ms, past the timeout@."
                (describe layer) !arrived total
                (Brr.(Performance.now_ms G.performance) -. t0);
              Lwt.wakeup_later set_rest
                (Some
                   {
                     layer;
                     samples = Brr.Tarray.to_bigarray1 copy;
                     frame;
                     missing = total - !arrived;
                     rest = Lwt.return None;
                   }));
          Lwt.return None
        end
        else begin
          Lwt.wakeup_later set_rest None;
          if !arrived = 0 then
            Brr.Console.error
              [
                Jstr.v
                  (Printf.sprintf "%s: no tile could be fetched"
                     (describe layer));
              ]
          else
            Format.eprintf "%s: %d/%d tiles in %.0f ms@." (describe layer)
              !arrived total
              (Brr.(Performance.now_ms G.performance) -. t0);
          Lwt.return (publish ())
        end
      in
      let timeout =
        let t, w = Lwt.task () in
        let ceiling = timeout_s *. 1000. in
        let rec arm ms =
          ignore
            (Brr.G.set_timeout ~ms:(truncate ms) (fun () ->
                 if not !published then begin
                   let now = Brr.(Performance.now_ms G.performance) in
                   let idle = now -. !last in
                   if !arrived = 0 then begin
                     if now -. t0 < 1.5 *. ceiling then
                       Format.eprintf
                         "%s: no tile after %.0f s, still waiting@."
                         (describe layer) timeout_s;
                     arm ceiling
                   end
                   else if idle < ceiling -. 1. then arm (ceiling -. idle)
                   else begin
                     Format.eprintf
                       "%s: timed out with %d/%d tiles after %.0f ms@."
                       (describe layer) !arrived total (now -. t0);
                     Lwt.wakeup_later w (publish ())
                   end
                 end))
        in
        arm ceiling;
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

(* The same grid, seen from another anchor arcsecond. The samples are absolute
   ground -- a block of it, decided by [block_id] and not by where the observer
   stands -- and only the frame's translation is stated relative to the anchor,
   so a grid
   blended for one location is exact for any other holding the same block. Both
   anchors are in arcseconds, latitude first, as [Web_utils.arcsec_floor] gives
   them. *)
let reanchor (t : t) ~old_anchor:(old_lat, old_lon) ~anchor:(alat, alon) =
  (* A point [p] arcseconds from the new anchor is [p + d] from the old one, and
     both translations are exact: a projected block's frame is fitted to the
     projection itself, so stated from either anchor it is the same map. *)
  let dx = float (alon - old_lon) and dy = float (alat - old_lat) in
  {
    t with
    frame =
      {
        t.frame with
        to_index = Warp.translate_input t.frame.to_index ~du:dx ~dv:dy;
        of_index = Warp.translate_output t.frame.of_index ~dx:(-.dx) ~dy:(-.dy);
      };
  }

(* The surface a refinement is mixed into: any u16 grid with a known origin and
   spacing, so the layers can be chained (base -> L13 -> finer). The fields are
   prefixed because [t] and [raw] carry same-named ones and OCaml resolves record
   fields by the last type defined -- an unprefixed [origin_x] here would silently
   pick the wrong record.

   Rows count from the south on both sides, matching [Dem_loader]. *)
type source = {
  src_grid : Dem_loader.t;
  src_to_index : Warp.t;
      (* arcseconds from the anchor to a fractional sample index of this grid,
         the same map a [frame] carries *)
  src_proj : grid option; (* and the projected grid it lies on, if any *)
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
    src_to_index =
      Warp.of_affine
        (Affine.inverse (Affine.diagonal ~sx:1.0 ~sy:1.0 ~tx:o ~ty:o));
    src_proj = None;
    src_height_scale = base_height_scale;
    src_height_offset = base_height_offset;
  }

(* A blended grid used in turn as the surface for a finer layer. *)
let as_source (t : t) =
  {
    src_grid = t.grid;
    src_to_index = t.frame.to_index;
    src_proj = t.frame.grid;
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

(* Refinement index to source index. Two grids in one projected CRS share axes,
   and in projected metres the map between them is exactly a scale and an
   offset, so it is built there rather than out through one fitted frame and
   back in through the other -- which would only approximate it, and lose
   [Blend_core.axis_aligned] to the residue. Otherwise out to arcseconds through
   the refinement's own frame and back in through the source's: exact for two
   graticule grids, both affine; curved where a projected ring meets the
   graticule surface beneath it, the one blend that goes through
   [Blend_core]'s general path. *)
let refinement_to_source (source : source) (raw : raw) =
  match (raw.frame.grid, source.src_proj) with
  | Some r, Some s when r.epsg = s.epsg ->
      Warp.of_affine
        (Affine.diagonal ~sx:(r.metres /. s.metres) ~sy:(r.metres /. s.metres)
           ~tx:((r.gx0 -. s.gx0) /. s.metres)
           ~ty:((r.gy0 -. s.gy0) /. s.metres))
  | _ -> Warp.compose source.src_to_index raw.frame.of_index

let blend ~lat:_ ~(source : source) (raw : raw) =
  let { size; fade_metres; _ } = raw.layer in
  let t0 = Brr.(Performance.now_ms G.performance) in
  let p =
    {
      Blend_core.size;
      to_src = refinement_to_source source raw;
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
        "%s blended in %.0f ms (missing tiles: %d, %.0f m range, %.2f cm \
         steps)@."
        (describe raw.layer)
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

(* Fetch, [parallel] at a time, every one of [urls] that the persistent data
   cache does not hold yet, and say how many that was.

   The requests are made through the service worker, which stores every
   response in the persistent data cache -- a 404 included, since it means
   "no data there" and is served back rather than asked again. [Cache.add]
   would reject those, hence fetches rather than the direct cache writes of
   [Dem_loader.prefetch]. *)
let warm_cache ~parallel urls =
  let* cache =
    to_lwt
      (Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ())
         (Jstr.v Data_cache.name))
  in
  match cache with
  | Error _ -> Lwt.return 0
  | Ok cache ->
      let fetched = ref 0 and queue = ref urls in
      let rec worker () =
        match !queue with
        | [] -> Lwt.return_unit
        | url :: rest ->
            queue := rest;
            let* () =
              Lwt.catch
                (fun () ->
                  let url = Jstr.v url in
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
                (fun _ -> Lwt.return_unit)
            in
            worker ()
      in
      let* () = Lwt.join (List.init (max 1 parallel) (fun _ -> worker ())) in
      Lwt.return !fetched

(* How far around the start our own tiles are warmed, each way: a drive of half
   an hour to three quarters in the mountains. For the 8 m ground that is ~900
   tiles, ~60 MB in the Ubaye, and every location within ~22 km keeps its whole
   ring offline; the 16 m canopy adds ~250 tiles, ~10 MB. *)
let own_prefetch_reach = 30_000.

(* Warm the persistent cache for offline use, like [Dem_loader.prefetch].

   For [Wmts], a block twice the extent's width centred on the same anchor gives
   every location within [block_tiles / 2] tiles (~10 km) of [lat]/[lon] its
   full extent offline, for ~40 MB on the wire. The geoplateforme exempts the
   WMTS from its rate limiting, so all the requests are issued at once, like the
   extent's own.

   For [Own_tiles], every tile within [own_prefetch_reach] of [lat]/[lon]: a
   third of the bytes per square kilometre of the WMTS, so three times the area
   for the same download. Sixteen at a time, so as not to crowd out whatever the
   view asks for next. *)
let prefetch layer ~lat ~lon =
  match layer.kind with
  (* WMS blocks are deliberately not prefetched: 4 MB buys ~600 m of offline
     roaming against the ~40 MB that buys ~10 km for [l13], which is the wrong
     trade. Their absence offline is invisible -- the blend falls back to the
     coarser surface. *)
  | Wms_projected _ -> Lwt.return_unit
  | Wmts { matrix_level; tiles_per_axis; block_tiles; _ } ->
      let _, _, anchor_row, anchor_col =
        anchor_corner ~tiles_per_axis ~lat ~lon
      in
      let n = 2 * block_tiles in
      let row0 = anchor_row - (n / 2) and col0 = anchor_col - (n / 2) in
      let urls =
        List.concat_map
          (fun br ->
            List.map
              (fun bc ->
                wmts_url ~matrix_level ~row:(row0 + br) ~col:(col0 + bc))
              (List.init n Fun.id))
          (List.init n Fun.id)
      in
      let* fetched = warm_cache ~parallel:(List.length urls) urls in
      if fetched > 0 then
        Format.eprintf "RGE ALTI: prefetched %d tiles@." fetched;
      Lwt.return_unit
  | Own_tiles { base_url; product; metres } -> (
      match Projection.of_location ~lat ~lon with
      | Some crs ->
          let x, y = Projection.forward crs ~lat ~lon in
          let tile_m = float tile_px *. metres in
          let index v = int_of_float (Float.floor (v /. tile_m)) in
          let range v =
            let lo = index (v -. own_prefetch_reach) in
            List.init (index (v +. own_prefetch_reach) - lo + 1) (( + ) lo)
          in
          let urls =
            List.concat_map
              (fun r ->
                List.map
                  (fun c -> own_url ~base_url ~crs ~product ~metres c r)
                  (range x))
              (range y)
          in
          let* fetched = warm_cache ~parallel:16 urls in
          if fetched > 0 then
            Format.eprintf "%s: prefetched %d of %d tiles@." (describe layer)
              fetched (List.length urls);
          Lwt.return_unit
      | _ -> Lwt.return_unit)
