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

(* Anything below this is IGN's nodata sentinel (-99999 outside coverage). *)
let nodata_limit = -500.

(* One refinement ring. Several can be nested (see PLAN.md), so nothing here is
   a module-level constant any more: every consumer reads the spacing and the
   sample count off the layer that produced the grid it is holding.

   [px_arcsec] and [size] are derived, never passed in, so they cannot drift
   from [matrix_level] and [block_tiles]. The spacing is *not* a power-of-two
   ratio to the 1-arcsecond base grid, which is why consumers must read it
   rather than assume one. *)
type layer = {
  matrix_level : int;
  tiles_per_axis : int;
  block_tiles : int;
  px_arcsec : float; (* sample spacing *)
  size : int; (* samples per side of the block *)
  fade_metres : float;
      (* width of the annulus over which the refinement is faded back into the
         coarser surface, both at the edge of the extent and around nodata. Held
         well inside the extent so that the renderer's hard switch at the
         boundary lands where the two agree. *)
}

let wmts_layer ~matrix_level ~block_tiles ~fade_metres =
  (* Level L has 2^(L+1) tiles across 360 degrees: level 0 is two tiles wide. *)
  let tiles_per_axis = 1 lsl (matrix_level + 1) in
  {
    matrix_level;
    tiles_per_axis;
    block_tiles;
    px_arcsec = 360. /. float tiles_per_axis /. float tile_px *. 3600.;
    size = tile_px * block_tiles;
    fade_metres;
  }

(* Level 13, 8 x 8 tiles: 0.309 arcseconds per sample (9.5 m north-south, a bit
   over three times the base grid), 2048 samples per side -- a power of two, so
   the relief pyramid keeps its usual shape -- spanning 633 arcseconds, i.e.
   19.5 km north-south and 13.6 km east-west at 46 degrees.

   Measured: 214 kB on the wire per tile (the service deflates the payload),
   64 tiles in ~4 s with browser-level parallelism over one HTTP/2
   connection. *)
let l13 = wmts_layer ~matrix_level:13 ~block_tiles:8 ~fade_metres:1500.

(* Ceiling on the whole set of tile requests. The location cannot be published
   before this resolves, so it is also the worst-case load-time penalty;
   expiring is not an error, the block is published with whatever has arrived by
   then -- the tiles still in flight are nodata, which the blend fades back into
   the base like any other gap. Only a block without a single tile falls back to
   the base alone. *)
let timeout_s = 25.

(* u16 quantisation shared with the .dem pipeline (Corine/compress_dem.ml),
   [Dem_loader.get_height] and HEIGHT_SCALE in the shaders. *)
let u16_of_metres h = (h +. 500.) *. (65535. /. 9500.)

let tile_url layer ~row ~col =
  Printf.sprintf
    "https://data.geopf.fr/wmts?SERVICE=WMTS&VERSION=1.0.0&REQUEST=GetTile&LAYER=ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES&STYLE=normal&FORMAT=image/x-bil;bits=32&TILEMATRIXSET=WGS84G&TILEMATRIX=%d&TILEROW=%d&TILECOL=%d"
    layer.matrix_level row col

let to_lwt f =
  let t, u = Lwt.task () in
  (Fut.await f @@ fun v -> Lwt.wakeup u v);
  t

(* The tile of [layer]'s matrix holding the location, on which the block is
   centred. Anchoring on the *tile* rather than on the location makes every
   location within one tile share the same URLs, which is what makes the service
   worker's cache-first rule worth having. *)
let anchor_tile layer ~lat ~lon =
  let alat = Web_utils.arcsec_floor lat and alon = Web_utils.arcsec_floor lon in
  let span = 360. /. float layer.tiles_per_axis in
  let row = int_of_float (floor ((90. -. (float alat /. 3600.)) /. span)) in
  let col = int_of_float (floor (((float alon /. 3600.) +. 180.) /. span)) in
  (alat, alon, row, col)

type raw = {
  layer : layer;
  samples : (float, float32_elt, c_layout) Array1.t;
      (* [layer.size] x [layer.size], row 0 northernmost *)
  origin_x : float; (* arcseconds from the anchor to sample column 0 *)
  origin_y : float; (* arcseconds from the anchor to the southernmost row *)
  missing : int;
}

(* One tile. A tile fully outside RGE ALTI coverage answers 404 with an
   exception report; that -- like any other per-tile failure -- just leaves its
   region at nodata, which the blend turns back into the base. *)
let fetch_tile layer ~row ~col =
  let open Brr_io.Fetch in
  let* res = to_lwt (url (Jstr.v (tile_url layer ~row ~col))) in
  match res with
  | Error e -> Lwt.fail (Failure (Jstr.to_string (Jv.Error.message e)))
  | Ok resp when not (Response.ok resp) ->
      Lwt.fail (Failure (Printf.sprintf "HTTP %d" (Response.status resp)))
  | Ok resp -> (
      let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
      match buf with
      | Error e -> Lwt.fail (Failure (Jstr.to_string (Jv.Error.message e)))
      | Ok buf ->
          let expected = tile_px * tile_px * 4 in
          if Brr.Tarray.Buffer.byte_length buf <> expected then
            Lwt.fail
              (Failure
                 (Printf.sprintf "expected %d bytes, got %d" expected
                    (Brr.Tarray.Buffer.byte_length buf)))
          else Lwt.return (Brr.Tarray.of_buffer Brr.Tarray.Float32 buf))

(* Fetch only: the blend needs the base tile, which is being fetched in
   parallel. Never fails: [None] means "render from the base tile alone". *)
let fetch layer ~lat ~lon : raw option Lwt.t =
  let { block_tiles; px_arcsec; size; _ } = layer in
  let alat, alon, anchor_row, anchor_col = anchor_tile layer ~lat ~lon in
  let row0 = anchor_row - (block_tiles / 2) in
  let col0 = anchor_col - (block_tiles / 2) in
  (* Sample centres: lon = -180 + (col * tile_px + j + 0.5) * px,
     lat = 90 - (row * tile_px + i + 0.5) * px. *)
  let origin_x =
    -648000. +. ((float (col0 * tile_px) +. 0.5) *. px_arcsec) -. float alon
  in
  let origin_y =
    324000.
    -. ((float ((row0 * tile_px) + size - 1) +. 0.5) *. px_arcsec)
    -. float alat
  in
  let full = Brr.Tarray.create Brr.Tarray.Float32 (size * size) in
  Brr.Tarray.fill (-99999.) full;
  let total = block_tiles * block_tiles in
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
    List.concat_map
      (fun br ->
        List.map
          (fun bc ->
            Lwt.catch
              (fun () ->
                let* tile =
                  fetch_tile layer ~row:(row0 + br) ~col:(col0 + bc)
                in
                if not !published then begin
                  (* Blit row by row: the tile is contiguous, the destination is
                     strided by the width of the whole block. *)
                  for i = 0 to tile_px - 1 do
                    Brr.Tarray.set_tarray full
                      ~dst:((((br * tile_px) + i) * size) + (bc * tile_px))
                      (Brr.Tarray.sub tile ~start:(i * tile_px)
                         ~stop:((i + 1) * tile_px))
                  done;
                  incr arrived
                end;
                Lwt.return ())
              (fun _ -> Lwt.return ()))
          (List.init block_tiles Fun.id))
      (List.init block_tiles Fun.id)
  in
  let publish () =
    published := true;
    if !arrived = 0 then None
    else
      Some
        {
          layer;
          samples = Brr.Tarray.to_bigarray1 full;
          origin_x;
          origin_y;
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
                 "RGE ALTI: timed out with %d/%d tiles after %.0f ms@." !arrived
                 total
                 (Brr.(Performance.now_ms G.performance) -. t0);
             Lwt.wakeup_later w (publish ())
           end));
    t
  in
  Lwt.choose [ all; timeout ]

(* Chessboard distance in texels to the nearest nodata sample, saturated at
   255: two sweeps over the raster. Only built when the patch actually holds
   nodata, which means a location near the edge of French coverage. *)
let nodata_distance ~size (src : (float, float32_elt, c_layout) Array1.t) =
  let d = Array1.create int8_unsigned c_layout (size * size) in
  for i = 0 to size - 1 do
    let row = i * size in
    for j = 0 to size - 1 do
      let v =
        if Array1.unsafe_get src (row + j) < nodata_limit then 0
        else begin
          let m = ref 255 in
          let consider p = if p < !m then m := p in
          if i > 0 then begin
            consider (Array1.unsafe_get d (row - size + j));
            if j > 0 then consider (Array1.unsafe_get d (row - size + j - 1));
            if j < size - 1 then
              consider (Array1.unsafe_get d (row - size + j + 1))
          end;
          if j > 0 then consider (Array1.unsafe_get d (row + j - 1));
          if !m >= 255 then 255 else !m + 1
        end
      in
      Array1.unsafe_set d (row + j) v
    done
  done;
  for i = size - 1 downto 0 do
    let row = i * size in
    for j = size - 1 downto 0 do
      let m = ref (Array1.unsafe_get d (row + j)) in
      if !m > 0 then begin
        let consider p = if p + 1 < !m then m := p + 1 in
        if i < size - 1 then begin
          consider (Array1.unsafe_get d (row + size + j));
          if j > 0 then consider (Array1.unsafe_get d (row + size + j - 1));
          if j < size - 1 then
            consider (Array1.unsafe_get d (row + size + j + 1))
        end;
        if j < size - 1 then consider (Array1.unsafe_get d (row + j + 1));
        Array1.unsafe_set d (row + j) !m
      end
    done
  done;
  d

let smoothstep t =
  if t <= 0. then 0. else if t >= 1. then 1. else t *. t *. (3. -. (2. *. t))

type t = {
  layer : layer;
  grid : Dem_loader.t;
  origin_x : float;
  origin_y : float;
}

(* The surface a refinement is mixed into: any u16 grid with a known origin and
   spacing, so the layers can be chained (base -> L13 -> finer). The fields are
   prefixed because [t] and [raw] carry same-named ones and OCaml resolves record
   fields by the last type defined -- an unprefixed [origin_x] here would silently
   pick the wrong record.

   Rows count from the south on both sides, matching [Dem_loader]. *)
type source = {
  src_grid : Dem_loader.t;
  src_origin_x : float; (* arcseconds from the anchor to sample column 0 *)
  src_origin_y : float; (* arcseconds from the anchor to the southernmost row *)
  src_px_arcsec : float;
}

(* The base DEM: one arcsecond per sample, with the anchor arcsecond at index
   [size / 2] on both axes (see [Dem_loader.load]), i.e. sample 0 sits half the
   tile west and south of it. *)
let base_source (grid : Dem_loader.t) =
  let o = -.float (grid.Dem_loader.size / 2) in
  { src_grid = grid; src_origin_x = o; src_origin_y = o; src_px_arcsec = 1.0 }

(* A blended grid used in turn as the surface for a finer layer. *)
let as_source (t : t) =
  {
    src_grid = t.grid;
    src_origin_x = t.origin_x;
    src_origin_y = t.origin_y;
    src_px_arcsec = t.layer.px_arcsec;
  }

(* Build the blended grid: [source] resampled onto the refinement's grid, plus
   [fade] times the refinement's correction. [fade] is 1 in the interior, ramps
   to 0 over [fade_metres] at the edge of the extent and around nodata, and is 0
   on nodata itself, so the result is exactly the source upsample there.

   Returns [None] when the patch holds no valid sample at all (a location
   outside French coverage), which keeps such locations on the source alone
   rather than on a bilinear upsample of it. *)
let blend ~lat ~(source : source) (raw : raw) =
  let { px_arcsec; size; fade_metres; _ } = raw.layer in
  let t0 = Brr.(Performance.now_ms G.performance) in
  let src = raw.samples in
  let has_nodata = ref false and has_data = ref false in
  for i = 0 to (size * size) - 1 do
    if Array1.unsafe_get src i < nodata_limit then has_nodata := true
    else has_data := true
  done;
  if not !has_data then None
  else begin
    let deltax, deltay, _ = Render_state.compute_deltas ~lat in
    let fade_x = fade_metres /. (deltax *. px_arcsec) in
    let fade_y = fade_metres /. (deltay *. px_arcsec) in
    let dist = if !has_nodata then Some (nodata_distance ~size src) else None in
    let fade_nodata = Float.min fade_x fade_y in
    let bsize = source.src_grid.Dem_loader.size in
    let bdata = source.src_grid.Dem_loader.data in
    let get_base row col =
      let low = Array2.unsafe_get bdata row (col * 2) in
      let high = Array2.unsafe_get bdata row ((col * 2) + 1) in
      float_of_int ((high lsl 8) lor low)
    in
    (* Fine sample [j] sits [raw.origin_x + j * px_arcsec] arcseconds from the
       anchor; the source's own origin and spacing turn that into a source index.
       The refinement's samples fall between the source's, at a fixed fractional
       step, so neither grid need be a power-of-two multiple of the other.

       Written so that the base case is bit-identical to the hardcoded form it
       replaces: subtracting a negative origin is exactly adding, addition is
       commutative in IEEE754, the grouping is unchanged, and dividing by a
       spacing of 1.0 is exact. *)
    let clamp_base v = Float.max 0. (Float.min (float (bsize - 2)) v) in
    let index_of origin o j =
      clamp_base
        ((o -. origin +. (float j *. px_arcsec)) /. source.src_px_arcsec)
    in
    let col_of j = index_of source.src_origin_x raw.origin_x j in
    let col_lo = int_of_float (floor (col_of 0)) in
    let col_hi = int_of_float (floor (col_of (size - 1))) + 1 in
    let n_cols = col_hi - col_lo + 1 in
    let bx = Array.make size 0 and fx = Array.make size 0. in
    for j = 0 to size - 1 do
      let c = col_of j in
      let b = floor c in
      bx.(j) <- int_of_float b - col_lo;
      fx.(j) <- c -. b
    done;
    let out = Array1.create int8_unsigned c_layout (size * size * 2) in
    (* One base row pair resampled per base row rather than per sample. *)
    let rowa = Array.make (n_cols + 1) 0. in
    let rowb = Array.make (n_cols + 1) 0. in
    let rowv = Array.make (n_cols + 1) 0. in
    let cur_by = ref (-1) in
    (* Per-column edge fade, hoisted out of the inner loop. *)
    let edge_x = Array.make size 0. in
    for j = 0 to size - 1 do
      edge_x.(j) <- float (min j (size - 1 - j)) /. fade_x
    done;
    for u = 0 to size - 1 do
      let cby = index_of source.src_origin_y raw.origin_y u in
      let by = int_of_float (floor cby) in
      let fy = cby -. float by in
      if by <> !cur_by then begin
        cur_by := by;
        for k = 0 to n_cols - 1 do
          rowa.(k) <- get_base by (col_lo + k);
          rowb.(k) <- get_base (by + 1) (col_lo + k)
        done
      end;
      for k = 0 to n_cols - 1 do
        rowv.(k) <- rowa.(k) +. (fy *. (rowb.(k) -. rowa.(k)))
      done;
      let edge_y = float (min u (size - 1 - u)) /. fade_y in
      (* Row 0 of the raster is the northernmost one, row 0 of a DEM tile the
         southernmost. *)
      let src_row = (size - 1 - u) * size in
      let dst_row = u * size * 2 in
      for j = 0 to size - 1 do
        let k = bx.(j) in
        let b = rowv.(k) +. (fx.(j) *. (rowv.(k + 1) -. rowv.(k))) in
        let h = Array1.unsafe_get src (src_row + j) in
        let v =
          if h < nodata_limit then b
          else
            let t = Float.min edge_x.(j) edge_y in
            let t =
              match dist with
              | None -> t
              | Some d ->
                  Float.min t
                    (float (Array1.unsafe_get d (src_row + j)) /. fade_nodata)
            in
            b +. (smoothstep t *. (u16_of_metres h -. b))
        in
        let v = int_of_float (v +. 0.5) in
        let v = if v < 0 then 0 else if v > 65535 then 65535 else v in
        Array1.unsafe_set out (dst_row + (2 * j)) (v land 0xff);
        Array1.unsafe_set out (dst_row + (2 * j) + 1) (v lsr 8)
      done
    done;
    Format.eprintf "RGE ALTI blended in %.0f ms (missing tiles: %d)@."
      (Brr.(Performance.now_ms G.performance) -. t0)
      raw.missing;
    Some
      {
        layer = raw.layer;
        grid =
          {
            Dem_loader.data = reshape_2 (genarray_of_array1 out) size (size * 2);
            size;
          };
        origin_x = raw.origin_x;
        origin_y = raw.origin_y;
      }
  end

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
let prefetch layer ~lat ~lon =
  let _, _, anchor_row, anchor_col = anchor_tile layer ~lat ~lon in
  let n = 2 * layer.block_tiles in
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
                      Jstr.v (tile_url layer ~row:(row0 + br) ~col:(col0 + bc))
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
