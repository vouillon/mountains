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

(* Level 13 of the WGS84G tile matrix set: 256 x 256 sample tiles over
   360/16384 degrees each, origin (-180, 90), row 0 northernmost. One sample is
   0.309 arcseconds, i.e. 9.5 m north-south -- a bit over three times the base
   grid, and *not* a power-of-two ratio to it, so every consumer takes the
   spacing from [px_arcsec] rather than assuming one.

   Level 14 exists at four times the data; measurements at level 13 (below)
   already put this at the edge of what a location load can absorb. *)
let matrix_level = 13
let tiles_per_axis = 16384
let tile_px = 256

(* Sample spacing. [px_arcsec] is the one spacing constant of the module. *)
let px_deg = 360. /. float tiles_per_axis /. float tile_px
let px_arcsec = px_deg *. 3600.

(* A square block of 8 x 8 tiles centred on the tile holding the location:
   2048 samples per side (a power of two, so the relief pyramid keeps its usual
   shape), 633 arcseconds, i.e. 19.5 km north-south and 13.6 km east-west at 46
   degrees. Anchoring the block on the *tile* rather than on the location makes
   every location within one level-13 tile share the same 64 URLs, which is
   what makes the service worker's cache-first rule worth having.

   Measured: 214 kB on the wire per tile (the service deflates the payload),
   64 tiles in ~4 s with browser-level parallelism over one HTTP/2
   connection. *)
let block_tiles = 8
let size = tile_px * block_tiles

(* Anything below this is IGN's nodata sentinel (-99999 outside coverage). *)
let nodata_limit = -500.

(* Width of the annulus over which the high-resolution data is faded back into
   the base, both at the edge of the extent and around nodata. Held well inside
   the extent so that the renderer's hard switch at the boundary lands where
   the two agree. *)
let fade_metres = 1500.

(* Ceiling on the whole set of tile requests. The location cannot be published
   before this resolves, so it is also the worst-case load-time penalty;
   expiring is not an error, the location simply renders from the base tile. *)
let timeout_s = 25.

(* u16 quantisation shared with the .dem pipeline (Corine/compress_dem.ml),
   [Dem_loader.get_height] and HEIGHT_SCALE in the shaders. *)
let u16_of_metres h = (h +. 500.) *. (65535. /. 9500.)

let tile_url ~row ~col =
  Printf.sprintf
    "https://data.geopf.fr/wmts?SERVICE=WMTS&VERSION=1.0.0&REQUEST=GetTile&LAYER=ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES&STYLE=normal&FORMAT=image/x-bil;bits=32&TILEMATRIXSET=WGS84G&TILEMATRIX=%d&TILEROW=%d&TILECOL=%d"
    matrix_level row col

let to_lwt f =
  let t, u = Lwt.task () in
  (Fut.await f @@ fun v -> Lwt.wakeup u v);
  t

(* The level-13 tile holding the location, on which the block is centred. *)
let anchor_tile ~lat ~lon =
  let alat = Web_utils.arcsec_floor lat and alon = Web_utils.arcsec_floor lon in
  let span = 360. /. float tiles_per_axis in
  let row = int_of_float (floor ((90. -. (float alat /. 3600.)) /. span)) in
  let col = int_of_float (floor (((float alon /. 3600.) +. 180.) /. span)) in
  (alat, alon, row, col)

type raw = {
  samples : (float, float32_elt, c_layout) Array1.t;
      (* [size] x [size], row 0 northernmost *)
  origin_x : float; (* arcseconds from the anchor to sample column 0 *)
  origin_y : float; (* arcseconds from the anchor to the southernmost row *)
  missing : int;
}

(* One tile. A tile fully outside RGE ALTI coverage answers 404 with an
   exception report; that -- like any other per-tile failure -- just leaves its
   region at nodata, which the blend turns back into the base. *)
let fetch_tile ~row ~col =
  let open Brr_io.Fetch in
  let* res = to_lwt (url (Jstr.v (tile_url ~row ~col))) in
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
let fetch ~lat ~lon : raw option Lwt.t =
  let alat, alon, anchor_row, anchor_col = anchor_tile ~lat ~lon in
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
  let missing = ref 0 in
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
                let* tile = fetch_tile ~row:(row0 + br) ~col:(col0 + bc) in
                (* Blit row by row: the tile is contiguous, the destination is
                   strided by the width of the whole block. *)
                for i = 0 to tile_px - 1 do
                  Brr.Tarray.set_tarray full
                    ~dst:((((br * tile_px) + i) * size) + (bc * tile_px))
                    (Brr.Tarray.sub tile ~start:(i * tile_px)
                       ~stop:((i + 1) * tile_px))
                done;
                Lwt.return ())
              (fun _ ->
                incr missing;
                Lwt.return ()))
          (List.init block_tiles Fun.id))
      (List.init block_tiles Fun.id)
  in
  let finished = ref false in
  let all =
    let* () = Lwt.join tasks in
    finished := true;
    let total = block_tiles * block_tiles in
    if !missing = total then begin
      Brr.Console.error
        [ Jstr.v "High-resolution elevation: no tile could be fetched" ];
      Lwt.return None
    end
    else begin
      Format.eprintf "RGE ALTI: %d/%d tiles in %.0f ms@." (total - !missing)
        total
        (Brr.(Performance.now_ms G.performance) -. t0);
      Lwt.return
        (Some
           {
             samples = Brr.Tarray.to_bigarray1 full;
             origin_x;
             origin_y;
             missing = !missing;
           })
    end
  in
  let timeout =
    let t, w = Lwt.task () in
    ignore
      (Brr.G.set_timeout
         ~ms:(truncate (timeout_s *. 1000.))
         (fun () ->
           if not !finished then begin
             Brr.Console.error
               [ Jstr.v "High-resolution elevation: request timed out" ];
             Lwt.wakeup_later w None
           end));
    t
  in
  Lwt.choose [ all; timeout ]

(* Chessboard distance in texels to the nearest nodata sample, saturated at
   255: two sweeps over the raster. Only built when the patch actually holds
   nodata, which means a location near the edge of French coverage. *)
let nodata_distance (src : (float, float32_elt, c_layout) Array1.t) =
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

type t = { grid : Dem_loader.t; origin_x : float; origin_y : float }

(* Build the blended grid: the base tile resampled onto the high-resolution
   grid, plus [fade] times the high-resolution correction. [fade] is 1 in the
   interior, ramps to 0 over [fade_metres] at the edge of the extent and around
   nodata, and is 0 on nodata itself, so the result is exactly the base upsample
   there.

   Returns [None] when the patch holds no valid sample at all (a location
   outside French coverage), which keeps such locations on the pure base path
   rather than on a bilinear upsample of it. *)
let blend ~lat ~(base : Dem_loader.t) raw =
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
    let dist = if !has_nodata then Some (nodata_distance src) else None in
    let fade_nodata = Float.min fade_x fade_y in
    let bsize = base.Dem_loader.size in
    let bdata = base.Dem_loader.data in
    let get_base row col =
      let low = Array2.unsafe_get bdata row (col * 2) in
      let high = Array2.unsafe_get bdata row ((col * 2) + 1) in
      float_of_int ((high lsl 8) lor low)
    in
    (* The anchor arcsecond sits at base index [bsize / 2] on both axes (see
       [Dem_loader.load]); the high-resolution samples fall between base
       samples, at a fixed fractional step. *)
    let half = float (bsize / 2) in
    let clamp_base v = Float.max 0. (Float.min (float (bsize - 2)) v) in
    let col_of j =
      clamp_base (half +. raw.origin_x +. (float j *. px_arcsec))
    in
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
      let cby = clamp_base (half +. raw.origin_y +. (float u *. px_arcsec)) in
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
let prefetch ~lat ~lon =
  let _, _, anchor_row, anchor_col = anchor_tile ~lat ~lon in
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
                      Jstr.v (tile_url ~row:(row0 + br) ~col:(col0 + bc))
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
