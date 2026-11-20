(* DEM Loader - Loads compressed .dem tiles using a Web Worker
   
   Protocol:
   1. "init" -> Resets worker state.
   2. "decode" -> Decodes a sub-tile into WASM memory.
   3. "finish" -> returns the accumulated heightmap.
*)

open Bigarray

let ( let* ) = Lwt.bind

(* Floor division *)
let ( // ) x y =
  let q = x / y in
  let r = x mod y in
  if r >= 0 then q else q - 1

let sub_tile_size = 1200

type t = { data : (int, int8_unsigned_elt, c_layout) Array2.t; size : int }

let get_height h row col =
  let low = h.data.{row, col * 2} in
  let high = h.data.{row, (col * 2) + 1} in
  let u16_val = (high lsl 8) lor low in
  (Float.of_int u16_val *. (9500.0 /. 65535.0)) -. 500.0

let get_texture_data h = h.data

(* Helper to convert Future to Lwt *)
let to_lwt f =
  let t, u = Lwt.task () in
  (Fut.await f @@ fun v -> Lwt.wakeup u v);
  t

(* Worker Manager *)
(* Worker Pool Manager *)

(* Path to a sub-tile file *)
let path ?(step = 1) ?(cover = false) ~lat ~lon ~row ~col () =
  let lat_c = if lat >= 0 then 'N' else 'S' in
  let lon_c = if lon >= 0 then 'E' else 'W' in
  Printf.sprintf "data/%s/%c%02d_%c%03d_%d_%d.dem"
    (if cover then Printf.sprintf "clc%d" step
     else if step = 1 then "dem"
     else Printf.sprintf "dem%d" step)
    lat_c (abs lat) lon_c (abs lon) row col

(* [step] is the grid's spacing in arcseconds: 1 for the base tiles, 4 for the
   far field's (compress_dem --coarse), whose sample [k] of a degree cell
   stands for the base samples [4 k] to [4 k + 3]. Everything below is in
   samples of that grid, the "arcsec" in the names being the base case: a
   sample's row is its northernmost arcsecond's, less one, over [step], plus
   one -- the sub-tiles' rows start one arcsecond north of the degree -- and
   its column its westernmost arcsecond's over [step]. A far tile that is not
   there is open sea, and not a failure.

   With [cover] the tiles are the far field's land cover (extract_tiles
   --coarse), on the same samples and in the same framing: the "heights" are
   palette indices, to be read raw, low byte first, and not through
   [get_height]. *)
(* Sub-tiles being downloaded, and how many loads wait for each: a load that
   starts while another is still fetching the same sub-tile -- a move made
   before a partial base was completed -- shares the download rather than
   starting it again. The buffer goes to a decode worker, transferred, so all
   but the last to take it get a copy. *)
let in_flight : (string, Jv.t Lwt.t * int ref) Hashtbl.t = Hashtbl.create 16

let shared_fetch p fetch =
  let t, users =
    match Hashtbl.find_opt in_flight p with
    | Some e -> e
    | None ->
        let t = fetch () in
        let e = (t, ref 0) in
        if Lwt.state t = Lwt.Sleep then begin
          Hashtbl.replace in_flight p e;
          Lwt.on_termination t (fun () -> Hashtbl.remove in_flight p)
        end;
        e
  in
  incr users;
  let* buf = t in
  decr users;
  Lwt.return (if !users > 0 then Jv.call buf "slice" [||] else buf)

(* The heightmap of [load], filled with sea level, and its sub-tiles: for each,
   the rectangle of the heightmap it fills -- first row, first column, last row,
   last column -- its path, and the task that fetches and decodes it into
   place, when it is run. A task never fails: a sub-tile that cannot be had is left at sea
   level, and counted in [failed] unless it is an absent far tile. *)
let sub_tiles ~step ~cover ~lat ~lon ~size ~loaded ~failed =
  let sub_tile_size = sub_tile_size / step in
  let degree = 3600 / step in

  (* Allocate full tile buffer (size * size * 2 bytes) *)
  let full_ba : (int, int8_unsigned_elt, c_layout) Array1.t =
    Array1.create int8_unsigned c_layout (size * size * 2)
  in
  (* Pre-fill with the 16-bit encoding of 0 m (little-endian; see
     [get_height]): sub-tiles that do not exist -- open sea, e.g. the
     Mediterranean south of Marseille -- must read as sea level, not as the
     -500 m that zero bytes decode to. 3449 = round (500 * 65535 / 9500). *)
  let sea = if cover then Clc_palette.get_index 523 else 3449 in
  let sea_low = sea land 0xff and sea_high = sea lsr 8 in
  (* By doubling blits, which are memcpys: an element at a time, the 32 MB
     took 360 ms under wasm_of_ocaml, a frozen frame at the start of each
     load. *)
  let n = Array1.dim full_ba in
  Array1.unsafe_set full_ba 0 sea_low;
  Array1.unsafe_set full_ba 1 sea_high;
  let filled = ref 2 in
  while !filled < n do
    let len = min !filled (n - !filled) in
    Array1.blit (Array1.sub full_ba 0 len) (Array1.sub full_ba !filled len);
    filled := !filled + len
  done;

  (* Convert center to arcseconds and compute bounds *)
  let center_lat_arcsec = ((Web_utils.arcsec_floor lat - 1) // step) + 1 in
  let center_lon_arcsec = Web_utils.arcsec_floor lon // step in
  let min_lat_arcsec = center_lat_arcsec - (size / 2) in
  let min_lon_arcsec = center_lon_arcsec - (size / 2) in
  let max_lat_arcsec = min_lat_arcsec + size - 1 in
  let max_lon_arcsec = min_lon_arcsec + size - 1 in

  let min_deg_lat = (min_lat_arcsec - 1) // degree in
  let max_deg_lat = (max_lat_arcsec - 1) // degree in
  let min_deg_lon = min_lon_arcsec // degree in
  let max_deg_lon = max_lon_arcsec // degree in

  let tasks = ref [] in
  for deg_lat = min_deg_lat to max_deg_lat do
    let tile_base_lat_arcsec = (deg_lat * degree) + 1 in
    for deg_lon = min_deg_lon to max_deg_lon do
      let tile_base_lon_arcsec = deg_lon * degree in

      (* Sub-tile bounds *)
      let min_sub_row =
        max 0 ((min_lat_arcsec - tile_base_lat_arcsec) / sub_tile_size)
      in
      let max_sub_row =
        min 2 ((max_lat_arcsec - tile_base_lat_arcsec) / sub_tile_size)
      in
      let min_sub_col =
        max 0 ((min_lon_arcsec - tile_base_lon_arcsec) / sub_tile_size)
      in
      let max_sub_col =
        min 2 ((max_lon_arcsec - tile_base_lon_arcsec) / sub_tile_size)
      in

      for sub_row = min_sub_row to max_sub_row do
        for sub_col = min_sub_col to max_sub_col do
          let subtile_min_lat =
            tile_base_lat_arcsec + (sub_row * sub_tile_size)
          in
          let subtile_min_lon =
            tile_base_lon_arcsec + (sub_col * sub_tile_size)
          in
          let subtile_max_lat = subtile_min_lat + sub_tile_size - 1 in
          let subtile_max_lon = subtile_min_lon + sub_tile_size - 1 in

          let overlap_min_lat = max min_lat_arcsec subtile_min_lat in
          let overlap_max_lat = min max_lat_arcsec subtile_max_lat in
          let overlap_min_lon = max min_lon_arcsec subtile_min_lon in
          let overlap_max_lon = min max_lon_arcsec subtile_max_lon in

          if
            overlap_min_lat <= overlap_max_lat
            && overlap_min_lon <= overlap_max_lon
          then begin
            let p =
              path ~step ~cover ~lat:deg_lat ~lon:deg_lon ~row:sub_row
                ~col:sub_col ()
            in

            (* Create a task for this sub-tile *)
            let task () =
              (* Fetch data on main thread *)
              let* data_jv =
                shared_fetch p @@ fun () ->
                let open Brr_io.Fetch in
                let* res = to_lwt (Web_utils.fetch_tile (Jstr.v p)) in
                match res with
                | Error e ->
                    Lwt.fail (Failure (Jstr.to_string (Jv.Error.message e)))
                | Ok resp when not (Response.ok resp) ->
                    Lwt.fail
                      (Failure
                         (Printf.sprintf "HTTP %d for %s" (Response.status resp)
                            p))
                | Ok resp -> (
                    let* res_buf =
                      to_lwt (Body.array_buffer (Response.as_body resp))
                    in
                    match res_buf with
                    | Error e ->
                        Lwt.fail (Failure (Jstr.to_string (Jv.Error.message e)))
                    | Ok buf -> Lwt.return (Brr.Tarray.Buffer.to_jv buf))
              in

              let* w = Worker_pool.acquire () in
              let* res =
                Lwt.finalize
                  (fun () ->
                    Worker_pool.post w (Worker_pool.Decode (DEM data_jv)))
                  (fun () ->
                    Worker_pool.release w;
                    Lwt.return ())
              in
              match res with
              | Worker_pool.ResultDEM { heights = ba } ->
                  (* Blit sub-tile into full buffer. *)
                  let sub_width = sub_tile_size in

                  (* Copy row by row *)
                  (* overlap_* coordinates are in arcseconds. *)
                  let overlap_h = overlap_max_lat - overlap_min_lat + 1 in
                  let overlap_w = overlap_max_lon - overlap_min_lon + 1 in

                  let src_start_row = overlap_min_lat - subtile_min_lat in
                  let src_start_col = overlap_min_lon - subtile_min_lon in

                  let dst_start_row = overlap_min_lat - min_lat_arcsec in
                  let dst_start_col = overlap_min_lon - min_lon_arcsec in

                  for r = 0 to overlap_h - 1 do
                    (* Convert South-to-North iterator 'r' to Top-Down row indices *)
                    let src_row = sub_width - 1 - (src_start_row + r) in
                    let dst_row = dst_start_row + r in

                    let src_idx =
                      (src_row * sub_width * 2) + (src_start_col * 2)
                    in
                    let dst_idx = (dst_row * size * 2) + (dst_start_col * 2) in
                    let len = overlap_w * 2 in

                    if
                      src_idx >= 0 && dst_idx >= 0
                      && src_idx + len <= Array1.dim ba
                      && dst_idx + len <= Array1.dim full_ba
                    then
                      Array1.blit
                        (Array1.sub ba src_idx len)
                        (Array1.sub full_ba dst_idx len)
                  done;
                  Lwt.return ()
              | _ -> Lwt.fail (Failure "Unexpected worker response for DEM")
            in
            (* A missing or broken sub-tile must not abort the whole heightmap:
               its region is simply left at the zero-initialized value. *)
            let task () =
              Lwt.catch
                (fun () ->
                  let* () = task () in
                  incr loaded;
                  Lwt.return ())
                (fun exn ->
                  (match exn with
                  | Failure msg
                    when step > 1 && String.starts_with ~prefix:"HTTP 404 " msg
                    ->
                      ()
                  | _ ->
                      incr failed;
                      Brr.Console.error
                        [
                          Jstr.v
                            (Printf.sprintf "DEM sub-tile %s not loaded: %s" p
                               (Printexc.to_string exn));
                        ]);
                  Lwt.return ())
            in
            let rect =
              ( overlap_min_lat - min_lat_arcsec,
                overlap_min_lon - min_lon_arcsec,
                overlap_max_lat - min_lat_arcsec,
                overlap_max_lon - min_lon_arcsec )
            in
            tasks := (rect, p, task) :: !tasks
          end
        done
      done
    done
  done;

  let heights = reshape_2 (genarray_of_array1 full_ba) size (size * 2) in
  ({ data = heights; size }, List.rev !tasks)

let run tasks = Lwt.join (List.map (fun (_, _, task) -> task ()) tasks)

let load ?(step = 1) ?(cover = false) ~lat ~lon ~size () =
  Worker_pool.init ();
  let loaded = ref 0 and failed = ref 0 in
  let t, tasks = sub_tiles ~step ~cover ~lat ~lon ~size ~loaded ~failed in
  let* () = run tasks in
  if !loaded = 0 && !failed > 0 then
    Lwt.fail (Failure "Could not load any DEM sub-tile")
  else Lwt.return t

(* The sub-tiles under the [half_rows] x [half_cols] rectangle about the centre
   first, the others only if they are all in the cache already: a first visit,
   where they are not, can then go to screen on the near ground alone, the far
   field standing in beyond it, and fetch the rest in its own time. *)
let load_progressive ~lat ~lon ~size ~half_rows ~half_cols =
  Worker_pool.init ();
  let loaded = ref 0 and failed = ref 0 in
  let t, tasks =
    sub_tiles ~step:1 ~cover:false ~lat ~lon ~size ~loaded ~failed
  in
  let r0 = (size / 2) - half_rows and r1 = (size / 2) + half_rows in
  let c0 = (size / 2) - half_cols and c1 = (size / 2) + half_cols in
  let inner, outer =
    List.partition
      (fun ((tr0, tc0, tr1, tc1), _, _) ->
        tr0 <= r1 && tr1 >= r0 && tc0 <= c1 && tc1 >= c0)
      tasks
  in
  (* The inner sub-tiles are wanted either way: fetched while the cache is
     looked into for the others. *)
  let inner_done = run inner in
  let* cached =
    Lwt.all
      (List.map
         (fun (_, p, _) -> to_lwt (Web_utils.is_cached (Jstr.v p)))
         outer)
  in
  let complete = List.for_all Fun.id cached in
  let* () = Lwt.join (inner_done :: (if complete then [ run outer ] else [])) in
  if !loaded = 0 && !failed > 0 then
    Lwt.fail (Failure "Could not load any DEM sub-tile")
  else if complete then Lwt.return (t, None)
  else
    (* What the inner sub-tiles filled, a rectangle: they are a block of the
       sub-tile grid. *)
    let valid =
      List.fold_left
        (fun (a, b, c, d) ((tr0, tc0, tr1, tc1), _, _) ->
          (min a tr0, min b tc0, max c tr1, max d tc1))
        (size, size, -1, -1) inner
    in
    Lwt.return (t, Some (valid, fun () -> run outer))

(* The tiles [load] would fetch, with the same [step] and [cover] and the same
   meaning of [size]. *)
let prefetch ?(step = 1) ?(cover = false) ~lat ~lon ~size () =
  (* Simple prefetch to warm browser cache *)
  let sub_tile_size = sub_tile_size / step in
  let degree = 3600 / step in
  let center_lat_arcsec = ((Web_utils.arcsec_floor lat - 1) // step) + 1 in
  let center_lon_arcsec = Web_utils.arcsec_floor lon // step in
  let min_lat_arcsec = center_lat_arcsec - (size / 2) in
  let min_lon_arcsec = center_lon_arcsec - (size / 2) in
  let max_lat_arcsec = min_lat_arcsec + size - 1 in
  let max_lon_arcsec = min_lon_arcsec + size - 1 in

  let min_deg_lat = (min_lat_arcsec - 1) // degree in
  let max_deg_lat = (max_lat_arcsec - 1) // degree in
  let min_deg_lon = min_lon_arcsec // degree in
  let max_deg_lon = max_lon_arcsec // degree in

  let prefetch_tasks = ref [] in
  for deg_lat = min_deg_lat to max_deg_lat do
    let tile_base_lat_arcsec = (deg_lat * degree) + 1 in
    for deg_lon = min_deg_lon to max_deg_lon do
      let tile_base_lon_arcsec = deg_lon * degree in
      let min_sub_row =
        max 0 ((min_lat_arcsec - tile_base_lat_arcsec) / sub_tile_size)
      in
      let max_sub_row =
        min 2 ((max_lat_arcsec - tile_base_lat_arcsec) / sub_tile_size)
      in
      let min_sub_col =
        max 0 ((min_lon_arcsec - tile_base_lon_arcsec) / sub_tile_size)
      in
      let max_sub_col =
        min 2 ((max_lon_arcsec - tile_base_lon_arcsec) / sub_tile_size)
      in

      for sub_row = min_sub_row to max_sub_row do
        for sub_col = min_sub_col to max_sub_col do
          let subtile_min_lat =
            tile_base_lat_arcsec + (sub_row * sub_tile_size)
          in
          let subtile_min_lon =
            tile_base_lon_arcsec + (sub_col * sub_tile_size)
          in
          let subtile_max_lat = subtile_min_lat + sub_tile_size - 1 in
          let subtile_max_lon = subtile_min_lon + sub_tile_size - 1 in

          let overlap_min_lat = max min_lat_arcsec subtile_min_lat in
          let overlap_max_lat = min max_lat_arcsec subtile_max_lat in
          let overlap_min_lon = max min_lon_arcsec subtile_min_lon in
          let overlap_max_lon = min max_lon_arcsec subtile_max_lon in

          if
            overlap_min_lat <= overlap_max_lat
            && overlap_min_lon <= overlap_max_lon
          then begin
            let p =
              path ~step ~cover ~lat:deg_lat ~lon:deg_lon ~row:sub_row
                ~col:sub_col ()
            in
            let task =
              let to_lwt f =
                let t, u = Lwt.task () in
                ( Fut.await f @@ fun v ->
                  match v with
                  | Ok v -> Lwt.wakeup u v
                  | Error err -> Lwt.wakeup_exn u (Jv.Error err) );
                t
              in
              to_lwt
              @@
              let open Fut.Result_syntax in
              let request = Brr_io.Fetch.Request.v (Jstr.v p) in
              let* cache =
                Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ())
                  (Jstr.v Data_cache.name)
              in
              let* response = Brr_io.Fetch.Cache.match' cache request in
              match response with
              | Some response when Brr_io.Fetch.Response.ok response ->
                  Fut.return (Ok ())
              | _ ->
                  Format.eprintf "  Prefetching %s@." p;
                  Brr_io.Fetch.Cache.add cache request
            in
            (* A far tile that is not there is open sea: nothing to keep,
               and no reason to give up on the others. *)
            let task =
              if step = 1 then task
              else Lwt.catch (fun () -> task) (fun _ -> Lwt.return_unit)
            in
            prefetch_tasks := task :: !prefetch_tasks
          end
        done
      done
    done
  done;
  Lwt.join !prefetch_tasks

(* Check if position is within available data range: the whole extent requested
   by [load], not just its center. *)
let in_range ~size ~min_lat ~max_lat ~min_lon ~max_lon ~lat ~lon =
  (* Same bounds as in [load]: [arcsec_floor] here and there must match. *)
  let center_lat_arcsec = Web_utils.arcsec_floor lat in
  let center_lon_arcsec = Web_utils.arcsec_floor lon in
  let min_lat_arcsec = center_lat_arcsec - (size / 2) in
  let min_lon_arcsec = center_lon_arcsec - (size / 2) in
  let max_lat_arcsec = min_lat_arcsec + size - 1 in
  let max_lon_arcsec = min_lon_arcsec + size - 1 in
  (* Tile rows cover latitudes (deg, deg + 1] in arcseconds (see the +1 in
     [load]) while tile columns cover longitudes [deg, deg + 1): the available
     data spans (min_lat, max_lat] by [min_lon, max_lon) degrees. *)
  min_lat_arcsec > min_lat * 3600
  && max_lat_arcsec <= max_lat * 3600
  && min_lon_arcsec >= min_lon * 3600
  && max_lon_arcsec < max_lon * 3600
