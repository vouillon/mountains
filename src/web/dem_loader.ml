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
let path ~lat ~lon ~row ~col =
  let lat_c = if lat >= 0 then 'N' else 'S' in
  let lon_c = if lon >= 0 then 'E' else 'W' in
  Printf.sprintf "data/dem/%c%02d_%c%03d_%d_%d.dem" lat_c (abs lat) lon_c
    (abs lon) row col

let load ~lat ~lon ~size =
  Worker_pool.init ();

  (* Allocate full tile buffer (size * size * 2 bytes) *)
  let full_ba : (int, int8_unsigned_elt, c_layout) Array1.t =
    Array1.create int8_unsigned c_layout (size * size * 2)
  in
  (* Pre-fill with the 16-bit encoding of 0 m (little-endian; see
     [get_height]): sub-tiles that do not exist -- open sea, e.g. the
     Mediterranean south of Marseille -- must read as sea level, not as the
     -500 m that zero bytes decode to. 3449 = round (500 * 65535 / 9500). *)
  let sea = 3449 in
  let sea_low = sea land 0xff and sea_high = sea lsr 8 in
  for i = 0 to (Array1.dim full_ba / 2) - 1 do
    Array1.unsafe_set full_ba (2 * i) sea_low;
    Array1.unsafe_set full_ba ((2 * i) + 1) sea_high
  done;

  (* Convert center to arcseconds and compute bounds *)
  let center_lat_arcsec = Web_utils.arcsec_floor lat in
  let center_lon_arcsec = Web_utils.arcsec_floor lon in
  let min_lat_arcsec = center_lat_arcsec - (size / 2) in
  let min_lon_arcsec = center_lon_arcsec - (size / 2) in
  let max_lat_arcsec = min_lat_arcsec + size - 1 in
  let max_lon_arcsec = min_lon_arcsec + size - 1 in

  let min_deg_lat = (min_lat_arcsec - 1) // 3600 in
  let max_deg_lat = (max_lat_arcsec - 1) // 3600 in
  let min_deg_lon = min_lon_arcsec // 3600 in
  let max_deg_lon = max_lon_arcsec // 3600 in

  let tasks = ref [] in
  let loaded = ref 0 in
  let failed = ref 0 in
  for deg_lat = min_deg_lat to max_deg_lat do
    let tile_base_lat_arcsec = (deg_lat * 3600) + 1 in
    for deg_lon = min_deg_lon to max_deg_lon do
      let tile_base_lon_arcsec = deg_lon * 3600 in

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
            let p = path ~lat:deg_lat ~lon:deg_lon ~row:sub_row ~col:sub_col in

            (* Create a task for this sub-tile *)
            let task () =
              (* Fetch data on main thread *)
              let* data_jv =
                let open Brr_io.Fetch in
                let* res = to_lwt (url (Jstr.v p)) in
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
                  let sub_width = 1200 in

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
            let task =
              Lwt.catch
                (fun () ->
                  let* () = task () in
                  incr loaded;
                  Lwt.return ())
                (fun exn ->
                  incr failed;
                  Brr.Console.error
                    [
                      Jstr.v
                        (Printf.sprintf "DEM sub-tile %s not loaded: %s" p
                           (Printexc.to_string exn));
                    ];
                  Lwt.return ())
            in
            tasks := task :: !tasks
          end
        done
      done
    done
  done;

  (* Run all tasks in parallel *)
  let* () = Lwt.join !tasks in
  let* () =
    if !loaded = 0 && !failed > 0 then
      Lwt.fail (Failure "Could not load any DEM sub-tile")
    else Lwt.return ()
  in

  (* Reshape to 2D *)
  let heights = reshape_2 (genarray_of_array1 full_ba) size (size * 2) in
  Lwt.return { data = heights; size }

let prefetch ~lat ~lon ~size =
  (* Simple prefetch to warm browser cache *)
  let center_lat_arcsec = Web_utils.arcsec_floor lat in
  let center_lon_arcsec = Web_utils.arcsec_floor lon in
  let min_lat_arcsec = center_lat_arcsec - (size / 2) in
  let min_lon_arcsec = center_lon_arcsec - (size / 2) in
  let max_lat_arcsec = min_lat_arcsec + size - 1 in
  let max_lon_arcsec = min_lon_arcsec + size - 1 in

  let min_deg_lat = (min_lat_arcsec - 1) // 3600 in
  let max_deg_lat = (max_lat_arcsec - 1) // 3600 in
  let min_deg_lon = min_lon_arcsec // 3600 in
  let max_deg_lon = max_lon_arcsec // 3600 in

  let prefetch_tasks = ref [] in
  for deg_lat = min_deg_lat to max_deg_lat do
    let tile_base_lat_arcsec = (deg_lat * 3600) + 1 in
    for deg_lon = min_deg_lon to max_deg_lon do
      let tile_base_lon_arcsec = deg_lon * 3600 in
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
            let p = path ~lat:deg_lat ~lon:deg_lon ~row:sub_row ~col:sub_col in
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
                  (Jstr.v "v1")
              in
              let* response = Brr_io.Fetch.Cache.match' cache request in
              match response with
              | Some response when Brr_io.Fetch.Response.ok response ->
                  Fut.return (Ok ())
              | _ ->
                  Format.eprintf "  Prefetching %s@." p;
                  Brr_io.Fetch.Cache.add cache request
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
