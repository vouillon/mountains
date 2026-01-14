(* clc_loader.ml - Load and rasterize CLC tiles for web viewer *)

open Bigarray

let ( let* ) = Lwt.bind

(* Convert Lwt future to Lwt promise *)
let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
  t

(* CLC tile header info *)
type clc_header = {
  count : int;
  total_verts : int;
  total_indices : int;
  water_count : int; (* CLC4+ only *)
  water_verts : int; (* CLC4+ only *)
  water_indices : int; (* CLC4+ only *)
  poi_count : int; (* CLC5 only *)
  min_lon : float;
  min_lat : float;
  scale_x : float;
  scale_y : float;
  water_scale_x : float; (* CLC4+ only *)
  water_scale_y : float; (* CLC4+ only *)
  poi_scale_x : float; (* CLC5 only *)
  poi_scale_y : float; (* CLC5 only *)
  is_clc4 : bool;
  is_clc5 : bool;
}

(* POI data *)
type poi_type = Peak | Saddle

type poi = {
  name : string;
  lat : float;
  lon : float;
  elevation : int;
  poi_type : poi_type;
}

(* Parsed CLC tile data ready for rasterization *)
type clc_tile = {
  header : clc_header;
  positions : (int, int16_unsigned_elt, c_layout) Array1.t; (* x,y pairs U16 *)
  colors : (int, int8_unsigned_elt, c_layout) Array1.t; (* palette indices *)
  indices : (int32, int32_elt, c_layout) Array1.t;
  (* Water layer (CLC4+ only) - stored as int32 for full 24-bit precision *)
  water_positions : (int32, int32_elt, c_layout) Array1.t;
  water_colors : (int, int8_unsigned_elt, c_layout) Array1.t;
  water_indices : (int32, int32_elt, c_layout) Array1.t;
  (* POI data (CLC5 only) *)
  pois : poi list;
}

(* Helper to parse header from bigarray *)
let parse_header_ba ba =
  let get_s i len =
    String.init len (fun j -> Char.chr (Array1.get ba (i + j)))
  in
  let magic = get_s 0 4 in
  let is_clc4 = magic = "CLC4" in
  let is_clc5 = magic = "CLC5" in

  let read_i32_be i =
    let b0 = Array1.get ba i in
    let b1 = Array1.get ba (i + 1) in
    let b2 = Array1.get ba (i + 2) in
    let b3 = Array1.get ba (i + 3) in
    (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
  in

  let read_f64_le i =
    let rec loop j acc =
      if j < 0 then acc
      else
        loop (j - 1)
          (Int64.logor (Int64.shift_left acc 8)
             (Int64.of_int (Array1.get ba (i + j))))
    in
    Int64.float_of_bits (loop 7 0L)
  in

  let count = read_i32_be 4 in
  let total_verts = read_i32_be 8 in
  let total_indices = read_i32_be 12 in

  let water_count, water_verts, water_indices, poi_count, float_offset =
    if is_clc5 then
      (read_i32_be 16, read_i32_be 20, read_i32_be 24, read_i32_be 28, 32)
    else if is_clc4 then (read_i32_be 16, read_i32_be 20, read_i32_be 24, 0, 28)
    else (0, 0, 0, 0, 16)
  in

  let min_lon = read_f64_le float_offset in
  let min_lat = read_f64_le (float_offset + 8) in
  let scale_x = read_f64_le (float_offset + 16) in
  let scale_y = read_f64_le (float_offset + 24) in

  let water_scale_x, water_scale_y, poi_scale_x, poi_scale_y =
    if is_clc5 then
      ( read_f64_le (float_offset + 32),
        read_f64_le (float_offset + 40),
        read_f64_le (float_offset + 48),
        read_f64_le (float_offset + 56) )
    else if is_clc4 then
      ( read_f64_le (float_offset + 32),
        read_f64_le (float_offset + 40),
        0.0,
        0.0 )
    else (0.0, 0.0, 0.0, 0.0)
  in

  {
    count;
    total_verts;
    total_indices;
    water_count;
    water_verts;
    water_indices;
    poi_count;
    min_lon;
    min_lat;
    scale_x;
    scale_y;
    water_scale_x;
    water_scale_y;
    poi_scale_x;
    poi_scale_y;
    is_clc4;
    is_clc5;
  }

(* Get CLC tile name from lat/lon *)
let tile_name lat lon =
  let lat_int = int_of_float (floor lat) in
  let lon_int = int_of_float (floor lon) in
  let lat_str = if lat_int >= 0 then "N" else "S" in
  let lon_str = if lon_int >= 0 then "E" else "W" in
  Printf.sprintf "%s%02d%s%03d.clc" lat_str (abs lat_int) lon_str (abs lon_int)

(* Tile path for fetching *)
let tile_path lat lon = "data/clc/" ^ tile_name lat lon

let ( // ) x y =
  let q = x / y in
  let r = x mod y in
  if r >= 0 then q else q - 1

let rec parallel_iter min max f =
  if min > max then Lwt.return ()
  else Lwt.join [ f min; parallel_iter (min + 1) max f ]

let prefetch_tile ~lat ~lon =
  let open Lwt.Syntax in
  let f = tile_path (float lat +. 0.5) (float lon +. 0.5) in
  let request = Brr_io.Fetch.Request.v (Jstr.v f) in
  let* cache =
    to_lwt
      (Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ()) (Jstr.v "v1"))
  in
  let* response = to_lwt (Brr_io.Fetch.Cache.match' cache request) in
  match response with
  | Some response when Brr_io.Fetch.Response.ok response -> Lwt.return ()
  | _ ->
      Format.eprintf "Prefetching CLC %s@." f;
      to_lwt (Brr_io.Fetch.Cache.add cache request)

let prefetch ~size ~lat ~lon =
  let min_lat = truncate (lat *. 3600.) - (size / 2) in
  let min_lon = truncate (lon *. 3600.) - (size / 2) in
  let max_lat = min_lat + size - 1 in
  let max_lon = min_lon + size - 1 in
  let* () =
    parallel_iter ((min_lat - 1) // 3600) ((max_lat - 1) // 3600) @@ fun lat ->
    parallel_iter (min_lon // 3600) (max_lon // 3600) @@ fun lon ->
    prefetch_tile ~lat ~lon
  in
  Lwt.return ()

(* Converters *)
let to_u8 jv =
  Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv jv)

let _to_ba ta = Brr.Tarray.to_bigarray1 ta

let _to_ba32 ta =
  let buf = Brr.Tarray.buffer ta in
  let ta32 = Brr.Tarray.of_buffer Brr.Tarray.Int32 buf in
  Brr.Tarray.to_bigarray1 ta32

(* Parse POIs from decompressed buffers *)
let parse_pois header names_jv coords_jv elevs_jv types_jv =
  if
    Jv.is_null names_jv || Jv.is_null coords_jv || Jv.is_null elevs_jv
    || Jv.is_null types_jv
  then []
  else
    let names_ba = Brr.Tarray.to_bigarray1 (to_u8 names_jv) in
    let coords_ba = Brr.Tarray.to_bigarray1 (to_u8 coords_jv) in
    let elevs_ba = Brr.Tarray.to_bigarray1 (to_u8 elevs_jv) in
    let types_ba = Brr.Tarray.to_bigarray1 (to_u8 types_jv) in

    let name_idx = ref 0 in
    let coord_idx = ref 0 in
    let elev_idx = ref 0 in
    let type_idx = ref 0 in

    let pois = ref [] in

    for _ = 0 to header.poi_count - 1 do
      (* Parse Name *)
      let name_len = Array1.get names_ba !name_idx in
      incr name_idx;
      let name =
        String.init name_len (fun i ->
            Char.chr (Array1.get names_ba (!name_idx + i)))
      in
      name_idx := !name_idx + name_len;

      (* Parse Coords (24-bit x, 24-bit y) *)
      let xl = Array1.get coords_ba !coord_idx in
      let xm = Array1.get coords_ba (!coord_idx + 1) in
      let xh = Array1.get coords_ba (!coord_idx + 2) in
      let yl = Array1.get coords_ba (!coord_idx + 3) in
      let ym = Array1.get coords_ba (!coord_idx + 4) in
      let yh = Array1.get coords_ba (!coord_idx + 5) in
      coord_idx := !coord_idx + 6;

      let qx = xl lor (xm lsl 8) lor (xh lsl 16) in
      let qy = yl lor (ym lsl 8) lor (yh lsl 16) in

      let lon = header.min_lon +. (float qx /. header.poi_scale_x) in
      let lat = header.min_lat +. (float qy /. header.poi_scale_y) in

      (* Parse Elevation (16-bit signed) *)
      let el = Array1.get elevs_ba !elev_idx in
      let eh = Array1.get elevs_ba (!elev_idx + 1) in
      elev_idx := !elev_idx + 2;
      let edu = el lor (eh lsl 8) in
      let elevation = if edu >= 32768 then edu - 65536 else edu in

      (* Parse Type *)
      let t_byte = Array1.get types_ba !type_idx in
      incr type_idx;
      let poi_type = if t_byte = 0 then Peak else Saddle in

      pois := { name; lat; lon; elevation; poi_type } :: !pois
    done;
    List.rev !pois

(* Full CLC tile loading: offload to worker *)
let load_full_clc_tile path_str =
  let open Lwt.Syntax in
  let* resp = to_lwt (Brr_io.Fetch.url (Jstr.v path_str)) in
  let* buf =
    to_lwt (Brr_io.Fetch.Body.array_buffer (Brr_io.Fetch.Response.as_body resp))
  in
  let data = Brr.Tarray.of_buffer Brr.Tarray.Uint8 buf in
  let data_ba = Brr.Tarray.to_bigarray1 data in
  let header = parse_header_ba data_ba in

  let* w = Worker_pool.acquire () in
  let* res =
    Lwt.finalize
      (fun () ->
        (* We need to transfer valid ArrayBuffer. *)
        (* data is Uint8Array? Brr.Tarray.buffer gets the ArrayBuffer *)
        let buf_jv = Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer data) in
        Worker_pool.post w (Worker_pool.Decode (Worker_pool.CLC buf_jv)))
      (fun () ->
        Worker_pool.release w;
        Lwt.return ())
  in

  match res with
  | Worker_pool.ResultCLC (Clc_data obj_msg) ->
      let obj = Jv.get obj_msg "data" in
      let pos_jv = Jv.get obj "pos" in
      let col_jv = Jv.get obj "col" in
      let idx_jv = Jv.get obj "idx" in

      let water_pos_jv = Jv.get obj "water_pos" in
      let water_col_jv = Jv.get obj "water_col" in
      let water_idx_jv = Jv.get obj "water_idx" in

      (* Convert back to typed arrays / bigarrays *)
      let pos_ta = to_u8 pos_jv in
      let col_ta = to_u8 col_jv in
      let idx_ta = to_u8 idx_jv in
      (* It's bytes, need view as int32 *)

      (* Positions: we need (int, int16_unsigned_elt, c_layout) Array1.t *)
      let positions =
        let buf = Brr.Tarray.buffer pos_ta in
        let ta16 = Brr.Tarray.of_buffer Brr.Tarray.Uint16 buf in
        Brr.Tarray.to_bigarray1 ta16
      in

      let colors = Brr.Tarray.to_bigarray1 col_ta in
      (* Uint8 matches *)

      let indices =
        let buf = Brr.Tarray.buffer idx_ta in
        let ta32 = Brr.Tarray.of_buffer Brr.Tarray.Int32 buf in
        Brr.Tarray.to_bigarray1 ta32
      in

      let water_positions, water_colors, water_indices =
        if Jv.is_null water_pos_jv then
          ( Array1.create int32 c_layout 0,
            Array1.create int8_unsigned c_layout 0,
            Array1.create int32 c_layout 0 )
        else
          let wp_ta = to_u8 water_pos_jv in
          let wc_ta = to_u8 water_col_jv in
          let wi_ta = to_u8 water_idx_jv in

          let wp =
            let buf = Brr.Tarray.buffer wp_ta in
            let ta32 = Brr.Tarray.of_buffer Brr.Tarray.Int32 buf in
            Brr.Tarray.to_bigarray1 ta32
          in
          let wc = Brr.Tarray.to_bigarray1 wc_ta in
          let wi =
            let buf = Brr.Tarray.buffer wi_ta in
            let ta32 = Brr.Tarray.of_buffer Brr.Tarray.Int32 buf in
            Brr.Tarray.to_bigarray1 ta32
          in
          (wp, wc, wi)
      in

      let pois =
        if header.poi_count > 0 then
          parse_pois header (Jv.get obj "poi_names") (Jv.get obj "poi_coords")
            (Jv.get obj "poi_elevs") (Jv.get obj "poi_types")
        else []
      in

      Lwt.return
        {
          header;
          positions;
          colors;
          indices;
          water_positions;
          water_colors;
          water_indices;
          pois;
        }
  | _ -> Lwt.fail (Failure "Invalid response type for CLC")

(* Load CLC tiles in parallel *)
let load_tiles ~lat ~lon ~size =
  let open Lwt.Syntax in
  let size_deg = float size /. 3600. in
  let dem_min_lat = lat -. (size_deg /. 2.) in
  let dem_max_lat = lat +. (size_deg /. 2.) in
  let dem_min_lon = lon -. (size_deg /. 2.) in
  let dem_max_lon = lon +. (size_deg /. 2.) in
  let dem_range_lon = dem_max_lon -. dem_min_lon in
  let dem_range_lat = dem_max_lat -. dem_min_lat in

  let min_tile_lat = int_of_float (floor dem_min_lat) in
  let max_tile_lat = int_of_float (floor dem_max_lat) in
  let min_tile_lon = int_of_float (floor dem_min_lon) in
  let max_tile_lon = int_of_float (floor dem_max_lon) in

  let tasks = ref [] in
  for tile_lat = min_tile_lat to max_tile_lat do
    for tile_lon = min_tile_lon to max_tile_lon do
      let path = tile_path (float tile_lat +. 0.5) (float tile_lon +. 0.5) in
      let task =
        Lwt.catch
          (fun () ->
            let* tile = load_full_clc_tile path in
            let tile_range_lon = 65535. /. tile.header.scale_x in
            let tile_range_lat = 65535. /. tile.header.scale_y in
            Lwt.return (Some (tile, tile_range_lon, tile_range_lat)))
          (fun _exn -> Lwt.return None)
      in
      tasks := task :: !tasks
    done
  done;

  let* results = Lwt.all !tasks in
  let tiles = List.filter_map (fun x -> x) results in

  Lwt.return (dem_min_lon, dem_min_lat, dem_range_lon, dem_range_lat, tiles)
