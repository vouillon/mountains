(* clc_loader.ml - Load and rasterize CLC tiles for web viewer *)

open Bigarray

let ( let* ) = Lwt.bind

(* Convert Lwt future to Lwt promise *)
let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with
    | Ok v -> Lwt.wakeup u v
    | Error err -> Lwt.wakeup_exn u (Jv.Error err) );
  t

(* CLC tile header info *)
type clc_header = {
  count : int;
  total_verts : int;
  total_indices : int;
  water_count : int;
  water_verts : int;
  water_indices : int;
  poi_count : int;
  min_lon : float;
  min_lat : float;
  scale_x : float;
  scale_y : float;
  water_scale_x : float;
  water_scale_y : float;
  poi_scale_x : float;
  poi_scale_y : float;
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
  (* Water layer - stored as int32 for full 24-bit precision *)
  water_positions : (int32, int32_elt, c_layout) Array1.t;
  water_colors : (int, int8_unsigned_elt, c_layout) Array1.t;
  water_indices : (int32, int32_elt, c_layout) Array1.t;
  (* POI data *)
  pois : poi list;
}

(* Helper to parse header from bigarray: CLC6 (Corine/clc_file.ml), whose
   counts and scales are where CLC5 had them *)
let parse_header_ba ba =
  let get_s i len =
    String.init len (fun j -> Char.chr (Array1.get ba (i + j)))
  in
  if Array1.dim ba < 112 || get_s 0 4 <> "CLC6" then
    failwith "CLC tile: bad magic";

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

  let water_count = read_i32_be 16 in
  let water_verts = read_i32_be 20 in
  let water_indices = read_i32_be 24 in
  let poi_count = read_i32_be 28 in

  let min_lon = read_f64_le 32 in
  let min_lat = read_f64_le 40 in
  let scale_x = read_f64_le 48 in
  let scale_y = read_f64_le 56 in
  let water_scale_x = read_f64_le 64 in
  let water_scale_y = read_f64_le 72 in
  let poi_scale_x = read_f64_le 80 in
  let poi_scale_y = read_f64_le 88 in

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
  }

let ( // ) x y =
  let q = x / y in
  let r = x mod y in
  if r >= 0 then q else q - 1

(* The land cover comes in sub-cells of 600 arcseconds, [sub_cells] by [sub_cells] to
   a degree as the elevation (extract_tiles), the POIs on their own in cells
   of a degree. *)
let sub_cells = 6
let cell_arcsec ~pois = if pois then 3600 else 3600 / sub_cells

(* The path of a tile, [lat] and [lon] its south-west corner counted in
   tiles from 0°. *)
let tile_path ?(pois = false) lat lon =
  let cell lat lon =
    let lat_str = if lat >= 0 then "N" else "S" in
    let lon_str = if lon >= 0 then "E" else "W" in
    Printf.sprintf "%s%02d%s%03d" lat_str (abs lat) lon_str (abs lon)
  in
  if pois then Web_utils.tiles_url ("poi/" ^ cell lat lon ^ ".clc")
  else
    let lat_str = if lat >= 0 then "N" else "S" in
    let lon_str = if lon >= 0 then "E" else "W" in
    Web_utils.tiles_url
    @@ Printf.sprintf "clc/%s%02d_%s%03d_%d_%d.clc" lat_str
         (abs (lat // sub_cells))
         lon_str
         (abs (lon // sub_cells))
         (lat - (sub_cells * (lat // sub_cells)))
         (lon - (sub_cells * (lon // sub_cells)))

let rec parallel_iter min max f =
  if min > max then Lwt.return ()
  else Lwt.join [ f min; parallel_iter (min + 1) max f ]

let prefetch_tile ?pois ~lat ~lon () =
  let open Lwt.Syntax in
  let f = tile_path ?pois lat lon in
  let request = Brr_io.Fetch.Request.v (Jstr.v f) in
  let* cache =
    to_lwt
      (Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ())
         (Jstr.v Data_cache.name))
  in
  let* response = to_lwt (Brr_io.Fetch.Cache.match' cache request) in
  match response with
  | Some response when Brr_io.Fetch.Response.ok response -> Lwt.return ()
  | _ ->
      Format.eprintf "Prefetching CLC %s@." f;
      to_lwt (Brr_io.Fetch.Cache.add cache request)

let prefetch ?(pois = false) ~size ~lat ~lon () =
  let min_lat = Web_utils.arcsec_floor lat - (size / 2) in
  let min_lon = Web_utils.arcsec_floor lon - (size / 2) in
  let max_lat = min_lat + size - 1 in
  let max_lon = min_lon + size - 1 in
  let cell = cell_arcsec ~pois in
  let* () =
    parallel_iter ((min_lat - 1) // cell) ((max_lat - 1) // cell) @@ fun lat ->
    parallel_iter (min_lon // cell) (max_lon // cell) @@ fun lon ->
    (* Not every cell has POIs of its own (extract_tiles --pois), and one
       that has none is no reason to give up on the others. *)
    if pois then
      Lwt.catch
        (fun () -> prefetch_tile ~pois ~lat ~lon ())
        (fun _ -> Lwt.return_unit)
    else prefetch_tile ~lat ~lon ()
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
let load_full_clc_tile ~pois path_str =
  let open Lwt.Syntax in
  let* resp = to_lwt (Web_utils.fetch_tile (Jstr.v path_str)) in
  let* () =
    if Brr_io.Fetch.Response.ok resp then Lwt.return ()
    else
      Lwt.fail
        (Failure
           (Printf.sprintf "HTTP %d for %s"
              (Brr_io.Fetch.Response.status resp)
              path_str))
  in
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
        Worker_pool.post w
          (Worker_pool.Decode (Worker_pool.CLC { data = buf_jv; pois })))
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

      (* The page takes its POIs from the 1-degree POI tiles alone: those the
         land-cover sub-cells carry as well are neither decoded nor read. *)
      let pois =
        if pois && header.poi_count > 0 then
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

(* The window of [load_tiles] -- its corner and extent in degrees -- and the
   tiles it takes, counted from 0 degrees. *)
let window ~pois ~lat ~lon ~size =
  let size_deg = float size /. 3600. in
  let dem_min_lat = lat -. (size_deg /. 2.) in
  let dem_max_lat = lat +. (size_deg /. 2.) in
  let dem_min_lon = lon -. (size_deg /. 2.) in
  let dem_max_lon = lon +. (size_deg /. 2.) in
  let per_degree = float (3600 / cell_arcsec ~pois) in
  let tile v = int_of_float (floor (v *. per_degree)) in
  let cells = ref [] in
  for tile_lat = tile dem_min_lat to tile dem_max_lat do
    for tile_lon = tile dem_min_lon to tile dem_max_lon do
      cells := (tile_lat, tile_lon) :: !cells
    done
  done;
  ( ( dem_min_lon,
      dem_min_lat,
      dem_max_lon -. dem_min_lon,
      dem_max_lat -. dem_min_lat ),
    List.rev !cells )

(* Those tiles, in parallel *)
let load_cells ~pois cells =
  let open Lwt.Syntax in
  let range = float (cell_arcsec ~pois) /. 3600. in
  let tasks =
    List.map
      (fun (tile_lat, tile_lon) ->
        let path = tile_path ~pois tile_lat tile_lon in
        Lwt.catch
          (fun () ->
            let* tile = load_full_clc_tile ~pois path in
            Lwt.return (Some (tile, range, range)))
          (fun exn ->
            (* Not every tile is published (none for open sea), so a 404 is
               expected and stays silent; a decode or worker failure must be
               visible. *)
            (match exn with
            | Failure msg when String.starts_with ~prefix:"HTTP 404 " msg -> ()
            | _ ->
                Brr.Console.error
                  [
                    Jstr.v
                      (Printf.sprintf "CLC tile %s not loaded: %s" path
                         (Printexc.to_string exn));
                  ]);
            Lwt.return None))
      cells
  in
  let* results = Lwt.all tasks in
  Lwt.return (List.filter_map Fun.id results)

let load_tiles ?(pois = false) ~lat ~lon ~size () =
  let open Lwt.Syntax in
  let (min_lon, min_lat, range_lon, range_lat), cells =
    window ~pois ~lat ~lon ~size
  in
  let* tiles = load_cells ~pois cells in
  Lwt.return (min_lon, min_lat, range_lon, range_lat, tiles)

let load_progressive ~lat ~lon ~size
    ~inner:(i_min_lon, i_min_lat, i_max_lon, i_max_lat) =
  let open Lwt.Syntax in
  let (min_lon, min_lat, range_lon, range_lat), cells =
    window ~pois:false ~lat ~lon ~size
  in
  let per_degree = float sub_cells in
  let tile v = int_of_float (floor (v *. per_degree)) in
  let la0 = tile i_min_lat and la1 = tile i_max_lat in
  let lo0 = tile i_min_lon and lo1 = tile i_max_lon in
  let inner, outer =
    List.partition
      (fun (la, lo) -> la >= la0 && la <= la1 && lo >= lo0 && lo <= lo1)
      cells
  in
  (* The inner tiles are wanted either way: fetched while the cache is looked
     into for the others. *)
  let inner_tiles = load_cells ~pois:false inner in
  let* cached =
    Lwt.all
      (List.map
         (fun (la, lo) ->
           let t, u = Lwt.wait () in
           Fut.await
             (Web_utils.is_cached (Jstr.v (tile_path la lo)))
             (Lwt.wakeup u);
           t)
         outer)
  in
  let window = (min_lon, min_lat, range_lon, range_lat) in
  if List.for_all Fun.id cached then
    let* a = inner_tiles and* b = load_cells ~pois:false outer in
    Lwt.return (window, a @ b, None)
  else
    let* a = inner_tiles in
    let box =
      ( float lo0 /. per_degree,
        float la0 /. per_degree,
        float (lo1 + 1) /. per_degree,
        float (la1 + 1) /. per_degree )
    in
    Lwt.return (window, a, Some (box, fun () -> load_cells ~pois:false outer))
