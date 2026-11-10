(* Tree cover over Europe from the EEA's Copernicus high-resolution layers
   (2018, 10 m): tree cover density (percent) and dominant leaf type (1
   broadleaved, 2 coniferous). Where the LIDAR HD canopy does not reach --
   beyond ~2.5 km, and everywhere outside France -- they say where forest
   stands and of which kind, in place of land cover drawn to 25 ha.

   Blocks of [size] samples square around the location, asked for in WGS84 so
   that the renderer maps the base grid's coordinate onto them linearly: at
   level 0 ~20 m each (~20 km across), at level 1 ~80 m (~80 km), for the
   distance; snapped to a grid ([snap_deg]) so that nearby locations ask for
   the same ones (the service lets browsers cache them for 12 hours). The service
   resamples from its own grids (Web Mercator, ETRS89-LAEA). It answers raw
   values only as TIFF -- PNG comes out through its colour rendering -- here
   8-bit, uncompressed, in tiles, which [parse] locates; the renderer uploads
   them straight from the response. *)

let ( let* ) = Lwt.bind
let ( and* ) = Lwt.both
let size = 1024
let tile = 128
let snap_deg level = if level = 0 then 0.05 else 0.2

(* Degrees per sample north-south: ~20 m at level 0, ~80 m at level 1.
   East-west, the same metres at the block's latitude. *)
let lat_step level = if level = 0 then 0.00018 else 0.00072

type t = {
  west : float;
  south : float;
  east : float;
  north : float;
  density : Brr.Tarray.uint8;
  leaf_type : Brr.Tarray.uint8;
  density_tiles : int array;
  leaf_type_tiles : int array;
}

let bounds ~level ~lat ~lon =
  let snap = snap_deg level in
  let clat = Float.round (lat /. snap) *. snap
  and clon = Float.round (lon /. snap) *. snap in
  let half_lat = lat_step level *. float size /. 2. in
  let half_lon = half_lat /. Float.cos (clat *. Float.pi /. 180.) in
  (clon -. half_lon, clat -. half_lat, clon +. half_lon, clat +. half_lat)

let block_id ~level ~lat ~lon =
  let w, s, _, _ = bounds ~level ~lat ~lon in
  Printf.sprintf "%d:%.4f,%.4f" level w s

let request_url ~service ~interpolation (w, s, e, n) =
  Printf.sprintf
    "https://image.discomap.eea.europa.eu/arcgis/rest/services/GioLandPublic/%s/ImageServer/exportImage?bbox=%.6f,%.6f,%.6f,%.6f&bboxSR=4326&imageSR=4326&size=%d,%d&format=tiff&pixelType=U8&interpolation=%s&f=image"
    service w s e n size size interpolation

let to_lwt f =
  let t, u = Lwt.task () in
  (Fut.await f @@ fun v -> Lwt.wakeup u v);
  t

(* The byte offset of each tile of a little-endian, uncompressed, 8-bit,
   [size]-square TIFF tiled by [tile], rows of tiles from the north; 0 for a
   tile the file leaves out (empty). [None] for any other layout. *)
let parse (b : Brr.Tarray.uint8) =
  let a = Brr.Tarray.to_bigarray1 b in
  let n = Bigarray.Array1.dim a in
  let u8 i = if i < n then a.{i} else 0 in
  let u16 i = u8 i lor (u8 (i + 1) lsl 8) in
  let u32 i = u16 i lor (u16 (i + 2) lsl 16) in
  if n < 8 || u16 0 <> 0x4949 || u16 2 <> 42 then None
  else
    let ifd = u32 4 in
    let entries = u16 ifd in
    let tag t =
      let rec find k =
        if k = entries then None
        else
          let e = ifd + 2 + (12 * k) in
          if u16 e = t then Some (u16 (e + 2), u32 (e + 4), e + 8)
          else find (k + 1)
      in
      find 0
    in
    let scalar t =
      match tag t with
      | Some (3, 1, v) -> Some (u16 v)
      | Some (4, 1, v) -> Some (u32 v)
      | _ -> None
    in
    let tiles_per_axis = size / tile in
    let count = tiles_per_axis * tiles_per_axis in
    match
      ( scalar 256,
        scalar 257,
        scalar 258,
        scalar 259,
        scalar 322,
        scalar 323,
        tag 324 )
    with
    | ( Some w,
        Some h,
        Some 8,
        Some 1,
        Some tw,
        Some th,
        Some (((4 | 3) as typ), c, v) )
      when w = size && h = size && tw = tile && th = tile && c = count ->
        let at = u32 v in
        let offsets =
          Array.init count (fun k ->
              if typ = 4 then u32 (at + (4 * k)) else u16 (at + (2 * k)))
        in
        if Array.for_all (fun o -> o = 0 || o + (tile * tile) <= n) offsets then
          Some offsets
        else None
    | _ -> None

let fetch_layer ~service ~interpolation box =
  let open Brr_io.Fetch in
  let* res = to_lwt (url (Jstr.v (request_url ~service ~interpolation box))) in
  match res with
  | Ok resp when Response.ok resp -> (
      let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
      match buf with
      | Ok buf ->
          let b = Brr.Tarray.of_buffer Brr.Tarray.Uint8 buf in
          Lwt.return (Option.map (fun tiles -> (b, tiles)) (parse b))
      | Error _ -> Lwt.return None)
  | Ok _ | Error _ -> Lwt.return None

(* Never fails: [None] when either layer could not be had, in [timeout_s]. *)
let fetch ?(timeout_s = 60.) ~level ~lat ~lon () =
  let ((west, south, east, north) as box) = bounds ~level ~lat ~lon in
  let both =
    let* d =
      fetch_layer ~service:"HRL_TreeCoverDensity_2018"
        ~interpolation:"RSP_BilinearInterpolation" box
    and* l =
      fetch_layer ~service:"HRL_DominantLeafType2018"
        ~interpolation:"RSP_NearestNeighbor" box
    in
    match (d, l) with
    | Some (density, density_tiles), Some (leaf_type, leaf_type_tiles) ->
        Lwt.return
          (Some
             {
               west;
               south;
               east;
               north;
               density;
               leaf_type;
               density_tiles;
               leaf_type_tiles;
             })
    | _ ->
        Format.eprintf "Tree cover: could not be fetched@.";
        Lwt.return None
  in
  let timeout =
    let t, w = Lwt.task () in
    ignore
      (Brr.G.set_timeout
         ~ms:(truncate (timeout_s *. 1000.))
         (fun () -> Lwt.wakeup_later w None));
    t
  in
  Lwt.pick [ both; timeout ]
