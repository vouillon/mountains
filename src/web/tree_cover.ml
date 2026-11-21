(* Tree cover over Europe from the EEA's Copernicus high-resolution layers
   (2018, 10 m): tree cover density (percent) and dominant leaf type (1
   broadleaved, 2 coniferous). Where the LIDAR HD canopy does not reach --
   beyond ~2.5 km, and everywhere outside France -- they say where forest
   stands and of which kind, in place of land cover drawn to 25 ha.

   Blocks of [size] samples tall around the location, in WGS84 so that the
   renderer maps the base grid's coordinate onto them linearly: at level 0 a
   sample every 0.6" (~19 m, ~20 km across), at level 1 every 2.4" (~75 m,
   ~80 km), and as many samples across as make the block square on the
   ground; snapped to a grid ([snap_deg]) so that nearby locations ask for
   the same ones.

   They are cut from our own cells (Corine/tree_cover_tiles.py, on R2 by
   [Web_utils.tiles_url]): 300" at level 0, 1200" at level 1, 500 samples a
   side, resampled from the EEA's image service as the app asked it before
   (bilinear density, nearest leaf type), a byte a sample -- the density in
   steps of 10% (15 for no data) in the low 4 bits, the leaf type in the next
   2 -- zlib-compressed behind 4 bytes of magic; a cell with no data is
   absent. The block is handed to the renderer as the service's tiled TIFFs
   were: each layer in tiles of [tile] samples. *)

let ( let* ) = Lwt.bind
let ( let+ ) x f = Lwt.map f x
let size = 1024
let tile = 128
let snap_deg level = if level = 0 then 0.05 else 0.2

(* Arcseconds per sample; cells of [cell_samples] samples a side, [per_degree]
   to a degree. *)
let step_arcsec level = if level = 0 then 0.6 else 2.4
let cell_samples = 500
let per_degree level = if level = 0 then 12 else 3

type t = {
  width : int;
  west : float;
  south : float;
  east : float;
  north : float;
  density : Brr.Tarray.uint8;
  leaf_type : Brr.Tarray.uint8;
  density_tiles : int array;
  leaf_type_tiles : int array;
}

(* The block as samples: its first column and the row past its last, counted
   from 0 degrees ([step_arcsec] each, rows from the south), and its width. *)
let samples ~level ~lat ~lon =
  let step = step_arcsec level in
  let snap = snap_deg level in
  let clat = Float.round (lat /. snap) *. snap
  and clon = Float.round (lon /. snap) *. snap in
  let half_width =
    int_of_float
      (Float.round (float size /. 2. /. Float.cos (clat *. Float.pi /. 180.)))
  in
  let col = int_of_float (Float.round (clon *. 3600. /. step))
  and row = int_of_float (Float.round (clat *. 3600. /. step)) in
  (col - half_width, row + (size / 2), 2 * half_width)

let bounds ~level ~lat ~lon =
  let west, north, width = samples ~level ~lat ~lon in
  let deg i = float i *. step_arcsec level /. 3600. in
  (deg west, deg (north - size), deg (west + width), deg north)

let block_id ~level ~lat ~lon =
  let w, s, _, _ = bounds ~level ~lat ~lon in
  Printf.sprintf "%d:%.4f,%.4f" level w s

let to_lwt f =
  let t, u = Lwt.task () in
  (Fut.await f @@ fun v -> Lwt.wakeup u v);
  t

let ( // ) x y =
  let q = x / y in
  if x mod y >= 0 then q else q - 1

let cell_url level row col =
  let per = per_degree level in
  let lat = row // per and lon = col // per in
  Web_utils.tiles_url
    (Printf.sprintf "tc%d/%c%02d_%c%03d_%d_%d.tc" level
       (if lat >= 0 then 'N' else 'S')
       (abs lat)
       (if lon >= 0 then 'E' else 'W')
       (abs lon)
       (row - (per * lat))
       (col - (per * lon)))

(* A cell's bytes, rows from the north; [None] for one that is absent (no
   data) or cannot be had. Not from the network before [network]. *)
let fetch_cell ~network cell =
  let open Brr_io.Fetch in
  let* cached = to_lwt (Web_utils.is_cached (Jstr.v cell)) in
  let* () = if cached then Lwt.return_unit else network in
  let* res =
    to_lwt (Web_utils.fetch_tile ~serve_not_found:true (Jstr.v cell))
  in
  match res with
  | Ok resp when Response.ok resp -> (
      let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
      match buf with
      | Error _ -> Lwt.return_none
      | Ok buf ->
          let all = Brr.Tarray.of_buffer Brr.Tarray.Uint8 buf in
          if Brr.Tarray.length all < 4 then Lwt.return_none
          else
            (* After the magic, zlib: inflated by the browser *)
            let body = Brr.Tarray.sub all ~start:4 in
            let stream =
              Jv.call
                (Jv.new' (Jv.get Jv.global "Blob")
                   [| Jv.of_jv_array [| Brr.Tarray.to_jv body |] |])
                "stream" [||]
            in
            let inflated =
              Jv.call stream "pipeThrough"
                [|
                  Jv.new'
                    (Jv.get Jv.global "DecompressionStream")
                    [| Jv.of_string "deflate" |];
                |]
            in
            let* out =
              to_lwt
                (Fut.of_promise ~ok:Fun.id
                   (Jv.call
                      (Jv.new' (Jv.get Jv.global "Response") [| inflated |])
                      "arrayBuffer" [||]))
            in
            Lwt.return
              (match out with
              | Ok b ->
                  let a =
                    Brr.Tarray.of_buffer Brr.Tarray.Uint8
                      (Brr.Tarray.Buffer.of_jv b)
                  in
                  if Brr.Tarray.length a = cell_samples * cell_samples then
                    Some a
                  else None
              | Error _ -> None))
  | Ok _ | Error _ -> Lwt.return_none

(* The block of [level] about [lat]/[lon], from its cells: each layer in
   tiles of [tile] samples, rows of tiles from the north, the last tile of a
   row padded out, at byte offset 1 + k tile^2 (the renderer takes an offset
   of 0 for a tile to leave out). No data where no cell has it: density 255,
   leaf type 0. *)
let assemble ~level ~lat ~lon cells =
  let west, north, width = samples ~level ~lat ~lon in
  let across = (width + tile - 1) / tile in
  let count = across * (size / tile) in
  let bytes = 1 + (count * tile * tile) in
  let density = Brr.Tarray.create Brr.Tarray.Uint8 bytes in
  let leaf_type = Brr.Tarray.create Brr.Tarray.Uint8 bytes in
  Brr.Tarray.fill 255 density;
  let d = Brr.Tarray.to_bigarray1 density
  and l = Brr.Tarray.to_bigarray1 leaf_type in
  let n = cell_samples in
  List.iter
    (fun ((crow, ccol), data) ->
      let src = Brr.Tarray.to_bigarray1 data in
      (* The cell's samples in the block: rows from the north *)
      let top = north - 1 - (((crow + 1) * n) - 1) in
      let left = (ccol * n) - west in
      for i = max 0 (-top) to min (n - 1) (size - 1 - top) do
        let r = top + i in
        let row_base =
          1 + ((r / tile * across * tile * tile) + (r mod tile * tile))
        in
        for j = max 0 (-left) to min (n - 1) (width - 1 - left) do
          let c = left + j in
          let o = row_base + (c / tile * tile * tile) + (c mod tile) in
          let b = Bigarray.Array1.unsafe_get src ((i * n) + j) in
          let q = b land 15 in
          Bigarray.Array1.unsafe_set d o (if q = 15 then 255 else q * 10);
          Bigarray.Array1.unsafe_set l o ((b lsr 4) land 3)
        done
      done)
    cells;
  let tiles = Array.init count (fun k -> 1 + (k * tile * tile)) in
  let w, s, e, nn = bounds ~level ~lat ~lon in
  {
    width;
    west = w;
    south = s;
    east = e;
    north = nn;
    density;
    leaf_type;
    density_tiles = tiles;
    leaf_type_tiles = tiles;
  }

(* Never fails: [None] when the cells could not all be had, in [timeout_s]
   from when [network] let them go to the network. *)
let fetch ?(timeout_s = 60.) ?(network = Lwt.return_unit) ~level ~lat ~lon () =
  let west, north, width = samples ~level ~lat ~lon in
  let n = cell_samples in
  let rows =
    List.init
      (((north - 1) // n) - ((north - size) // n) + 1)
      (fun k -> ((north - size) // n) + k)
  in
  let cols =
    List.init
      (((west + width - 1) // n) - (west // n) + 1)
      (fun k -> (west // n) + k)
  in
  let cells = List.concat_map (fun r -> List.map (fun c -> (r, c)) cols) rows in
  let all =
    let* got =
      Lwt.all
        (List.map
           (fun (r, c) ->
             let+ data = fetch_cell ~network (cell_url level r c) in
             Option.map (fun d -> ((r, c), d)) data)
           cells)
    in
    Lwt.return (Some (assemble ~level ~lat ~lon (List.filter_map Fun.id got)))
  in
  let timeout =
    let* () = network in
    let t, w = Lwt.task () in
    ignore
      (Brr.G.set_timeout
         ~ms:(truncate (timeout_s *. 1000.))
         (fun () -> Lwt.wakeup_later w None));
    t
  in
  Lwt.pick [ all; timeout ]
