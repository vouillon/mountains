(* DEM Loader - Loads compressed .dem tiles efficiently
   
   File format (DEM1):
   - Magic: "DEM1" (4 bytes)
   - Width: uint32 LE
   - Height: uint32 LE
   - Min elevation: float32 LE
   - Max elevation: float32 LE
   - High bytes compressed size: uint32 LE
   - Low bytes compressed size: uint32 LE
   - High bytes data (deflate compressed)
   - Low bytes data (deflate compressed)
*)

let ( let* ) = Lwt.bind

(* Floor division that works correctly for negative numbers *)
let ( // ) x y =
  let q = x / y in
  let r = x mod y in
  if r >= 0 then q else q - 1

(* Sub-tile size: each 1° DEM is split into 3x3 = 9 tiles of 1200x1200 *)
let sub_tile_size = 1200

(* RG8 heightmap type: stores u16 heights as high/low bytes *)
type heightmap = {
  data : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t;
  size : int;
}

(* Get height at (row, col) from RG8 heightmap as float meters *)
let get_height h row col =
  let low = h.data.{row, col * 2} in
  let high = h.data.{row, (col * 2) + 1} in
  let u16_val = (high lsl 8) lor low in
  (Float.of_int u16_val *. (9500.0 /. 65535.0)) -. 500.0

(* Get raw u16 bigarray for GPU texture upload.
   The texture should be created with RG8 format, width = size, height = size. *)
let get_texture_data h = h.data

(* Convert Lwt future to Lwt promise *)
let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
  t

(* External inflate function from inflate.js *)
external inflate_impl : Brr.Tarray.uint8 -> Jv.Promise.t = "inflate"

external inflate_into : Brr.Tarray.uint8 -> Brr.Tarray.uint8 -> Jv.Promise.t
  = "inflate_into"

(* Type alias for uint8 bigarray *)
type ba_uint8 =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Wasm = struct
  let instance = ref None
  let high_mem = ref None
  let low_mem = ref None
  let target_mem = ref None

  let create_memory pages =
    let opts = Jv.obj [| ("initial", Jv.of_int pages) |] in
    Jv.new' (Jv.get (Jv.get Jv.global "WebAssembly") "Memory") [| opts |]

  let get_ba mem =
    let buf = Jv.get mem "buffer" in
    Brr.Tarray.to_bigarray1
      (Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv buf))

  let load () =
    match !instance with
    | Some inst -> Lwt.return inst
    | None ->
        let* resp =
          to_lwt @@ Brr_io.Fetch.url (Jstr.v "decompress_tile.wasm")
        in
        let* buf =
          to_lwt
            (Brr_io.Fetch.Body.array_buffer
               (Brr_io.Fetch.Response.as_body resp))
        in

        let h_mem = create_memory 25 in
        let l_mem = create_memory 25 in
        let t_mem = create_memory 526 in

        high_mem := Some h_mem;
        low_mem := Some l_mem;
        target_mem := Some t_mem;

        let imports =
          Jv.obj
            [|
              ( "env",
                Jv.obj
                  [|
                    ("high_mem", h_mem);
                    ("low_mem", l_mem);
                    ("target_mem", t_mem);
                  |] );
            |]
        in

        let* res =
          to_lwt
            (Fut.of_promise
               ~ok:(fun x -> x)
               (Jv.call
                  (Jv.get Jv.global "WebAssembly")
                  "instantiate"
                  [| Brr.Tarray.Buffer.to_jv buf; imports |]))
        in
        let inst = Jv.get res "instance" in
        instance := Some inst;
        Lwt.return inst
end

let decode_mutex = Lwt_mutex.create ()

(* Convert typed array to bigarray for efficient access *)
let to_ba (arr : Brr.Tarray.uint8) : ba_uint8 = Brr.Tarray.to_bigarray1 arr

(* Read uint32 LE from bigarray at offset *)
let get_uint32_le_ba (arr : ba_uint8) offset =
  let b0 = Bigarray.Array1.get arr offset in
  let b1 = Bigarray.Array1.get arr (offset + 1) in
  let b2 = Bigarray.Array1.get arr (offset + 2) in
  let b3 = Bigarray.Array1.get arr (offset + 3) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

(* Read float32 LE from bigarray at offset *)
let get_float32_le_ba (arr : ba_uint8) offset =
  Int32.float_of_bits (Int32.of_int (get_uint32_le_ba arr offset))

(* Zigzag decode: maps unsigned int16 back to signed int16 *)
let zigzag_decode n = (n lsr 1) lxor -(n land 1)

(* Check if a given position is within the available data bounds *)
let in_range ~size ~min_lat ~max_lat ~min_lon ~max_lon ~lat ~lon =
  let min_lat' = truncate (lat *. 3600.) - (size / 2) in
  let min_lon' = truncate (lon *. 3600.) - (size / 2) in
  let max_lat' = min_lat + size - 1 in
  let max_lon' = min_lon + size - 1 in
  min_lat <= (min_lat' - 1) // 3600
  && (max_lat' - 1) // 3600 <= max_lat
  && min_lon <= min_lon' // 3600
  && max_lon' // 3600 <= max_lon

(* Path to a sub-tile file *)
let path ~lat ~lon ~row ~col =
  Printf.sprintf "data/N%02d_E%03d_%d_%d.dem" lat lon row col

(* Fetch a .dem file as uint8 typed array *)
let fetch_dem ~lat ~lon ~row ~col =
  let p = path ~lat ~lon ~row ~col in
  let open Brr_io.Fetch in
  let* resp = to_lwt @@ url (Jstr.v p) in
  let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
  Lwt.return Brr.Tarray.(of_buffer Uint8 buf)

(* Decode a .dem tile and write directly into target heightmap.
   dst_row/dst_col specify where in the target to write (tile origin).
   target_size is the size of the target heightmap (assumed square). *)
let decode_tile ~data ~dst_row ~dst_col ~target_size =
  (* Convert typed array to bigarray for efficient access *)
  let data_ba = to_ba data in

  (* Parse header *)
  let magic =
    String.init 4 (fun i -> Char.chr (Bigarray.Array1.get data_ba i))
  in
  if magic <> "DEM1" then failwith "Invalid DEM file magic";
  let width = get_uint32_le_ba data_ba 4 in
  let height = get_uint32_le_ba data_ba 8 in
  let _min_elev = get_float32_le_ba data_ba 12 in
  let _max_elev = get_float32_le_ba data_ba 16 in
  let high_size = get_uint32_le_ba data_ba 20 in
  let low_size = get_uint32_le_ba data_ba 24 in
  let header_size = 28 in

  (* Extract compressed data *)
  let high_compressed =
    Brr.Tarray.sub data ~start:header_size ~stop:(header_size + high_size)
  in
  let low_compressed =
    Brr.Tarray.sub data ~start:(header_size + high_size)
      ~stop:(header_size + high_size + low_size)
  in

  (* Decompress *)
  let* inst = Wasm.load () in
  let h_mem = Option.get !Wasm.high_mem in
  let l_mem = Option.get !Wasm.low_mem in

  (* Decompress and Decode - Protected by mutex as we use shared WASM memories *)
  Lwt_mutex.with_lock decode_mutex (fun () ->
      let l_ta =
        Brr.Tarray.of_buffer Brr.Tarray.Uint8
          (Brr.Tarray.Buffer.of_jv (Jv.get l_mem "buffer"))
      in
      let h_ta =
        Brr.Tarray.of_buffer Brr.Tarray.Uint8
          (Brr.Tarray.Buffer.of_jv (Jv.get h_mem "buffer"))
      in

      let* _ =
        to_lwt
          (Fut.of_promise ~ok:(fun x -> x) (inflate_into high_compressed h_ta))
      in
      let* _ =
        to_lwt
          (Fut.of_promise ~ok:(fun x -> x) (inflate_into low_compressed l_ta))
      in

      let func = Jv.get (Jv.get inst "exports") "decompress_simd" in
      ignore
        (Jv.apply func
           [|
             Jv.of_int 0;
             Jv.of_int 0;
             Jv.of_int 0;
             Jv.of_int dst_col;
             Jv.of_int (target_size - dst_row - height);
             Jv.of_int target_size;
             Jv.of_int target_size;
             Jv.of_int target_size;
             Jv.of_int width;
             Jv.of_int height;
           |]);
      Lwt.return_unit)

(* Load heightmap for a given center position and size.
   lat/lon are in degrees, size is in arcseconds (pixels). *)
let load ~lat ~lon ~size =
  (* Convert center to arcseconds and compute bounds *)
  let center_lat_arcsec = truncate (lat *. 3600.) in
  let center_lon_arcsec = truncate (lon *. 3600.) in
  let min_lat_arcsec = center_lat_arcsec - (size / 2) in
  let min_lon_arcsec = center_lon_arcsec - (size / 2) in
  let max_lat_arcsec = min_lat_arcsec + size - 1 in
  let max_lon_arcsec = min_lon_arcsec + size - 1 in

  Format.eprintf "DEM_LOADER: Loading %dx%d region centered at (%.4f, %.4f)@."
    size size lat lon;
  Format.eprintf "  Bounds: lat %d-%d, lon %d-%d arcsec@." min_lat_arcsec
    max_lat_arcsec min_lon_arcsec max_lon_arcsec;

  let* _inst = Wasm.load () in
  let heights_ba = Wasm.get_ba (Option.get !Wasm.target_mem) in
  let h_ba = Bigarray.Array1.sub heights_ba 0 (size * size * 2) in
  (* Reshape 1D WASM memory to 2D heightmap *)
  let heights =
    Bigarray.reshape_2 (Bigarray.genarray_of_array1 h_ba) size (size * 2)
  in
  (* Clear memory (optional, but good for predictability) *)
  Bigarray.Array1.fill h_ba 0;

  (* Determine which degree tiles we need *)
  let min_deg_lat = (min_lat_arcsec - 1) // 3600 in
  let max_deg_lat = (max_lat_arcsec - 1) // 3600 in
  let min_deg_lon = min_lon_arcsec // 3600 in
  let max_deg_lon = max_lon_arcsec // 3600 in

  Format.eprintf "  Degree tiles: lat %d-%d, lon %d-%d@." min_deg_lat
    max_deg_lat min_deg_lon max_deg_lon;

  (* Iterate over degree tiles and sub-tiles *)
  let rec iter_deg_lat deg_lat =
    if deg_lat > max_deg_lat then Lwt.return ()
    else
      let* () = iter_deg_lon deg_lat min_deg_lon in
      iter_deg_lat (deg_lat + 1)
  and iter_deg_lon deg_lat deg_lon =
    if deg_lon > max_deg_lon then Lwt.return ()
    else
      let* () = iter_subtiles deg_lat deg_lon in
      iter_deg_lon deg_lat (deg_lon + 1)
  and iter_subtiles deg_lat deg_lon =
    (* For each degree tile, determine which sub-tiles overlap our region.
       Note: The original 3601x3601 tiles had the bottom row (at the degree boundary)
       removed to make 3600x3600. So N45 covers lat 45+1/3600 to 46, not exactly 45 to 46.
       We add 1 arcsec offset to account for this. *)
    let tile_base_lat_arcsec = (deg_lat * 3600) + 1 in
    let tile_base_lon_arcsec = deg_lon * 3600 in

    (* Sub-tile bounds within the degree tile (in arcseconds from tile origin) *)
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

    let rec iter_sub_row sub_row =
      if sub_row > max_sub_row then Lwt.return ()
      else
        let* () = iter_sub_col sub_row min_sub_col in
        iter_sub_row (sub_row + 1)
    and iter_sub_col sub_row sub_col =
      if sub_col > max_sub_col then Lwt.return ()
      else
        let* () = load_subtile deg_lat deg_lon sub_row sub_col in
        iter_sub_col sub_row (sub_col + 1)
    and load_subtile deg_lat deg_lon sub_row sub_col =
      (* Calculate the geographic bounds of this sub-tile *)
      let subtile_min_lat_arcsec =
        tile_base_lat_arcsec + (sub_row * sub_tile_size)
      in
      let subtile_min_lon_arcsec =
        tile_base_lon_arcsec + (sub_col * sub_tile_size)
      in
      let subtile_max_lat_arcsec = subtile_min_lat_arcsec + sub_tile_size - 1 in
      let subtile_max_lon_arcsec = subtile_min_lon_arcsec + sub_tile_size - 1 in

      (* Calculate overlap with our requested region *)
      let overlap_min_lat = max min_lat_arcsec subtile_min_lat_arcsec in
      let overlap_max_lat = min max_lat_arcsec subtile_max_lat_arcsec in
      let overlap_min_lon = max min_lon_arcsec subtile_min_lon_arcsec in
      let overlap_max_lon = min max_lon_arcsec subtile_max_lon_arcsec in

      if overlap_min_lat > overlap_max_lat || overlap_min_lon > overlap_max_lon
      then Lwt.return () (* No overlap *)
      else begin
        Format.eprintf "  Loading sub-tile N%02d_E%03d_%d_%d@." deg_lat deg_lon
          sub_row sub_col;
        Format.eprintf "    subtile lat: %d-%d, lon: %d-%d@."
          subtile_min_lat_arcsec subtile_max_lat_arcsec subtile_min_lon_arcsec
          subtile_max_lon_arcsec;

        (* Fetch and decode the tile *)
        let* data =
          fetch_dem ~lat:deg_lat ~lon:deg_lon ~row:sub_row ~col:sub_col
        in

        (* Calculate destination position in target heightmap *)
        let dst_row = subtile_min_lat_arcsec - min_lat_arcsec in
        let dst_col = subtile_min_lon_arcsec - min_lon_arcsec in

        Format.eprintf "    dst_row=%d, dst_col=%d (target_size=%d)@." dst_row
          dst_col size;

        decode_tile ~data ~dst_row ~dst_col ~target_size:size
      end
    in
    iter_sub_row min_sub_row
  in

  let* () = iter_deg_lat min_deg_lat in

  Lwt.return { data = heights; size }

(* Prefetch tiles for a given region *)
let prefetch ~lat ~lon ~size =
  (* Convert center to arcseconds and compute bounds *)
  let center_lat_arcsec = truncate (lat *. 3600.) in
  let center_lon_arcsec = truncate (lon *. 3600.) in
  let min_lat_arcsec = center_lat_arcsec - (size / 2) in
  let min_lon_arcsec = center_lon_arcsec - (size / 2) in
  let max_lat_arcsec = min_lat_arcsec + size - 1 in
  let max_lon_arcsec = min_lon_arcsec + size - 1 in

  (* Determine which degree tiles we need *)
  let min_deg_lat = (min_lat_arcsec - 1) // 3600 in
  let max_deg_lat = (max_lat_arcsec - 1) // 3600 in
  let min_deg_lon = min_lon_arcsec // 3600 in
  let max_deg_lon = max_lon_arcsec // 3600 in

  Format.eprintf "DEM_LOADER: Prefetching tiles for lat %d-%d, lon %d-%d@."
    min_deg_lat max_deg_lat min_deg_lon max_deg_lon;

  (* Prefetch all sub-tiles in parallel *)
  let prefetch_tasks = ref [] in
  for deg_lat = min_deg_lat to max_deg_lat do
    let tile_base_lat_arcsec = deg_lat * 3600 in
    let tile_base_lon_arcsec_base = min_deg_lon * 3600 in
    for deg_lon = min_deg_lon to max_deg_lon do
      let tile_base_lon_arcsec =
        tile_base_lon_arcsec_base + ((deg_lon - min_deg_lon) * 3600)
      in
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
          let p = path ~lat:deg_lat ~lon:deg_lon ~row:sub_row ~col:sub_col in
          let task =
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
        done
      done
    done
  done;
  Lwt.join !prefetch_tasks
