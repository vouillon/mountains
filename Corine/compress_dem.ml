(* DEM Compression Experiment
   
   This tool compresses DEM data using an alternative scheme:
   1. Convert 32-bit floats to 16-bit integers (range: -500m to 9000m)
   2. Apply parallelogram predictor: Residue = Val - (Left + Up - UpLeft)
   3. Zigzag encode the signed residues to unsigned values
   4. Split into high byte and low byte streams
   5. Compress each stream separately with gzip
*)

(* --- TIFF Reading (simplified, synchronous version) --- *)

module IntMap = Map.Make (Int)

type tiff_info = {
  width : int;
  height : int;
  tile_width : int;
  tile_height : int;
  tile_offsets : int32 array;
  tile_byte_counts : int32 array;
}

let read_int32_le ch =
  let s = really_input_string ch 4 in
  String.get_int32_le s 0

let read_uint16_le ch =
  let s = really_input_string ch 2 in
  String.get_uint16_le s 0

type ifd_entry = { typ : int; count : int32; offset : int32 }

let read_ifd_entry ch =
  let tag = read_uint16_le ch in
  let typ = read_uint16_le ch in
  let count = read_int32_le ch in
  let offset = read_int32_le ch in
  (tag, { typ; count; offset })

let read_ifd ch offset =
  seek_in ch (Int32.to_int offset);
  let n = read_uint16_le ch in
  let m = ref IntMap.empty in
  for _ = 0 to n - 1 do
    let tag, v = read_ifd_entry ch in
    m := IntMap.add tag v !m
  done;
  !m

let ifd_uint16 tag ifd =
  let v = IntMap.find tag ifd in
  assert (v.typ = 3);
  assert (v.count = 1l);
  Int32.to_int v.offset

let ifd_int32s tag ifd ch =
  let v = IntMap.find tag ifd in
  let count = Int32.to_int v.count in
  assert (v.typ = 4);
  assert (count > 1);
  let a = Array.make count 0l in
  seek_in ch (Int32.to_int v.offset);
  for i = 0 to count - 1 do
    a.(i) <- read_int32_le ch
  done;
  a

let read_tiff_info ch =
  let s = really_input_string ch 4 in
  assert (s = "II\042\000");
  let offset = read_int32_le ch in
  let ifd = read_ifd ch offset in
  let width = ifd_uint16 256 ifd in
  let height = ifd_uint16 257 ifd in
  let tile_width = ifd_uint16 322 ifd in
  let tile_height = ifd_uint16 323 ifd in
  let tile_offsets = ifd_int32s 324 ifd ch in
  let tile_byte_counts = ifd_int32s 325 ifd ch in
  (* Verify expected format *)
  assert (ifd_uint16 258 ifd = 32);
  (* 32 bits per sample *)
  assert (ifd_uint16 259 ifd = 8);
  (* Deflate *)
  assert (ifd_uint16 277 ifd = 1);
  (* 1 sample per pixel *)
  assert (ifd_uint16 317 ifd = 3);
  (* Floating point prediction *)
  { width; height; tile_width; tile_height; tile_offsets; tile_byte_counts }

(* Decode horizontal delta prediction (TIFF predictor = 2 with float prediction) *)
let decode_delta b w h =
  for i = 0 to h - 1 do
    let row = i * w in
    for j = 1 to w - 1 do
      Bytes.set b (row + j)
        (Char.chr
           ((Char.code (Bytes.get b (row + j))
            + Char.code (Bytes.get b (row + j - 1)))
           land 0xff))
    done
  done

(* Undo the floating-point byte shuffling used by TIFF *)
let decode_fp b w h =
  decode_delta b (w * 4) h;
  let b' = Bytes.create (w * h * 4) in
  for i = 0 to h - 1 do
    let row = i * w * 4 in
    for j = 0 to w - 1 do
      for k = 0 to 3 do
        Bytes.set b'
          (row + (4 * j) + k)
          (Bytes.get b (row + ((4 - k - 1) * w) + j))
      done
    done
  done;
  b'

let read_tile ch info i =
  let { tile_width; tile_height; tile_offsets; tile_byte_counts; _ } = info in
  seek_in ch (Int32.to_int tile_offsets.(i));
  let compressed = really_input_string ch (Int32.to_int tile_byte_counts.(i)) in
  let b = Bytes.create (tile_width * tile_height * 4) in
  (* Inflate using zlib *)
  let st = Zlib.inflate_init true in
  let ok, i_consumed, j_produced =
    Zlib.inflate_string st compressed 0 (String.length compressed) b 0
      (Bytes.length b) Zlib.Z_FINISH
  in
  assert ok;
  assert (i_consumed = String.length compressed);
  assert (j_produced = Bytes.length b);
  Zlib.inflate_end st;
  (* Decode floating-point prediction *)
  let b = decode_fp b tile_width tile_height in
  (* Convert to float array *)
  let tile = Bigarray.(Array2.create Float32 C_layout) tile_height tile_width in
  for row = 0 to tile_height - 1 do
    for col = 0 to tile_width - 1 do
      tile.{row, col} <-
        Int32.float_of_bits
          (Bytes.get_int32_le b (4 * ((row * tile_width) + col)))
    done
  done;
  tile

(* Read all tiles and assemble into a single heightmap *)
let read_dem tiff_path =
  let ch = open_in tiff_path in
  let info = read_tiff_info ch in
  Printf.eprintf "DEM size: %dx%d, tile size: %dx%d\n%!" info.width info.height
    info.tile_width info.tile_height;
  let tiles_x = (info.width + info.tile_width - 1) / info.tile_width in
  let tiles_y = (info.height + info.tile_height - 1) / info.tile_height in
  Printf.eprintf "Tiles: %dx%d\n%!" tiles_x tiles_y;
  let heights =
    Bigarray.(Array2.create Float32 C_layout) info.height info.width
  in
  for ty = 0 to tiles_y - 1 do
    for tx = 0 to tiles_x - 1 do
      let tile_idx = (ty * tiles_x) + tx in
      Printf.eprintf "Reading tile %d/%d\r%!" (tile_idx + 1) (tiles_x * tiles_y);
      let tile = read_tile ch info tile_idx in
      (* Copy tile into the heightmap *)
      let dst_y = ty * info.tile_height in
      let dst_x = tx * info.tile_width in
      for row = 0 to info.tile_height - 1 do
        if dst_y + row < info.height then
          for col = 0 to info.tile_width - 1 do
            if dst_x + col < info.width then
              heights.{dst_y + row, dst_x + col} <- tile.{row, col}
          done
      done
    done
  done;
  Printf.eprintf "\n%!";
  close_in ch;
  (heights, info.width, info.height)

(* --- Compression Pipeline --- *)

(* Parameters for 16-bit quantization *)
let min_elevation = -500.0
let max_elevation = 9000.0
let scale = 65535.0 /. (max_elevation -. min_elevation)

(* Convert float elevation to 16-bit unsigned integer *)
let float_to_u16 f =
  let clamped = Float.min max_elevation (Float.max min_elevation f) in
  let scaled = (clamped -. min_elevation) *. scale in
  Int.min 65535 (Int.max 0 (Float.to_int scaled))

(* Parallelogram predictor: predicts current value from Left, Up, and UpLeft *)
let predict left up up_left = left + up - up_left

(* Zigzag encoding: maps signed int16 to unsigned int16 *)
let zigzag_encode n =
  let n = n land 0xFFFF in
  let signed = if n >= 0x8000 then n - 0x10000 else n in
  (signed lsl 1) lxor (signed asr 15) land 0xFFFF

(* Apply compression pipeline to a sub-region of the heightmap *)
let compress heights ~src_row ~src_col ~tile_width ~tile_height =
  Printf.eprintf "Converting to 16-bit...\n%!";
  (* Step 1: Convert to 16-bit *)
  let u16_data = Array.make (tile_width * tile_height) 0 in
  for row = 0 to tile_height - 1 do
    for col = 0 to tile_width - 1 do
      u16_data.((row * tile_width) + col) <-
        float_to_u16 heights.{src_row + row, src_col + col}
    done
  done;
  Printf.eprintf "Applying parallelogram predictor...\n%!";
  (* Step 2: Apply parallelogram predictor, producing signed residues *)
  let residues = Array.make (tile_width * tile_height) 0 in
  for row = 0 to tile_height - 1 do
    for col = 0 to tile_width - 1 do
      let idx = (row * tile_width) + col in
      let val_ = u16_data.(idx) in
      let predicted =
        if row = 0 && col = 0 then 0
        else if row = 0 then u16_data.(idx - 1)
        else if col = 0 then u16_data.(idx - tile_width)
        else
          predict
            u16_data.(idx - 1)
            u16_data.(idx - tile_width)
            u16_data.(idx - tile_width - 1)
      in
      residues.(idx) <- val_ - predicted
    done
  done;
  Printf.eprintf "Zigzag encoding...\n%!";
  (* Step 3: Zigzag encode *)
  let zigzag = Array.map zigzag_encode residues in
  Printf.eprintf "Splitting bytes...\n%!";
  (* Step 4: Split into high and low bytes *)
  let n = tile_width * tile_height in
  let high_bytes = Bytes.create n in
  let low_bytes = Bytes.create n in
  for i = 0 to n - 1 do
    let v = zigzag.(i) in
    Bytes.set high_bytes i (Char.chr ((v lsr 8) land 0xFF));
    Bytes.set low_bytes i (Char.chr (v land 0xFF))
  done;
  (high_bytes, low_bytes)

(* Compress bytes using zlib *)
let gzip_compress input =
  let input_pos = ref 0 in
  let len = Bytes.length input in
  let input_cb buf =
    let n = min (Bytes.length buf) (len - !input_pos) in
    if n > 0 then (
      Bytes.blit input !input_pos buf 0 n;
      input_pos := !input_pos + n);
    n
  in
  let out_buf = Buffer.create 1024 in
  let output_cb buf n = Buffer.add_subbytes out_buf buf 0 n in
  Zlib.compress input_cb output_cb;
  Buffer.contents out_buf

(* --- File Format --- *)

(* Magic: "DEM1" (4 bytes)
   Width: uint32 LE
   Height: uint32 LE
   Min elevation: float32 LE (for reconstruction)
   Max elevation: float32 LE
   High bytes compressed size: uint32 LE
   Low bytes compressed size: uint32 LE
   High bytes data (gzip)
   Low bytes data (gzip)
*)

let write_output path width height high_compressed low_compressed =
  let oc = open_out_bin path in
  output_string oc "DEM1";
  let buf = Bytes.create 4 in
  Bytes.set_int32_le buf 0 (Int32.of_int width);
  output_bytes oc buf;
  Bytes.set_int32_le buf 0 (Int32.of_int height);
  output_bytes oc buf;
  Bytes.set_int32_le buf 0 (Int32.bits_of_float min_elevation);
  output_bytes oc buf;
  Bytes.set_int32_le buf 0 (Int32.bits_of_float max_elevation);
  output_bytes oc buf;
  Bytes.set_int32_le buf 0 (Int32.of_int (String.length high_compressed));
  output_bytes oc buf;
  Bytes.set_int32_le buf 0 (Int32.of_int (String.length low_compressed));
  output_bytes oc buf;
  output_string oc high_compressed;
  output_string oc low_compressed;
  close_out oc

(* Compress and write a single tile *)
let process_tile heights ~src_row ~src_col ~tile_width ~tile_height output_path
    =
  Printf.eprintf "Processing tile at (%d, %d) -> %s\n%!" src_row src_col
    output_path;
  let high_bytes, low_bytes =
    compress heights ~src_row ~src_col ~tile_width ~tile_height
  in
  Printf.eprintf "Gzip compressing high bytes (%d bytes)...\n%!"
    (Bytes.length high_bytes);
  let high_compressed = gzip_compress high_bytes in
  Printf.eprintf "  -> %d bytes\n%!" (String.length high_compressed);
  Printf.eprintf "Gzip compressing low bytes (%d bytes)...\n%!"
    (Bytes.length low_bytes);
  let low_compressed = gzip_compress low_bytes in
  Printf.eprintf "  -> %d bytes\n%!" (String.length low_compressed);
  write_output output_path tile_width tile_height high_compressed low_compressed;
  Printf.eprintf "Compressed: %d + %d = %d bytes (%.2f%%)\n%!"
    (String.length high_compressed)
    (String.length low_compressed)
    (String.length high_compressed + String.length low_compressed)
    (100.0
    *. Float.of_int
         (String.length high_compressed + String.length low_compressed)
    /. Float.of_int (tile_width * tile_height * 4))

(* Tile size: 3600 / 3 = 1200 *)
let sub_tile_size = 1200

(* Parse lat/lon from Copernicus DEM filename like:
   Copernicus_DSM_COG_10_N45_00_E006_00_DEM.tif -> (45, 6) *)
let parse_lat_lon filename =
  let basename = Filename.basename filename in
  (* Match pattern: N{lat}_00_E{lon}_00 or similar *)
  try
    let re = Str.regexp "N\\([0-9]+\\)_[0-9]+_E\\([0-9]+\\)_[0-9]+" in
    let _ = Str.search_forward re basename 0 in
    let lat = int_of_string (Str.matched_group 1 basename) in
    let lon = int_of_string (Str.matched_group 2 basename) in
    Some (lat, lon)
  with Not_found -> None

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: %s <input.tif> <output_dir>\n" Sys.argv.(0);
    Printf.eprintf
      "  Generates 9 tiles in output_dir: N{lat}_E{lon}_{row}_{col}.dem\n";
    Printf.eprintf
      "  Lat/lon are parsed from input filename (Copernicus format)\n";
    Printf.eprintf "  Row 0 = bottom (south), Col 0 = left (west)\n";
    exit 1
  end;
  let input_path = Sys.argv.(1) in
  let output_dir = Sys.argv.(2) in

  (* Parse lat/lon from input filename *)
  let lat, lon =
    match parse_lat_lon input_path with
    | Some (lat, lon) -> (lat, lon)
    | None ->
        Printf.eprintf "Error: Could not parse lat/lon from filename: %s\n"
          input_path;
        Printf.eprintf
          "Expected format like: Copernicus_DSM_COG_10_N45_00_E006_00_DEM.tif\n";
        exit 1
  in
  Printf.eprintf "Parsed coordinates: N%02d E%03d\n%!" lat lon;

  Printf.eprintf "Reading %s...\n%!" input_path;
  let heights, width, height = read_dem input_path in
  Printf.eprintf "DEM size: %dx%d\n%!" width height;
  if width <> 3600 || height <> 3600 then begin
    Printf.eprintf "Error: Expected 3600x3600 DEM, got %dx%d\n" width height;
    exit 1
  end;
  Printf.eprintf "Splitting into 9 tiles of %dx%d...\n%!" sub_tile_size
    sub_tile_size;
  (* Iterate over 3x3 grid of tiles.
     Row 0 = bottom (south) = rows 2400-3599 in the heightmap (since row 0 in
     heightmap is north). Col 0 = left (west) = cols 0-1199 in the heightmap. *)
  for tile_row = 0 to 2 do
    for tile_col = 0 to 2 do
      (* Map tile coordinates to heightmap coordinates.
         tile_row 0 (south) -> src_row = 2400 (rows 2400-3599)
         tile_row 2 (north) -> src_row = 0 (rows 0-1199) *)
      let src_row = (2 - tile_row) * sub_tile_size in
      let src_col = tile_col * sub_tile_size in
      let output_path =
        Printf.sprintf "%s/N%02d_E%03d_%d_%d.dem" output_dir lat lon tile_row
          tile_col
      in
      process_tile heights ~src_row ~src_col ~tile_width:sub_tile_size
        ~tile_height:sub_tile_size output_path
    done
  done;
  Printf.eprintf "Done! Generated 9 tiles.\n%!"
