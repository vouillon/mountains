(*
https://registry.opendata.aws/copernicus-dem/

aws s3 cp  s3://copernicus-dem-30m/Copernicus_DSM_COG_10_N44_00_E006_00_DEM/Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif ~/tmp/relief

=====

https://developer.nvidia.com/gpugems/gpugems2/part-i-geometric-complexity/chapter-2-terrain-rendering-using-gpu-based-geometry
https://blogs.igalia.com/itoral/2016/10/13/opengl-terrain-renderer-rendering-the-terrain-mesh/
https://stackoverflow.com/questions/5281261/generating-a-normal-map-from-a-height-map

https://iquilezles.org/articles/fog/

Terrender: A Web-Based Multi-Resolution Terrain ...
RASTeR: Simple and efficient terrain rendering on the GPU
Top-Down View-Dependent Terrain Triangulation using the Octagon Metric.
Visualization of large terrains made easy.

http://app.geotiff.io/identify

=======================================

- draw from above using height values
- draw from above using normal map
- change viewpoint
*)

let read_int32 ch =
  let s = really_input_string ch 4 in
  String.get_int32_le s 0

let read_uint16 ch =
  let s = really_input_string ch 2 in
  String.get_uint16_le s 0

type entry = { typ : int; count : int32; offset : int32 }

let read_ifd_entry ch =
  let tag = read_uint16 ch in
  let typ = read_uint16 ch in
  let count = read_int32 ch in
  let offset = read_int32 ch in
  Format.eprintf "%d %d %ld %ld@." tag typ count offset;
  (tag, { typ; count; offset })

module IntMap = Map.Make (Int)

let read_ifd ch offset =
  seek_in ch (Int32.to_int offset);
  let n = read_uint16 ch in
  Format.eprintf "%d@." n;
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
    a.(i) <- read_int32 ch
  done;
  a

let decode_delta b w h =
  for i = 0 to h - 1 do
    let i = i * w in
    for j = 1 to w - 1 do
      Bytes.set b (i + j)
        (Char.chr
           ((Char.code (Bytes.get b (i + j))
            + Char.code (Bytes.get b (i + j - 1)))
           land 0xff))
    done
  done

let decode_fp b w h =
  decode_delta b (w * 4) h;
  let b' = Bytes.copy b in
  for i = 0 to h - 1 do
    let i = i * w * 4 in
    for j = 0 to w - 1 do
      for k = 0 to 3 do
        Bytes.set b' (i + (4 * j) + k) (Bytes.get b (i + ((4 - k - 1) * w) + j))
      done
    done
  done;
  b'

let read_tile ch tile_width tile_height tile_offsets tile_byte_counts i =
  Format.eprintf "%ld %ld@." tile_offsets.(i) tile_byte_counts.(i);
  seek_in ch (Int32.to_int tile_offsets.(i));
  let s = really_input_string ch (Int32.to_int tile_byte_counts.(i)) in
  let st = Zlib.inflate_init true in
  let b = Bytes.create (tile_width * tile_height * 4) in
  let ok, i, j =
    Zlib.inflate_string st s 0 (String.length s) b 0 (Bytes.length b)
      Zlib.Z_FINISH
  in
  assert ok;
  Format.eprintf "=> %b %d %d@." ok i j;
  Zlib.inflate_end st;
  let b = decode_fp b tile_width tile_height in
  let tile = Bigarray.(Array2.create Float32 C_layout) tile_height tile_width in
  for i = 0 to tile_height - 1 do
    for j = 0 to tile_width - 1 do
      tile.{i, j} <-
        Int32.float_of_bits (Bytes.get_int32_le b (4 * ((i * tile_width) + j)))
    done
  done;
  tile

type t = {
  width : int;
  height : int;
  tile_width : int;
  tile_height : int;
  tile_offsets : int32 array;
  tile_byte_counts : int32 array;
}

let read_info ch =
  let s = really_input_string ch 4 in
  assert (s = "II\042\000");
  let offset = read_int32 ch in
  Format.eprintf "%ld@." offset;
  let ifd = read_ifd ch offset in
  let width = ifd_uint16 256 ifd in
  let height = ifd_uint16 257 ifd in
  Format.eprintf "size: %dx%d@." width height;
  let tile_width = ifd_uint16 322 ifd in
  let tile_height = ifd_uint16 323 ifd in
  Format.eprintf "tile size: %dx%d@." tile_width tile_height;
  let tile_offsets = ifd_int32s 324 ifd ch in
  let tile_byte_counts = ifd_int32s 325 ifd ch in
  assert (ifd_uint16 258 ifd = 32);
  (* 32 bits per sample *)
  assert (ifd_uint16 259 ifd = 8);
  (* Deflate *)
  assert (ifd_uint16 277 ifd = 1);
  (* 1 sample per pixel *)
  assert (ifd_uint16 317 ifd = 3);
  (* Prediction: floating points *)
  { width; height; tile_width; tile_height; tile_offsets; tile_byte_counts }

(*
let () =
  let ch = open_in "Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif" in
  let { tile_width; tile_height; tile_offsets; tile_byte_counts; _ } =
    read_info ch
  in
  Graphics.open_graph " 1024x1024";
  for i = 0 to Array.length tile_offsets - 1 do
    let tile =
      read_tile ch tile_width tile_height tile_offsets tile_byte_counts i
    in
    for y = 1 to tile_height - 2 do
      for x = 1 to tile_width - 2 do
        (*
        let l = truncate (256. -. (tile.{y, x} /. 4000. *. 256.)) in
*)
        let nx = tile.{y, x - 1} -. tile.{y, x + 1} in
        let ny = tile.{y - 1, x} -. tile.{y + 1, x} in
        let nz = 2. *. 30. in
        (*
(-dhx, -dhy, d)

compute normal
scalar product with light direction
negative ==> black
*)
        let u =
          max 0.
            (-.(nx +. ny -. nz)
            /. sqrt 3.
            /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)))
          (*            (nz /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)))*)
        in
        let l = truncate (u *. 255.9) in
        let c = Graphics.rgb l l l in
        Graphics.set_color c;
        Graphics.plot x (tile_width - y - 1)
      done
    done;
    ignore (Graphics.read_key ())
  done
*)
