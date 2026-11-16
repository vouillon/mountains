(* Land cover tiles (.clc) on disk.

   CLC6, which extract_tiles writes:

     "CLC6"
     7 big-endian int32: land features, vertices, indices; water features,
       vertices, indices; POIs
     8 little-endian float64: min_lon, min_lat, scale_x, scale_y (land),
       water_scale_x, water_scale_y, poi_scale_x, poi_scale_y
     4 big-endian int32: land arcs, land longest walk; water arcs, water
       longest walk (what the decoder allocates)
     22 streams, each a little-endian u32 of its compressed length, one of
       its length once inflated, and the zlib data (none for an empty stream):
       land (Clc_codec, 16-bit coordinates): codes, topology, arcs, x high,
         x low, y high, y low, connectivity
       water (24-bit): codes, topology, arcs, x high, x middle, x low, y high,
         y middle, y low, connectivity
       POIs: names, coordinates, elevations, types (Poi_encoder)

   The header up to the floats is CLC5's. CLC5, which came before, is read
   to convert it: per feature its vertices and indices as deltas, in
   streams of a u32 length and zlib data. *)

let compress ?(level = 9) str =
  let pos = ref 0 in
  let input buf =
    let n = min (Bytes.length buf) (String.length str - !pos) in
    Bytes.blit_string str !pos buf 0 n;
    pos := !pos + n;
    n
  in
  let out = Buffer.create 1024 in
  Zlib.compress ~level input (fun buf n -> Buffer.add_subbytes out buf 0 n);
  Buffer.contents out

let uncompress str =
  let pos = ref 0 in
  let input buf =
    let n = min (Bytes.length buf) (String.length str - !pos) in
    Bytes.blit_string str !pos buf 0 n;
    pos := !pos + n;
    n
  in
  let out = Buffer.create (4 * String.length str) in
  Zlib.uncompress input (fun buf n -> Buffer.add_subbytes out buf 0 n);
  Buffer.contents out

type tile = {
  floats : string; (* the eight floats, as they are *)
  cover : Clc_codec.feature array;
  water : Clc_codec.feature array;
  poi_count : int;
  pois : string list; (* the four POI streams, inflated *)
}

let be32 s p = Int32.to_int (String.get_int32_be s p)
let le32 s p = Int32.to_int (String.get_int32_le s p) land 0xFFFFFFFF

let read_file path =
  let ic = open_in_bin path in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;
  data

(* CLC5: per feature, a class code, vertex and triangle counts (u16 LE);
   the vertices as deltas from the previous one (from 0 for the first),
   zigzagged, in a stream per byte; the indices likewise, from 0 per
   feature. *)
let read_clc5_layer ~bytes streams =
  let meta, rest =
    match streams with
    | meta :: rest -> (meta, rest)
    | [] -> invalid_arg "read_clc5_layer"
  in
  let xs = List.filteri (fun i _ -> i < bytes) rest
  and ys = List.filteri (fun i _ -> i >= bytes && i < 2 * bytes) rest
  and hi = List.nth rest (2 * bytes)
  and lo = List.nth rest ((2 * bytes) + 1) in
  let mask = (1 lsl (8 * bytes)) - 1 in
  let unzig z = (z lsr 1) lxor -(z land 1) in
  let value streams i =
    List.fold_left (fun z s -> (z lsl 8) lor Char.code s.[i]) 0 streams
  in
  let u16 i = Char.code meta.[i] lor (Char.code meta.[i + 1] lsl 8) in
  let vp = ref 0 and ip = ref 0 in
  Array.init
    (String.length meta / 6)
    (fun f ->
      let code = u16 (6 * f)
      and nv = u16 ((6 * f) + 2)
      and nt = u16 ((6 * f) + 4) in
      let px = ref 0 and py = ref 0 in
      let fxs = Array.make nv 0 and fys = Array.make nv 0 in
      for k = 0 to nv - 1 do
        px := (!px + unzig (value xs (!vp + k))) land mask;
        py := (!py + unzig (value ys (!vp + k))) land mask;
        fxs.(k) <- !px;
        fys.(k) <- !py
      done;
      vp := !vp + nv;
      let pi = ref 0 in
      let tris =
        Array.init (3 * nt) (fun k ->
            let z = (Char.code hi.[!ip + k] lsl 8) lor Char.code lo.[!ip + k] in
            pi := (!pi + unzig z) land 0xFFFF;
            !pi)
      in
      ip := !ip + (3 * nt);
      { Clc_codec.code; xs = fxs; ys = fys; tris })

let read_clc5 data =
  let pos = ref 96 in
  let streams =
    List.init 20 (fun _ ->
        let len = le32 data !pos in
        let s = String.sub data (!pos + 4) len in
        pos := !pos + 4 + len;
        if len = 0 then "" else uncompress s)
  in
  if !pos <> String.length data then failwith "CLC5: trailing data";
  let part a n = List.filteri (fun i _ -> i >= a && i < a + n) streams in
  let cover = read_clc5_layer ~bytes:2 (part 0 7) in
  let water = read_clc5_layer ~bytes:3 (part 7 9) in
  if
    Array.length cover <> be32 data 4
    || Array.length water <> be32 data 16
    || Array.fold_left (fun a f -> a + Array.length f.Clc_codec.xs) 0 cover
       <> be32 data 8
    || Array.fold_left (fun a f -> a + Array.length f.Clc_codec.xs) 0 water
       <> be32 data 20
  then failwith "CLC5: counts do not match the header";
  {
    floats = String.sub data 32 64;
    cover;
    water;
    poi_count = be32 data 28;
    pois = part 16 4;
  }

let read_clc6 data =
  let pos = ref 112 in
  let streams =
    List.init 22 (fun _ ->
        let len = le32 data !pos and raw = le32 data (!pos + 4) in
        let s =
          if len = 0 then "" else uncompress (String.sub data (!pos + 8) len)
        in
        if String.length s <> raw then failwith "CLC6: a stream's length";
        pos := !pos + 8 + len;
        s)
  in
  if !pos <> String.length data then failwith "CLC6: trailing data";
  let part a n = List.filteri (fun i _ -> i >= a && i < a + n) streams in
  {
    floats = String.sub data 32 64;
    cover = Clc_codec.decode ~bits:16 ~count:(be32 data 4) (part 0 8);
    water = Clc_codec.decode ~bits:24 ~count:(be32 data 16) (part 8 10);
    poi_count = be32 data 28;
    pois = part 18 4;
  }

let read path =
  let data = read_file path in
  match String.sub data 0 4 with
  | "CLC5" -> read_clc5 data
  | "CLC6" -> read_clc6 data
  | _ -> failwith (path ^ ": not a CLC5 or CLC6 tile")

(* Written to a temporary file renamed at the end, so that an interrupted
   run cannot leave a plausible-looking partial tile behind. Returns the
   compressed length of each stream. *)
let write path tile =
  let cover = Clc_codec.encode ~bits:16 tile.cover in
  let water = Clc_codec.encode ~bits:24 tile.water in
  let oc = open_out_bin (path ^ ".tmp") in
  let be v =
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 (Int32.of_int v);
    output_bytes oc b
  and le v =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 (Int32.of_int v);
    output_bytes oc b
  in
  output_string oc "CLC6";
  List.iter be
    [
      cover.count;
      cover.verts;
      cover.indices;
      water.count;
      water.verts;
      water.indices;
      tile.poi_count;
    ];
  output_string oc tile.floats;
  List.iter be [ cover.arcs; cover.max_walk; water.arcs; water.max_walk ];
  let sizes =
    List.map
      (fun s ->
        let c = if s = "" then "" else compress s in
        le (String.length c);
        le (String.length s);
        output_string oc c;
        String.length c)
      (cover.streams @ water.streams @ tile.pois)
  in
  close_out oc;
  Sys.rename (path ^ ".tmp") path;
  sizes
