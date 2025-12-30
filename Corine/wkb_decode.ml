(* wkb_decode.ml *)

type point = { x : float; y : float }

type geometry =
  | Point of point
  | Polygon of point list list (* List of rings (outer + holes) *)
  | MultiPolygon of point list list list (* List of Polygons *)
  | Unknown of int * string

(* --- Bounding Box --- *)

type bbox = { min_x : float; min_y : float; max_x : float; max_y : float }

let empty_bbox =
  {
    min_x = infinity;
    min_y = infinity;
    max_x = neg_infinity;
    max_y = neg_infinity;
  }

let combine_bbox b p =
  {
    min_x = min b.min_x p.x;
    min_y = min b.min_y p.y;
    max_x = max b.max_x p.x;
    max_y = max b.max_y p.y;
  }

let get_bbox geom =
  let rec from_points points acc = List.fold_left combine_bbox acc points in
  let rec from_rings rings acc =
    List.fold_left (fun acc ring -> from_points ring acc) acc rings
  in
  match geom with
  | Point p -> combine_bbox empty_bbox p
  | Polygon rings -> from_rings rings empty_bbox
  | MultiPolygon polys ->
      List.fold_left (fun acc rings -> from_rings rings acc) empty_bbox polys
  | Unknown _ -> empty_bbox

(* --- Binary Reading Helpers --- *)

type cursor = {
  data : string;
  mutable pos : int;
  mutable is_le : bool; (* Little Endian? *)
  limit : int;
}

let make_cursor ?(pos = 0) data =
  { data; pos; is_le = true; limit = String.length data }

let check_bounds c n =
  if c.pos + n > c.limit then failwith "Unexpected end of WKB data"

let read_byte c =
  check_bounds c 1;
  let b = Char.code c.data.[c.pos] in
  c.pos <- c.pos + 1;
  b

let read_int32 c =
  check_bounds c 4;
  let pos = c.pos in
  c.pos <- c.pos + 4;
  let b0 = Char.code c.data.[pos] in
  let b1 = Char.code c.data.[pos + 1] in
  let b2 = Char.code c.data.[pos + 2] in
  let b3 = Char.code c.data.[pos + 3] in
  if c.is_le then (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0
  else (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let read_float c =
  check_bounds c 8;
  let pos = c.pos in
  c.pos <- c.pos + 8;
  (* Read 8 bytes into an Int64, then convert to float *)
  let read_byte_at i = Int64.of_int (Char.code c.data.[pos + i]) in
  let i64 =
    if c.is_le then
      List.fold_left
        (fun acc i ->
          Int64.logor acc (Int64.shift_left (read_byte_at i) (i * 8)))
        0L [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    else
      List.fold_left
        (fun acc i -> Int64.logor (Int64.shift_left acc 8) (read_byte_at i))
        0L [ 0; 1; 2; 3; 4; 5; 6; 7 ]
  in
  Int64.float_of_bits i64

(* --- Geometry Parsers --- *)

(* Checks WKB type for Z/M presence and returns base type *)
let parse_type_info raw_type =
  let wkbZ = 0x80000000 in
  let wkbM = 0x40000000 in

  (* let wkbSRID = 0x20000000 in - Unused *)
  (* Only in EWKB, but good to know *)

  (* Handle ISO WKB Z/M values (1000 range) vs Extended WKB *)
  let base_type = raw_type land 0xFFFF in
  let has_z =
    raw_type land wkbZ <> 0 || (base_type >= 1000 && base_type < 2000)
  in
  let has_m =
    raw_type land wkbM <> 0 || (base_type >= 2000 && base_type < 3000)
  in

  (* Normalized base type (Point=1, Poly=3, etc.) *)
  let norm_type =
    if base_type >= 1000 && base_type < 2000 then base_type - 1000
    else if base_type >= 2000 && base_type < 3000 then base_type - 2000
    else if base_type >= 3000 then base_type - 3000 (* ZM *)
    else base_type
  in
  (norm_type, has_z, has_m)

let parse_point c has_z has_m =
  let x = read_float c in
  let y = read_float c in
  if has_z then ignore (read_float c);
  (* Skip Z *)
  if has_m then ignore (read_float c);
  (* Skip M *)
  { x; y }

let parse_linear_ring c has_z has_m =
  let num_points = read_int32 c in
  let rec loop n acc =
    if n <= 0 then List.rev acc
    else loop (n - 1) (parse_point c has_z has_m :: acc)
  in
  loop num_points []

let parse_polygon c has_z has_m =
  let num_rings = read_int32 c in
  let rec loop n acc =
    if n <= 0 then List.rev acc
    else loop (n - 1) (parse_linear_ring c has_z has_m :: acc)
  in
  loop num_rings []

(* Recursive parser *)
let rec parse_geometry_body c =
  let byte_order = read_byte c in
  c.is_le <- byte_order = 1;

  let raw_type = read_int32 c in
  let base_type, has_z, has_m = parse_type_info raw_type in

  match base_type with
  | 1 ->
      (* Point *)
      Point (parse_point c has_z has_m)
  | 3 ->
      (* Polygon *)
      Polygon (parse_polygon c has_z has_m)
  | 6 ->
      (* MultiPolygon *)
      let num_polys = read_int32 c in
      let rec loop n acc =
        if n <= 0 then MultiPolygon (List.rev acc)
        else
          match parse_geometry_body c with
          | Polygon p -> loop (n - 1) (p :: acc)
          | _ -> failwith "MultiPolygon must contain Polygons"
      in
      loop num_polys []
  (* Handle MultiSurface etc if they map to similar structures, 
     but standard WKB usually explicitly nests them. 
     For now, 1, 3, 6 are the main ones in CLC. *)
  | _ -> Unknown (base_type, "Unsupported Geometry Type")

(* --- Main Decoding --- *)

(* 
  GeoPackage Binary Header:
  - Magic (2 bytes): 0x47 0x50 ('GP')
  - Version (1 byte): 0x00
  - Flags (1 byte): 
      - bit 0: BinaryType (0=Standard, 1=Extended)
      - bit 1-3: GeometryEmpty (test), EnvelopeType (0-4)
      - bit 4: IsEmpty
      - bit 5: StandardType (0=Standard, 1=Extended)
  - SRS_ID (4 bytes)
  - Envelope (0, 32, 48, or 64 bytes depending on EnvelopeType)
*)

let decode_gpkg_header c =
  let start_pos = c.pos in
  (* Check for "GP" magic *)
  if
    c.limit - start_pos >= 8
    && c.data.[start_pos] = 'G'
    && c.data.[start_pos + 1] = 'P'
  then (
    c.pos <- start_pos + 2;
    let _version = read_byte c in
    (* Should be 0 *)
    let flags = read_byte c in

    (* Force little endian for the header integers as per GPKG spec *)
    let _old_le = c.is_le in
    c.is_le <- true;
    let _srid = read_int32 c in

    let envelope_indicator = (flags lsr 1) land 0x07 in
    let envelope_len =
      match envelope_indicator with
      | 1 -> 32 (* 2D: minx, maxx, miny, maxy *)
      | 2 -> 48 (* Z: + minz, maxz *)
      | 3 -> 48 (* M: + minm, maxm *)
      | 4 -> 64 (* ZM: + minz, maxz, minm, maxm *)
      | _ -> 0 (* 0=No envelope *)
    in
    c.pos <- c.pos + envelope_len
    (* Restoration of endianness is not needed as WKB body has its own byte order byte *))
  else
    (* Not a GPKG header, reset to start assuming pure WKB *)
    c.pos <- start_pos

let decode_wkb wkb_string =
  let c = make_cursor wkb_string in
  try
    decode_gpkg_header c;
    Some (parse_geometry_body c)
  with _ -> None

(* --- Stringifier --- *)

let point_to_str p = Printf.sprintf "%.2f %.2f" p.x p.y
let ring_to_str pts = "(" ^ String.concat ", " (List.map point_to_str pts) ^ ")"

let poly_to_str rings =
  "(" ^ String.concat ", " (List.map ring_to_str rings) ^ ")"

let to_string geom =
  match geom with
  | Point p -> "POINT (" ^ point_to_str p ^ ")"
  | Polygon rings -> "POLYGON " ^ poly_to_str rings
  | MultiPolygon polys ->
      "MULTIPOLYGON (" ^ String.concat ", " (List.map poly_to_str polys) ^ ")"
  | Unknown (n, msg) -> Printf.sprintf "UNKNOWN (%d: %s)" n msg
