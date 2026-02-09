(* extract_tiles.ml *)

open Geometry_types

(* --- Utils --- *)

let parse_tile_string str =
  (* Expected format: N45E006 *)
  try
    let ns = String.sub str 0 1 in
    let lat = float_of_string (String.sub str 1 2) in
    let ew = String.sub str 3 1 in
    let lon = float_of_string (String.sub str 4 3) in
    let lat = if ns = "S" then -.lat else lat in
    let lon = if ew = "W" then -.lon else lon in
    (lat, lon)
  with _ -> failwith ("Invalid tile string format: " ^ str)

(* --- Encoding / Quantization --- *)

module Encoder = struct
  type t = {
    (* We write raw bytes to these buffers, then compress them later *)
    (* 
       Columnar Storage Buffers:
       We separate attributes into distinct streams to improve compression.
       - 'high_x'/'low_x': Splitting 16-bit coordinates into high/low bytes puts similar bytes together.
         High bytes (e.g. 0x41) tend to stay constant or change slowly for local features, compression well.
         Low bytes are noisier (random bits), but isolating them prevents them from disrupting high byte entropy.
    *)
    meta : Buffer.t; (* Code (u16), VCount (u16), TCount (u16) per feature *)
    high_x : Buffer.t;
    high_y : Buffer.t;
    low_x : Buffer.t;
    low_y : Buffer.t;
    high_indices : Buffer.t;
    low_indices : Buffer.t;
  }

  let create () =
    {
      meta = Buffer.create 1024;
      high_x = Buffer.create 1024;
      low_x = Buffer.create 1024;
      high_y = Buffer.create 1024;
      low_y = Buffer.create 1024;
      high_indices = Buffer.create 1024;
      low_indices = Buffer.create 1024;
    }

  let write_u8 buf v = Buffer.add_char buf (Char.chr (v land 0xFF))

  let write_u16 buf v =
    write_u8 buf (v land 0xFF);
    write_u8 buf ((v lsr 8) land 0xFF)

  (* 
     ZigZag Encoding:
     Standard int16 representation uses two's complement. Small negative numbers (e.g. -1) are 0xFFFF,
     which has high Hamming distance from 0 or 1. This prevents Gzip from identifying small delta patterns.
     
     ZigZag maps signed integers to unsigned integers such that small absolute values (positive or negative)
     become small positive values:
       0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4 ...
     
     Formula: (n << 1) ^ (n >> 31)
  *)
  let zigzag_encode n = (n lsl 1) lxor (n asr 31)

  let encode_meta t code v_count t_count =
    write_u16 t.meta code;
    write_u16 t.meta v_count;
    write_u16 t.meta t_count

  let encode_vertices t (verts : float array) num_verts (min_x, min_y)
      (scale_x, scale_y) =
    let prev_x = ref 0 in
    let prev_y = ref 0 in

    for i = 0 to num_verts - 1 do
      let x = verts.(i * 2) in
      let y = verts.((i * 2) + 1) in

      (* Quantize *)
      let qx = int_of_float ((x -. min_x) *. scale_x) in
      let qy = int_of_float ((y -. min_y) *. scale_y) in

      (* Clamp to u16 range just in case *)
      let qx = max 0 (min 65535 qx) in
      let qy = max 0 (min 65535 qy) in

      (* 
         Modular Delta Encoding:
         Instead of handling boundary checks for wrapping, we embrace 16-bit modular arithmetic.
         (curr - prev) land 0xFFFF gives the "shortest path" difference in the u16 ring 
         assuming the step is small (which it is for geometry).
         
         We then interpret this unsigned u16 diff as a signed 16-bit integer (sdx/sdy).
         e.g., if diff is 0xFFFF (65535), it's interpreted as -1.
      *)
      let dx = (qx - !prev_x) land 0xFFFF in
      let dy = (qy - !prev_y) land 0xFFFF in
      prev_x := qx;
      prev_y := qy;

      let sdx = if dx >= 0x8000 then dx - 0x10000 else dx in
      let sdy = if dy >= 0x8000 then dy - 0x10000 else dy in

      (* ZigZag Encoding *)
      let zx = zigzag_encode sdx in
      let zy = zigzag_encode sdy in

      (* Split Bytes *)
      write_u8 t.low_x (zx land 0xFF);
      write_u8 t.high_x ((zx lsr 8) land 0xFF);

      write_u8 t.low_y (zy land 0xFF);
      write_u8 t.high_y ((zy lsr 8) land 0xFF)
    done

  let encode_indices t indices count =
    let prev_idx = ref 0 in
    for i = 0 to count - 1 do
      let idx = indices.(i) in

      (* Delta Encoding (Modular u16) *)
      let di = (idx - !prev_idx) land 0xFFFF in
      prev_idx := idx;

      (* Interpret as signed 16-bit for ZigZag *)
      let sdi = if di >= 0x8000 then di - 0x10000 else di in

      (* ZigZag Encoding *)
      let zi = zigzag_encode sdi in

      (* Split Bytes *)
      write_u8 t.low_indices (zi land 0xFF);
      write_u8 t.high_indices ((zi lsr 8) land 0xFF)
    done

  let compress_string str =
    let input_pos = ref 0 in
    let len = String.length str in
    let input_cb buf =
      let n = min (Bytes.length buf) (len - !input_pos) in
      if n > 0 then (
        Bytes.blit_string str !input_pos buf 0 n;
        input_pos := !input_pos + n);
      n
    in
    let out_buf = Buffer.create 1024 in
    let output_cb buf n = Buffer.add_subbytes out_buf buf 0 n in
    Zlib.compress input_cb output_cb;
    Buffer.contents out_buf

  let write_block out_ch buf =
    let raw_str = Buffer.contents buf in
    let compressed = compress_string raw_str in
    let comp_len = String.length compressed in

    (* Write Compressed Length (u32) *)
    output_byte out_ch (comp_len land 0xFF);
    output_byte out_ch ((comp_len lsr 8) land 0xFF);
    output_byte out_ch ((comp_len lsr 16) land 0xFF);
    output_byte out_ch ((comp_len lsr 24) land 0xFF);

    output_string out_ch compressed
end

(* Water encoder with 3-byte coordinates for ~30-50cm precision *)
module Water_encoder = struct
  type t = {
    meta : Buffer.t; (* Code (u16), VCount (u16), TCount (u16) per feature *)
    high_x : Buffer.t;
    mid_x : Buffer.t;
    low_x : Buffer.t;
    high_y : Buffer.t;
    mid_y : Buffer.t;
    low_y : Buffer.t;
    high_indices : Buffer.t;
    low_indices : Buffer.t;
  }

  let create () =
    {
      meta = Buffer.create 1024;
      high_x = Buffer.create 1024;
      mid_x = Buffer.create 1024;
      low_x = Buffer.create 1024;
      high_y = Buffer.create 1024;
      mid_y = Buffer.create 1024;
      low_y = Buffer.create 1024;
      high_indices = Buffer.create 1024;
      low_indices = Buffer.create 1024;
    }

  let write_u8 buf v = Buffer.add_char buf (Char.chr (v land 0xFF))

  let write_u16 buf v =
    write_u8 buf (v land 0xFF);
    write_u8 buf ((v lsr 8) land 0xFF)

  (* ZigZag encoding for 24-bit values *)
  let zigzag_encode n = (n lsl 1) lxor (n asr 31)

  let encode_meta t code v_count t_count =
    write_u16 t.meta code;
    write_u16 t.meta v_count;
    write_u16 t.meta t_count

  (* 3-byte coordinate encoding for water layer *)
  let encode_vertices t (verts : float array) num_verts (min_x, min_y)
      (scale_x, scale_y) =
    let prev_x = ref 0 in
    let prev_y = ref 0 in

    for i = 0 to num_verts - 1 do
      let x = verts.(i * 2) in
      let y = verts.((i * 2) + 1) in

      (* Quantize to 24-bit range but use limited scale for ~40cm precision *)
      let qx = int_of_float ((x -. min_x) *. scale_x) in
      let qy = int_of_float ((y -. min_y) *. scale_y) in

      (* Clamp to 24-bit range (0 to 16777215) *)
      let qx = max 0 (min 0xFFFFFF qx) in
      let qy = max 0 (min 0xFFFFFF qy) in

      (* Modular Delta Encoding for 24-bit values *)
      let dx = (qx - !prev_x) land 0xFFFFFF in
      let dy = (qy - !prev_y) land 0xFFFFFF in
      prev_x := qx;
      prev_y := qy;

      (* Convert to signed for zigzag *)
      let sdx = if dx >= 0x800000 then dx - 0x1000000 else dx in
      let sdy = if dy >= 0x800000 then dy - 0x1000000 else dy in

      (* ZigZag Encoding *)
      let zx = zigzag_encode sdx in
      let zy = zigzag_encode sdy in

      (* Split into 3 bytes *)
      write_u8 t.low_x (zx land 0xFF);
      write_u8 t.mid_x ((zx lsr 8) land 0xFF);
      write_u8 t.high_x ((zx lsr 16) land 0xFF);

      write_u8 t.low_y (zy land 0xFF);
      write_u8 t.mid_y ((zy lsr 8) land 0xFF);
      write_u8 t.high_y ((zy lsr 16) land 0xFF)
    done

  (* Index encoding unchanged from CLC (16-bit sufficient) *)
  let encode_indices t indices count =
    let prev_idx = ref 0 in
    for i = 0 to count - 1 do
      let idx = indices.(i) in
      let di = (idx - !prev_idx) land 0xFFFF in
      prev_idx := idx;
      let sdi = if di >= 0x8000 then di - 0x10000 else di in
      let zi = zigzag_encode sdi in
      write_u8 t.low_indices (zi land 0xFF);
      write_u8 t.high_indices ((zi lsr 8) land 0xFF)
    done

  let write_block out_ch buf = Encoder.write_block out_ch buf
end

(* POI encoder for peaks and saddles *)
module Poi_encoder = struct
  type t = {
    (* POI data uses simpler encoding since count is small *)
    names : Buffer.t; (* Length-prefixed UTF-8 strings *)
    coords : Buffer.t; (* 3-byte quantized lon/lat pairs (same as water) *)
    elevations : Buffer.t; (* Signed 16-bit elevation in meters *)
    types : Buffer.t; (* 1 byte: 0=peak, 1=saddle *)
  }

  let create () =
    {
      names = Buffer.create 1024;
      coords = Buffer.create 256;
      elevations = Buffer.create 128;
      types = Buffer.create 64;
    }

  let write_u8 buf v = Buffer.add_char buf (Char.chr (v land 0xFF))

  let write_u16 buf v =
    write_u8 buf (v land 0xFF);
    write_u8 buf ((v lsr 8) land 0xFF)

  let write_i16 buf v =
    (* Signed 16-bit as unsigned *)
    let uv = if v < 0 then v + 0x10000 else v in
    write_u16 buf uv

  let encode_poi t (poi : Poi_fetch.poi) (min_lon, min_lat) (scale_x, scale_y) =
    (* Encode name as length-prefixed UTF-8 *)
    let name_bytes = Bytes.of_string poi.name in
    let name_len = Bytes.length name_bytes in
    write_u8 t.names name_len;
    Buffer.add_bytes t.names name_bytes;

    (* Encode coordinates (3-byte quantized, same as water) *)
    let qx = int_of_float ((poi.lon -. min_lon) *. scale_x) in
    let qy = int_of_float ((poi.lat -. min_lat) *. scale_y) in
    let qx = max 0 (min 0xFFFFFF qx) in
    let qy = max 0 (min 0xFFFFFF qy) in
    write_u8 t.coords (qx land 0xFF);
    write_u8 t.coords ((qx lsr 8) land 0xFF);
    write_u8 t.coords ((qx lsr 16) land 0xFF);
    write_u8 t.coords (qy land 0xFF);
    write_u8 t.coords ((qy lsr 8) land 0xFF);
    write_u8 t.coords ((qy lsr 16) land 0xFF);

    (* Encode elevation (signed 16-bit, 0 if unknown) *)
    let elev = match poi.elevation with Some e -> e | None -> 0 in
    let elev = max (-32768) (min 32767 elev) in
    write_i16 t.elevations elev;

    (* Encode type (0=peak, 1=saddle) *)
    let type_byte =
      match poi.poi_type with Poi_fetch.Peak -> 0 | Poi_fetch.Saddle -> 1
    in
    write_u8 t.types type_byte

  let write_block out_ch buf = Encoder.write_block out_ch buf
end

module type PROJ = sig
  val of_wgs84 : float -> float -> float * float
  val to_wgs84 : float -> float -> float * float
end

(* --- Main Pipeline --- *)

let process_tile db_path output_dir tile_name =
  let lat, lon = parse_tile_string tile_name in
  Printf.printf "Processing Tile: %s (Lat: %.2f, Lon: %.2f)\n%!" tile_name lat
    lon;

  (* Select Table and Projection *)
  let table_name, (module P : PROJ) =
    if lat < -20.0 && lat > -23.0 && lon > 54.0 && lon < 57.0 then
      ("CLC2018_CLC2018_V2018_20_FR_REU", (module Proj_2975 : PROJ))
    else
      ( "CLC2018_CLC2018_V2018_20",
        (module struct
          let of_wgs84 = Proj_3035.wgs84_to_laea
          let to_wgs84 = Proj_3035.laea_to_wgs84
        end : PROJ) )
  in

  (* Define Bounds with Margin *)
  let margin_deg = 0.05 in
  (* 1 pixel margin *)
  let min_lat = lat -. margin_deg in
  let max_lat = lat +. 1.0 +. margin_deg in
  let min_lon = lon -. margin_deg in
  let max_lon = lon +. 1.0 +. margin_deg in

  (* 
     Coordinate Normalization:
     We Map [min_lon, max_lon] to [0, 65535].
     This 16-bit quantization is sufficient for <1m precision within a 1-degree tile.
     
     Note: Since we use LAEA (meters) for extraction but WGS84 for storage,
     there is non-linear distortion, but at the scale of 1 degree, simple linear 
     interpolation in the vertex shader is visually acceptable for this use-case,
     especially since we triangulate densely enough.
  *)
  let range_x = max_lon -. min_lon in
  let range_y = max_lat -. min_lat in
  let scale_x = 65535.0 /. range_x in
  let scale_y = 65535.0 /. range_y in

  let db = Sqlite3.db_open db_path in

  let laea_pts =
    [
      P.of_wgs84 min_lon min_lat;
      P.of_wgs84 max_lon min_lat;
      P.of_wgs84 max_lon max_lat;
      P.of_wgs84 min_lon max_lat;
      (* Sample Edge Midpoints to handle curvature *)
      P.of_wgs84 ((min_lon +. max_lon) /. 2.0) min_lat;
      P.of_wgs84 ((min_lon +. max_lon) /. 2.0) max_lat;
      P.of_wgs84 min_lon ((min_lat +. max_lat) /. 2.0);
      P.of_wgs84 max_lon ((min_lat +. max_lat) /. 2.0);
    ]
  in

  let min_laea_x =
    List.fold_left (fun acc (x, _) -> min acc x) infinity laea_pts
  in
  let max_laea_x =
    List.fold_left (fun acc (x, _) -> max acc x) neg_infinity laea_pts
  in
  let min_laea_y =
    List.fold_left (fun acc (_, y) -> min acc y) infinity laea_pts
  in
  let max_laea_y =
    List.fold_left (fun acc (_, y) -> max acc y) neg_infinity laea_pts
  in

  Printf.printf "LAEA Query Bounds: X[%.0f - %.0f] Y[%.0f - %.0f]\n%!"
    min_laea_x max_laea_x min_laea_y max_laea_y;

  let sql =
    Printf.sprintf
      "SELECT c.Shape, c.Code_18 \n\
      \       FROM %s c \n\
      \       JOIN rtree_%s_Shape r ON c.OBJECTID = r.id \n\
      \       WHERE r.maxx >= %f AND r.minx <= %f AND r.maxy >= %f AND r.miny \
       <= %f"
      table_name table_name min_laea_x max_laea_x min_laea_y max_laea_y
  in
  let stmt = Sqlite3.prepare db sql in

  let clipper_region =
    {
      Geometry_types.min_x = min_lon;
      Geometry_types.min_y = min_lat;
      Geometry_types.max_x = max_lon;
      Geometry_types.max_y = max_lat;
    }
  in

  let encoder = Encoder.create () in
  let entry_count = ref 0 in
  let total_vertices = ref 0 in
  let total_indices = ref 0 in
  (* 
     Recursive helper: clipped multipolygon handling.
  *)
  let max_clipped_verts = 5000000 in

  let rec clip_with_split_fixed flat_verts proper_poly region depth =
    let clipped_verts, result_polys =
      Polygon_clipping.Clipper.clip_multipolygon flat_verts [| proper_poly |]
        region
    in
    if Array.length result_polys = 0 then []
    else
      let vert_count = Array.length clipped_verts / 2 in
      (* Return if small enough OR if we've hit max recursion depth *)
      if vert_count <= max_clipped_verts || depth > 3 then
        Array.to_list (Array.map (fun p -> (clipped_verts, p)) result_polys)
      else
        (* Split region either Horizontally or Vertically *)
        let width = region.max_x -. region.min_x in
        let height = region.max_y -. region.min_y in

        let r1, r2 =
          if width > height then
            let mid_x = (region.min_x +. region.max_x) /. 2.0 in
            ({ region with max_x = mid_x }, { region with min_x = mid_x })
          else
            let mid_y = (region.min_y +. region.max_y) /. 2.0 in
            ({ region with max_y = mid_y }, { region with min_y = mid_y })
        in

        (* Recurse on EACH resulting polygon *)
        Array.fold_left
          (fun acc poly ->
            let res1 =
              clip_with_split_fixed clipped_verts poly r1 (depth + 1)
            in
            let res2 =
              clip_with_split_fixed clipped_verts poly r2 (depth + 1)
            in
            acc @ res1 @ res2)
          [] result_polys
  in

  let rec collect_features acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let batch_acc = ref [] in
        let shape_blob = Sqlite3.column stmt 0 in
        let code_str = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 1) in
        let code = try int_of_string code_str with _ -> 0 in

        begin match shape_blob with
        | Sqlite3.Data.BLOB b ->
            let wkb_opt = Some b in
            begin match wkb_opt with
            | Some raw -> begin
                match Wkb_decode.decode_wkb raw with
                | Some geom ->
                    (* 1. Convert Geometry to float array (MultiPolygon) *)
                    let polys =
                      match geom with
                      | Wkb_decode.Polygon rings -> [| rings |]
                      | Wkb_decode.MultiPolygon polys -> Array.of_list polys
                      | _ -> [||]
                    in

                    if Array.length polys > 0 then
                      (* Convert WKB list of points to native 'flat' float array for tools *)
                      (* AND Reproject on the fly *)
                      let float_polys =
                        Array.map
                          (fun rings ->
                            let outer_pts =
                              List.concat
                                (List.map
                                   (fun (p : Wkb_decode.point) ->
                                     let lon, lat = P.to_wgs84 p.x p.y in
                                     [ lon; lat ])
                                   (List.hd rings))
                            in

                            let hole_list = List.tl rings in
                            let raw_holes =
                              Array.of_list
                                (List.map
                                   (fun ring ->
                                     let pts =
                                       List.concat
                                         (List.map
                                            (fun (p : Wkb_decode.point) ->
                                              let lon, lat =
                                                P.to_wgs84 p.x p.y
                                              in
                                              [ lon; lat ])
                                            ring)
                                     in
                                     Array.of_list pts)
                                   hole_list)
                            in

                            (Array.of_list outer_pts, raw_holes))
                          polys
                      in

                      (* 
                         2. Flatten for Polygon Clipping
                         The Clipper library typically expects a flat array of floats.
                         We flatten the MultiPolygon structure (Outer + Holes) into a single
                         float array `flat_verts` and a list of hole descriptors.
                      *)
                      Array.iter
                        (fun (outer_verts, hole_arrays) ->
                          let total_len =
                            Array.length outer_verts
                            + Array.fold_left
                                (fun acc h -> acc + Array.length h)
                                0 hole_arrays
                          in
                          let flat_verts = Array.make total_len 0.0 in
                          Array.blit outer_verts 0 flat_verts 0
                            (Array.length outer_verts);
                          let offset = ref (Array.length outer_verts) in

                          let proper_holes =
                            Array.map
                              (fun raw_h ->
                                Array.blit raw_h 0 flat_verts !offset
                                  (Array.length raw_h);
                                let start = !offset / 2 in
                                let len = Array.length raw_h / 2 in
                                offset := !offset + (len * 2);
                                { Geometry_types.start; len })
                              hole_arrays
                          in

                          let proper_poly =
                            {
                              Geometry_types.outer =
                                {
                                  Geometry_types.start = 0;
                                  len = Array.length outer_verts / 2;
                                };
                              holes = proper_holes;
                            }
                          in

                          (* 3. Clip with recursive splitting for large polygons *)
                          let clipped_pieces =
                            clip_with_split_fixed flat_verts proper_poly
                              clipper_region 0
                          in

                          List.iter
                            (fun (clipped_verts, clipped_poly) ->
                              (* 4. Triangulate *)
                              let tris =
                                try
                                  Polygon_triangulation.Triangulator
                                  .triangulate_multi ~tile:tile_name
                                    ~feature_type:"clc" clipped_verts
                                    [| clipped_poly |]
                                with Invalid_argument msg ->
                                  Printf.printf "Triangulation failed: %s\n%!"
                                    msg;
                                  [||]
                              in

                              if Array.length tris > 0 then (
                                (* 5. Reorder & Encode *)
                                let n_old = Array.length clipped_verts / 2 in
                                let map = Array.make n_old (-1) in
                                let new_verts_dyn = ref [] in
                                let next_idx = ref 0 in
                                let new_indices_rev = ref [] in

                                for i = 0 to Array.length tris - 1 do
                                  let old_idx = tris.(i) in
                                  if old_idx >= 0 && old_idx < Array.length map
                                  then (
                                    if map.(old_idx) = -1 then (
                                      map.(old_idx) <- !next_idx;
                                      let vx = clipped_verts.(old_idx * 2) in
                                      let vy =
                                        clipped_verts.((old_idx * 2) + 1)
                                      in
                                      new_verts_dyn :=
                                        vy :: vx :: !new_verts_dyn;
                                      (* Store reverse for now *)
                                      incr next_idx);
                                    new_indices_rev :=
                                      map.(old_idx) :: !new_indices_rev)
                                done;

                                let final_indices =
                                  Array.of_list (List.rev !new_indices_rev)
                                in
                                let final_verts_list =
                                  List.rev !new_verts_dyn
                                in
                                (* x,y, x,y... *)
                                let final_verts =
                                  Array.of_list final_verts_list
                                in

                                (* Encode into Global Buffer *)
                                incr entry_count;
                                let v_count = !next_idx in
                                let i_count = Array.length final_indices in
                                (* ALIGNMENT FIX: Ensure we only write 3*N indices *)
                                let tri_count_val = i_count / 3 in
                                let valid_i_count = tri_count_val * 3 in

                                total_vertices := !total_vertices + v_count;
                                total_indices := !total_indices + valid_i_count;

                                (* Compute Area for Sorting *)
                                let min_x = ref infinity in
                                let max_x = ref neg_infinity in
                                let min_y = ref infinity in
                                let max_y = ref neg_infinity in
                                for k = 0 to v_count - 1 do
                                  let x = final_verts.(k * 2) in
                                  let y = final_verts.((k * 2) + 1) in
                                  if x < !min_x then min_x := x;
                                  if x > !max_x then max_x := x;
                                  if y < !min_y then min_y := y;
                                  if y > !max_y then max_y := y
                                done;
                                let area =
                                  (!max_x -. !min_x) *. (!max_y -. !min_y)
                                in

                                batch_acc :=
                                  ( area,
                                    code,
                                    v_count,
                                    tri_count_val,
                                    valid_i_count,
                                    final_verts,
                                    final_indices )
                                  :: !batch_acc))
                            clipped_pieces)
                        float_polys
                | None -> ()
              end
            | None -> ()
            end
        | _ -> ()
        end;
        collect_features (!batch_acc @ acc)
    | Sqlite3.Rc.DONE -> acc
    | _ -> failwith "DB Error"
  in

  let all_features = collect_features [] in

  (* Sorting by Area Ascending (Smallest First) *)
  let sorted_features =
    List.sort
      (fun (a1, _, _, _, _, _, _) (a2, _, _, _, _, _, _) -> compare a1 a2)
      all_features
  in

  (* Encode Sorted Features *)
  List.iter
    (fun ( _area,
           code,
           v_count,
           tri_count,
           valid_i_count,
           final_verts,
           final_indices ) ->
      Encoder.encode_meta encoder code v_count tri_count;

      let _ =
        Encoder.encode_vertices encoder final_verts v_count (min_lon, min_lat)
          (scale_x, scale_y)
      in
      let _ = Encoder.encode_indices encoder final_indices valid_i_count in
      ())
    sorted_features;

  (* === WATER LAYER PROCESSING === *)
  Printf.printf "Fetching water polygons from OSM...\n%!";

  (* Water scale: ~220000 values for 1.1° gives ~40cm precision *)
  let water_scale_x = 220000.0 /. range_x in
  let water_scale_y = 220000.0 /. range_y in

  let water_encoder = Water_encoder.create () in
  let water_entry_count = ref 0 in
  let water_total_vertices = ref 0 in
  let water_total_indices = ref 0 in

  let water_features =
    Osm_fetch.fetch_water_polygons ~min_lat ~min_lon ~max_lat ~max_lon
  in

  (* Process each water feature *)
  List.iter
    (fun (feature : Osm_fetch.water_feature) ->
      let tile_name = Printf.sprintf "%s-%d" tile_name feature.id in
      let flat_arrays = Osm_fetch.feature_to_flat_arrays feature in
      List.iter
        (fun (clc_code, outer_flat, holes_flat) ->
          (* Build geometry like CLC processing *)
          let total_len =
            Array.length outer_flat
            + Array.fold_left (fun acc h -> acc + Array.length h) 0 holes_flat
          in
          if total_len < 6 then () (* Skip degenerate polygons *)
          else begin
            let flat_verts = Array.make total_len 0.0 in
            Array.blit outer_flat 0 flat_verts 0 (Array.length outer_flat);
            let offset = ref (Array.length outer_flat) in

            let proper_holes =
              Array.map
                (fun raw_h ->
                  Array.blit raw_h 0 flat_verts !offset (Array.length raw_h);
                  let start = !offset / 2 in
                  let len = Array.length raw_h / 2 in
                  offset := !offset + (len * 2);
                  { Geometry_types.start; len })
                holes_flat
            in

            let proper_poly =
              {
                Geometry_types.outer =
                  {
                    Geometry_types.start = 0;
                    len = Array.length outer_flat / 2;
                  };
                holes = proper_holes;
              }
            in

            (* Clip to tile region *)
            let clipped_pieces =
              clip_with_split_fixed flat_verts proper_poly clipper_region 0
            in

            List.iter
              (fun (clipped_verts, clipped_poly) ->
                (* Triangulate *)
                let tris =
                  try
                    Polygon_triangulation.Triangulator.triangulate_multi
                      ~tile:tile_name ~feature_type:"water" clipped_verts
                      [| clipped_poly |]
                  with Invalid_argument msg ->
                    Printf.printf "Water triangulation failed: %s\n%!" msg;
                    [||]
                in

                if Array.length tris > 0 then begin
                  (* Reorder vertices for draw order optimization *)
                  let n_old = Array.length clipped_verts / 2 in
                  let map = Array.make n_old (-1) in
                  let new_verts_dyn = ref [] in
                  let next_idx = ref 0 in
                  let new_indices_rev = ref [] in

                  for i = 0 to Array.length tris - 1 do
                    let old_idx = tris.(i) in
                    if old_idx >= 0 && old_idx < Array.length map then begin
                      if map.(old_idx) = -1 then begin
                        map.(old_idx) <- !next_idx;
                        let vx = clipped_verts.(old_idx * 2) in
                        let vy = clipped_verts.((old_idx * 2) + 1) in
                        new_verts_dyn := vy :: vx :: !new_verts_dyn;
                        incr next_idx
                      end;
                      new_indices_rev := map.(old_idx) :: !new_indices_rev
                    end
                  done;

                  let final_indices =
                    Array.of_list (List.rev !new_indices_rev)
                  in
                  let final_verts = Array.of_list (List.rev !new_verts_dyn) in

                  let v_count = !next_idx in
                  let i_count = Array.length final_indices in
                  let tri_count_val = i_count / 3 in
                  let valid_i_count = tri_count_val * 3 in

                  if v_count > 0 && tri_count_val > 0 then begin
                    incr water_entry_count;
                    water_total_vertices := !water_total_vertices + v_count;
                    water_total_indices := !water_total_indices + valid_i_count;

                    Water_encoder.encode_meta water_encoder clc_code v_count
                      tri_count_val;
                    Water_encoder.encode_vertices water_encoder final_verts
                      v_count (min_lon, min_lat)
                      (water_scale_x, water_scale_y);
                    Water_encoder.encode_indices water_encoder final_indices
                      valid_i_count
                  end
                end)
              clipped_pieces
          end)
        flat_arrays)
    water_features;

  Printf.printf "Processed %d water features (%d verts, %d indices)\n%!"
    !water_entry_count !water_total_vertices !water_total_indices;

  (* === POI LAYER PROCESSING === *)
  Printf.printf "Fetching POIs from OSM...\n%!";

  let poi_encoder = Poi_encoder.create () in
  let poi_count = ref 0 in

  (* Use same scale as water for ~40cm precision *)
  let poi_scale_x = water_scale_x in
  let poi_scale_y = water_scale_y in

  let pois = Poi_fetch.fetch_pois ~min_lat ~min_lon ~max_lat ~max_lon in
  List.iter
    (fun poi ->
      Poi_encoder.encode_poi poi_encoder poi (min_lon, min_lat)
        (poi_scale_x, poi_scale_y);
      incr poi_count)
    pois;

  Printf.printf "Processed %d POIs\n%!" !poi_count;

  (* === FILE OUTPUT (CLC5 Format) === *)
  let output_file_path = Filename.concat output_dir (tile_name ^ ".clc") in
  let out_ch = open_out_bin output_file_path in

  (* Write Header (Magic + Counts + Bounds + Scales) *)
  output_string out_ch "CLC5";

  (* CLC counts *)
  output_binary_int out_ch !entry_count;
  output_binary_int out_ch !total_vertices;
  output_binary_int out_ch !total_indices;

  (* Water counts *)
  output_binary_int out_ch !water_entry_count;
  output_binary_int out_ch !water_total_vertices;
  output_binary_int out_ch !water_total_indices;

  (* POI count *)
  output_binary_int out_ch !poi_count;

  (* Write float params (bounds + scales) for reconstruction *)
  let write_float64 f =
    let bits = Int64.bits_of_float f in
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 0) land 0xFF);
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 8) land 0xFF);
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 16) land 0xFF);
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 24) land 0xFF);
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 32) land 0xFF);
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 40) land 0xFF);
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 48) land 0xFF);
    output_byte out_ch (Int64.to_int (Int64.shift_right bits 56) land 0xFF)
  in
  write_float64 min_lon;
  write_float64 min_lat;
  write_float64 scale_x;
  write_float64 scale_y;
  write_float64 water_scale_x;
  write_float64 water_scale_y;
  write_float64 poi_scale_x;
  write_float64 poi_scale_y;

  (* Write CLC Streams *)
  Encoder.write_block out_ch encoder.meta;
  Encoder.write_block out_ch encoder.high_x;
  Encoder.write_block out_ch encoder.low_x;
  Encoder.write_block out_ch encoder.high_y;
  Encoder.write_block out_ch encoder.low_y;
  Encoder.write_block out_ch encoder.high_indices;
  Encoder.write_block out_ch encoder.low_indices;

  (* Write Water Streams (3-byte coords) *)
  Water_encoder.write_block out_ch water_encoder.meta;
  Water_encoder.write_block out_ch water_encoder.high_x;
  Water_encoder.write_block out_ch water_encoder.mid_x;
  Water_encoder.write_block out_ch water_encoder.low_x;
  Water_encoder.write_block out_ch water_encoder.high_y;
  Water_encoder.write_block out_ch water_encoder.mid_y;
  Water_encoder.write_block out_ch water_encoder.low_y;
  Water_encoder.write_block out_ch water_encoder.high_indices;
  Water_encoder.write_block out_ch water_encoder.low_indices;

  (* Write POI Streams *)
  Poi_encoder.write_block out_ch poi_encoder.names;
  Poi_encoder.write_block out_ch poi_encoder.coords;
  Poi_encoder.write_block out_ch poi_encoder.elevations;
  Poi_encoder.write_block out_ch poi_encoder.types;

  Printf.printf "Streams Written.\n%!";

  close_out out_ch;
  ignore (Sqlite3.db_close db);
  Printf.printf
    "Done. Extracted %d CLC features (%d verts, %d indices) + %d water \
     features (%d verts, %d indices) + %d POIs.\n\
     %!"
    !entry_count !total_vertices !total_indices !water_entry_count
    !water_total_vertices !water_total_indices !poi_count

let () =
  if Array.length Sys.argv < 4 then
    Printf.printf "Usage: %s <DB> <Output Dir> <TileCode> (e.g. N45E006)\n"
      Sys.argv.(0)
  else process_tile Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
