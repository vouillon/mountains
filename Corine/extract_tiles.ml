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

(* WKB linear rings are closed (the last point repeats the first). Drop the
   closing duplicate: downstream code treats rings as implicitly closed, and
   the duplicate otherwise trips the duplicate-point validation. *)
let strip_closing_point (ring : Wkb_decode.point list) =
  match ring with
  | [] -> []
  | first :: _ ->
      let rec drop_last = function
        | [] -> []
        | [ (last : Wkb_decode.point) ] ->
            if last.x = first.x && last.y = first.y then [] else [ last ]
        | p :: rest -> p :: drop_last rest
      in
      drop_last ring

(* --- Encoding / Quantization --- *)

(* A feature's vertices on the tile's grid: [scale] steps per degree from
   [min], rounded to the nearest one and clamped to [0, limit]. *)
let quantize (verts : float array) (min_x, min_y) (scale_x, scale_y) limit =
  let q v m s = max 0 (min limit (int_of_float (((v -. m) *. s) +. 0.5))) in
  ( Array.init (Array.length verts / 2) (fun i -> q verts.(2 * i) min_x scale_x),
    Array.init
      (Array.length verts / 2)
      (fun i -> q verts.((2 * i) + 1) min_y scale_y) )

module Encoder = struct
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
    (* Encode name as length-prefixed UTF-8. The prefix is a single byte, so
       truncate longer names on a UTF-8 boundary to keep the stream framed. *)
    let name = poi.name in
    let name_len =
      if String.length name <= 255 then String.length name
      else begin
        let n = ref 255 in
        while !n > 0 && Char.code name.[!n] land 0xC0 = 0x80 do
          decr n
        done;
        !n
      end
    in
    write_u8 t.names name_len;
    Buffer.add_substring t.names name 0 name_len;

    (* Encode coordinates (3-byte quantized, rounded to nearest, same as
       water) *)
    let qx = int_of_float (((poi.lon -. min_lon) *. scale_x) +. 0.5) in
    let qy = int_of_float (((poi.lat -. min_lat) *. scale_y) +. 0.5) in
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
end

module type PROJ = sig
  val of_wgs84 : float -> float -> float * float
  val to_wgs84 : float -> float -> float * float
end

(* --- Main Pipeline --- *)

(* The far field's land cover: a degree cell as a grid of palette indices at 4
   arcseconds, on the samples of the far field's elevation (compress_dem
   --coarse) -- a sample's class is its centre's, 4 col + 1.5 arcseconds east
   of the cell's west edge and 4 row + 1.5 south of its north one, or water
   where half of the sixteen arcseconds around it are -- and cut
   into the same nine sub-tiles, in the same framing: a .dem tile whose
   heights are indices, so that the client reads both through one decoder. The
   elevation range its header must state means nothing here. Land cover drawn
   to 25 ha needs no more than this 45 km from the eye and beyond, and the
   polygons themselves are megabytes a cell. *)
module Coarse = struct
  let size = 900
  let sub_size = size / 3
  let create () = Bytes.make (size * size) '\255'

  (* The arcseconds of a sample, each way: the water is drawn on them. *)
  let per = 4
  let create_fine () = Bytes.make (size * per * size * per) '\255'

  (* Fill the samples whose centre a triangle covers, unless something has
     them already: the features come smallest first, as the client draws
     them. [verts] are (lon, lat) pairs. [grid] is one of samples, or of
     arcseconds with [fine]. *)
  let fill ?(fine = false) grid ~lon ~lat ~index verts indices tri_count =
    let size = if fine then size * per else size in
    let pos d = if fine then d *. 3600. else ((d *. 3600.) -. 1.5) /. 4. in
    let gx v = pos (verts.(2 * v) -. lon) in
    let gy v = pos (lat +. 1. -. verts.((2 * v) + 1)) in
    for t = 0 to tri_count - 1 do
      let a = indices.(3 * t)
      and b = indices.((3 * t) + 1)
      and c = indices.((3 * t) + 2) in
      let ax = gx a and ay = gy a in
      let bx = gx b and by = gy b in
      let cx = gx c and cy = gy c in
      let area = ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax)) in
      if area <> 0. then begin
        let lo u v w = max 0 (int_of_float (ceil (Float.min u (Float.min v w))))
        and hi u v w =
          min (size - 1) (int_of_float (floor (Float.max u (Float.max v w))))
        in
        for row = lo ay by cy to hi ay by cy do
          for col = lo ax bx cx to hi ax bx cx do
            let x = float col and y = float row in
            let side px py qx qy =
              (((qx -. px) *. (y -. py)) -. ((qy -. py) *. (x -. px))) *. area
            in
            if
              Bytes.get grid ((row * size) + col) = '\255'
              && side ax ay bx by >= 0.
              && side bx by cx cy >= 0.
              && side cx cy ax ay >= 0.
            then Bytes.set grid ((row * size) + col) (Char.chr index)
          done
        done
      end
    done

  (* The water, drawn by the arcsecond ([fill ~fine]), onto the samples: one
     is water when half of its arcseconds are, and of the class most of those
     have. A sample's centre alone makes a dotted line of a river narrower
     than the samples are apart, and the same river comes out whole, or not
     at all, this way. *)
  let fill_water grid water =
    let counts = Array.make 256 0 in
    for row = 0 to size - 1 do
      for col = 0 to size - 1 do
        let total = ref 0 in
        for r = 0 to per - 1 do
          for c = 0 to per - 1 do
            let i =
              Char.code
                (Bytes.get water
                   ((((row * per) + r) * size * per) + (col * per) + c))
            in
            if i <> 255 then begin
              counts.(i) <- counts.(i) + 1;
              incr total
            end
          done
        done;
        if !total > 0 then begin
          let best = ref 0 and most = ref 0 in
          Array.iteri
            (fun i n ->
              if n > !most then begin
                best := i;
                most := n
              end;
              counts.(i) <- 0)
            counts;
          if 2 * !total >= per * per then
            Bytes.set grid ((row * size) + col) (Char.chr !best)
        end
      done
    done

  (* Take back the patches of water, samples that touch by a side or a
     corner, that are all of class [index] and fewer than [min]: where a water
     course is about half as wide as the samples are apart, [fill_water] has
     it by fits and starts. To call while [grid] has nothing but water. *)
  let drop_specks grid ~index ~min =
    let seen = Bytes.make (size * size) '\000' in
    let patch = Array.make min 0 in
    let stack = Stack.create () in
    for start = 0 to (size * size) - 1 do
      if Bytes.get grid start <> '\255' && Bytes.get seen start = '\000' then begin
        let count = ref 0 and only = ref true in
        Bytes.set seen start '\001';
        Stack.push start stack;
        while not (Stack.is_empty stack) do
          let i = Stack.pop stack in
          if !count < min then patch.(!count) <- i;
          incr count;
          if Char.code (Bytes.get grid i) <> index then only := false;
          let row = i / size and col = i mod size in
          for r = max 0 (row - 1) to Int.min (size - 1) (row + 1) do
            for c = max 0 (col - 1) to Int.min (size - 1) (col + 1) do
              let j = (r * size) + c in
              if Bytes.get grid j <> '\255' && Bytes.get seen j = '\000' then begin
                Bytes.set seen j '\001';
                Stack.push j stack
              end
            done
          done
        done;
        if !only && !count < min then
          for k = 0 to !count - 1 do
            Bytes.set grid patch.(k) '\255'
          done
      end
    done

  (* The area of a triangulated polygon, in samples *)
  let area verts indices tri_count =
    let sum = ref 0. in
    for t = 0 to tri_count - 1 do
      let x v = verts.(2 * indices.((3 * t) + v))
      and y v = verts.((2 * indices.((3 * t) + v)) + 1) in
      sum :=
        !sum
        +. Float.abs
             (((x 1 -. x 0) *. (y 2 -. y 0)) -. ((y 1 -. y 0) *. (x 2 -. x 0)))
    done;
    !sum /. 2. *. float (size * size)

  (* The samples nothing has take the class of the nearest land, by a
     breadth-first walk from the land outwards: with the water taken from OSM
     these are where CORINE had water and OSM has none, as the client fills
     them on the base grid (cover_fill.frag). The walk goes over the water,
     which keeps its class: a hole may have nothing but water around it. *)
  let fill_holes grid ~is_land =
    let nearest = Bytes.make (size * size) '\255' in
    let queue = Queue.create () in
    Bytes.iteri
      (fun i c ->
        if c <> '\255' && is_land (Char.code c) then begin
          Bytes.set nearest i c;
          Queue.add i queue
        end)
      grid;
    while not (Queue.is_empty queue) do
      let i = Queue.take queue in
      let visit j =
        if Bytes.get nearest j = '\255' then begin
          Bytes.set nearest j (Bytes.get nearest i);
          if Bytes.get grid j = '\255' then
            Bytes.set grid j (Bytes.get nearest i);
          Queue.add j queue
        end
      in
      let row = i / size and col = i mod size in
      if row > 0 then visit (i - size);
      if row < size - 1 then visit (i + size);
      if col > 0 then visit (i - 1);
      if col < size - 1 then visit (i + 1)
    done

  let zigzag n =
    let n = n land 0xFFFF in
    let n = if n >= 0x8000 then n - 0x10000 else n in
    (n lsl 1) lxor (n asr 15) land 0xFFFF

  (* compress_dem's [compress] and [write_output], over indices. *)
  let write grid ~default ~row ~col path =
    let at r c =
      match
        Bytes.get grid ((((row * sub_size) + r) * size) + (col * sub_size) + c)
      with
      | '\255' -> default
      | i -> Char.code i
    in
    let n = sub_size * sub_size in
    let high = Bytes.create n and low = Bytes.create n in
    for r = 0 to sub_size - 1 do
      for c = 0 to sub_size - 1 do
        let predicted =
          if r = 0 && c = 0 then 0
          else if r = 0 then at r (c - 1)
          else if c = 0 then at (r - 1) c
          else at r (c - 1) + at (r - 1) c - at (r - 1) (c - 1)
        in
        let z = zigzag (at r c - predicted) in
        Bytes.set high ((r * sub_size) + c) (Char.chr (z lsr 8));
        Bytes.set low ((r * sub_size) + c) (Char.chr (z land 0xFF))
      done
    done;
    let high = Encoder.compress_string (Bytes.to_string high)
    and low = Encoder.compress_string (Bytes.to_string low) in
    let oc = open_out_bin (path ^ ".tmp") in
    let u32 v =
      let b = Bytes.create 4 in
      Bytes.set_int32_le b 0 v;
      output_bytes oc b
    in
    output_string oc "DEM1";
    u32 (Int32.of_int sub_size);
    u32 (Int32.of_int sub_size);
    u32 (Int32.bits_of_float (-500.));
    u32 (Int32.bits_of_float 9000.);
    u32 (Int32.of_int (String.length high));
    u32 (Int32.of_int (String.length low));
    output_string oc high;
    output_string oc low;
    close_out oc;
    Sys.rename (path ^ ".tmp") path;
    String.length high + String.length low
end

(* The water classes of CORINE, which the water of OSM stands in for *)
let is_water_code code = code = 511 || code = 512 || (code >= 521 && code <= 523)

(* A cell's tiles are its sub-cells of 600 arcseconds, [sub_cells] by
   [sub_cells], as for the elevation. A sub-cell's coordinates run from 0 at
   its south-west corner to [qmax] at its north-east one, exactly: the client
   places a vertex at [k * qmax + q] quanta from the sub-cell grid of its cover
   map, which a float holds exactly while under 2^24, so a vertex on an edge
   lands where its neighbour's does, and no margin is needed. The quanta are
   those of the 1° tiles, rounded to fractions of an arcsecond: 1/18 arcsecond (1.7 m)
   for the land cover, as fine as the finest cover-map texel (16 bits over a
   sub-cell, 28 cm, made the land 30% larger: deltas six times larger),
   1/72 arcsecond (43 cm) for the water, in 24-bit fields. The POIs are read on the
   CPU: all 24 bits. *)
let sub_cells = 6
let land_qmax = 600 * 18
let water_qmax = 600 * 72
let poi_qmax = 0xFFFFFF

let sub_tile_name ~lat ~lon ~row ~col ext =
  Printf.sprintf "%c%02d_%c%03d_%d_%d.%s"
    (if lat >= 0. then 'N' else 'S')
    (int_of_float (Float.abs lat))
    (if lon >= 0. then 'E' else 'W')
    (int_of_float (Float.abs lon))
    row col ext

(* A CLC6 tile of origin [min_lon, min_lat] and [range] degrees square: land
   cover and water quantized by [quantize] already, the POIs here. *)
let write_tile path (min_lon, min_lat) range ~cover ~water ~pois =
  let scale = float land_qmax /. range in
  let water_scale = float water_qmax /. range in
  let poi_scale = float poi_qmax /. range in
  let poi_encoder = Poi_encoder.create () in
  List.iter
    (fun poi ->
      Poi_encoder.encode_poi poi_encoder poi (min_lon, min_lat)
        (poi_scale, poi_scale))
    pois;
  let floats = Bytes.create 64 in
  List.iteri
    (fun i f -> Bytes.set_int64_le floats (8 * i) (Int64.bits_of_float f))
    [
      min_lon;
      min_lat;
      scale;
      scale;
      water_scale;
      water_scale;
      poi_scale;
      poi_scale;
    ];
  let sizes =
    Clc_file.write path
      {
        Clc_file.floats = Bytes.to_string floats;
        cover;
        water;
        poi_count = List.length pois;
        pois =
          List.map Buffer.contents
            [
              poi_encoder.names;
              poi_encoder.coords;
              poi_encoder.elevations;
              poi_encoder.types;
            ];
      }
  in
  List.fold_left ( + ) 112 (List.map (fun n -> n + 8) sizes)

let process_tile ?(coarse = false) ?overture ?poi_dir db_path output_dir
    tile_name =
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

  (* Define Bounds, with a margin for the far field's grid only (whose
     samples near the edge see past it). The sub-cells need none: see
     [sub_cells]. *)
  let margin_deg = if coarse then 0.005 else 0.0 in
  let min_lat = lat -. margin_deg in
  let max_lat = lat +. 1.0 +. margin_deg in
  let min_lon = lon -. margin_deg in
  let max_lon = lon +. 1.0 +. margin_deg in

  (* 
     Coordinate Normalization:
     A sub-cell maps onto [0, land_qmax] (see [sub_cells]), 1.7 m.
     
     Note: Since we use LAEA (meters) for extraction but WGS84 for storage,
     there is non-linear distortion, but at the scale of 1 degree, simple linear 
     interpolation in the vertex shader is visually acceptable for this use-case,
     especially since we triangulate densely enough.
  *)
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
    List.fold_left (fun acc (x, _) -> Float.min acc x) infinity laea_pts
  in
  let max_laea_x =
    List.fold_left (fun acc (x, _) -> Float.max acc x) neg_infinity laea_pts
  in
  let min_laea_y =
    List.fold_left (fun acc (_, y) -> Float.min acc y) infinity laea_pts
  in
  let max_laea_y =
    List.fold_left (fun acc (_, y) -> Float.max acc y) neg_infinity laea_pts
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

  let entry_count = ref 0 in
  let total_vertices = ref 0 in
  let total_indices = ref 0 in
  (*
     Recursive helper: clipped multipolygon handling.
     The threshold keeps each piece within the encoder's 16-bit vertex and
     triangle counts (with margin for triangulation bridges); the largest
     CORINE features approach a million points.
  *)
  let max_clipped_verts = 60000 in

  let rec clip_with_split_fixed flat_verts proper_poly region depth =
    let clipped_verts, result_polys =
      Polygon_clipping.Clipper.clip_multipolygon flat_verts [| proper_poly |]
        region
    in
    if Array.length result_polys = 0 then []
    else
      let vert_count = Array.length clipped_verts / 2 in
      (* Return if small enough OR if we've hit max recursion depth
         (2^8 = 256 subregions is enough for the largest features) *)
      if vert_count <= max_clipped_verts || depth > 8 then
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

  (* Where a feature goes: the far field's grid has the cell whole, the fine
     tiles a sub-cell each, row 0 the southern one. *)
  let regions =
    if coarse then [ (0, clipper_region) ]
    else
      let edge i v = v +. (float i /. float sub_cells) in
      List.init (sub_cells * sub_cells) (fun s ->
          let row = s / sub_cells and col = s mod sub_cells in
          ( s,
            {
              Geometry_types.min_x = edge col lon;
              min_y = edge row lat;
              max_x = edge (col + 1) lon;
              max_y = edge (row + 1) lat;
            } ))
  in
  (* A feature clipped to each region its outer ring's box meets, the pieces
     tagged with the region. *)
  let clip_to_regions flat_verts (poly : Geometry_types.polygon) =
    let o = poly.outer in
    let bx0 = ref infinity and bx1 = ref neg_infinity in
    let by0 = ref infinity and by1 = ref neg_infinity in
    for i = o.start to o.start + o.len - 1 do
      let x = flat_verts.(2 * i) and y = flat_verts.((2 * i) + 1) in
      bx0 := Float.min !bx0 x;
      bx1 := Float.max !bx1 x;
      by0 := Float.min !by0 y;
      by1 := Float.max !by1 y
    done;
    List.concat_map
      (fun (s, (r : Geometry_types.rect)) ->
        if !bx1 < r.min_x || !bx0 > r.max_x || !by1 < r.min_y || !by0 > r.max_y
        then []
        else
          List.map
            (fun piece -> (s, piece))
            (clip_with_split_fixed flat_verts poly r 0))
      regions
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
            | Some raw ->
                begin match Wkb_decode.decode_wkb raw with
                | Some geom ->
                    (* 1. Convert Geometry to float array (MultiPolygon) *)
                    let polys =
                      match geom with
                      | Wkb_decode.Polygon rings ->
                          if rings = [] then [||] else [| rings |]
                      | Wkb_decode.MultiPolygon polys ->
                          Array.of_list
                            (List.filter (fun rings -> rings <> []) polys)
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
                                   (strip_closing_point (List.hd rings)))
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
                                            (strip_closing_point ring))
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
                            clip_to_regions flat_verts proper_poly
                          in

                          List.iter
                            (fun (sub, (clipped_verts, clipped_poly)) ->
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
                                    sub,
                                    code,
                                    v_count,
                                    tri_count_val,
                                    valid_i_count,
                                    final_verts,
                                    final_indices )
                                  :: !batch_acc))
                            clipped_pieces)
                        float_polys
                | None ->
                    Printf.printf
                      "Warning: skipping undecodable geometry (code %d)\n%!"
                      code
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
  ignore (Sqlite3.finalize stmt);

  (* Sorting by Area Ascending (Smallest First) *)
  let sorted_features =
    List.sort
      (fun (a1, _, _, _, _, _, _, _) (a2, _, _, _, _, _, _, _) ->
        Float.compare a1 a2)
      all_features
  in

  (* === WATER LAYER PROCESSING === *)
  (* The water areas, triangulated: from a file of Overture's if there is one
     (the sea with them), from the Overpass API otherwise. The far field's
     cover has them only from a file. *)
  let water_features =
    match overture with
    | Some dir ->
        Overture.load_water
          (Filename.concat dir (tile_name ^ ".water.geojsonl"))
    | None when coarse -> []
    | None ->
        Printf.printf "Fetching water polygons from OSM...\n%!";
        Osm_fetch.fetch_water_polygons ~min_lat ~min_lon ~max_lat ~max_lon
  in
  let water_pieces = ref [] in

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
            let clipped_pieces = clip_to_regions flat_verts proper_poly in

            List.iter
              (fun (sub, (clipped_verts, clipped_poly)) ->
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

                  if v_count > 0 && tri_count_val > 0 then
                    water_pieces :=
                      ( sub,
                        clc_code,
                        final_verts,
                        v_count,
                        final_indices,
                        tri_count_val,
                        valid_i_count )
                      :: !water_pieces
                end)
              clipped_pieces
          end)
        flat_arrays)
    water_features;
  let water_pieces = List.rev !water_pieces in

  (* The POIs of the cell, a summit on its north or east edge being the next
     cell's: the 1° POI tile of the far field, and the fine tiles'. *)
  let pois =
    lazy
      (List.filter
         (fun (poi : Poi_fetch.poi) ->
           poi.lat >= lat
           && poi.lat < lat +. 1.
           && poi.lon >= lon
           && poi.lon < lon +. 1.)
         (match overture with
         | Some dir ->
             Overture.load_pois
               (Filename.concat dir (tile_name ^ ".pois.geojsonl"))
         | None -> Poi_fetch.fetch_pois ~min_lat ~min_lon ~max_lat ~max_lon))
  in
  Option.iter
    (fun dir ->
      let pois = Lazy.force pois in
      let bytes =
        write_tile
          (Filename.concat dir (tile_name ^ ".clc"))
          (lon, lat) 1.0 ~cover:[||] ~water:[||] ~pois
      in
      Printf.printf "%d POIs on their own: %d bytes.\n%!" (List.length pois)
        bytes)
    poi_dir;

  if coarse then begin
    let grid = Coarse.create () in
    (* The water of OSM first, which then has its samples, and in place of
       CORINE's: what that leaves empty is filled with the land around. But
       for the smaller ponds: each would be a sample of water, when CORINE
       draws nothing under 100 m across. And the water courses are those of
       both: CORINE draws one 100 m across whole, when what OSM has of its
       water is too narrow for the samples here and there, and has the bed of
       gravel of a wider one for sand, where OSM has the water. *)
    let water = Coarse.create_fine () in
    List.iter
      (fun (_, code, verts, _, indices, tri_count, _) ->
        if code = 511 || Coarse.area verts indices tri_count >= 4. then
          Coarse.fill ~fine:true water ~lon ~lat
            ~index:(Clc_palette.get_index code)
            verts indices tri_count)
      water_pieces;
    Coarse.fill_water grid water;
    Coarse.drop_specks grid ~index:(Clc_palette.get_index 511) ~min:8;
    List.iter
      (fun (_area, _, code, _, tri_count, _, verts, indices) ->
        if overture = None || code = 511 || not (is_water_code code) then
          Coarse.fill grid ~lon ~lat
            ~index:(Clc_palette.get_index code)
            verts indices tri_count)
      sorted_features;
    if overture <> None then
      Coarse.fill_holes grid ~is_land:(fun index ->
          index < Clc_palette.get_index 511);
    ignore (Sqlite3.db_close db);
    (* A cell CORINE has nothing for gets no tiles: the client takes a missing
       one for open sea. *)
    if sorted_features = [] then begin
      Printf.printf "No features: nothing written.\n%!";
      exit 0
    end;
    (* What CORINE does not cover is open sea. *)
    let default = Clc_palette.get_index 523 in
    let bytes = ref 0 in
    for row = 0 to 2 do
      for col = 0 to 2 do
        (* Row 0 is the southern one, as for the elevation. *)
        bytes :=
          !bytes
          + Coarse.write grid ~default ~row:(2 - row) ~col
              (Filename.concat output_dir
                 (sub_tile_name ~lat ~lon ~row ~col "dem"))
      done
    done;
    Printf.printf "Done. %d features over 9 tiles, %d bytes.\n%!"
      (List.length sorted_features)
      !bytes;
    exit 0
  end;

  let step = 1.0 /. float sub_cells in
  let tiles = ref 0 and bytes = ref 0 in
  for row = 0 to sub_cells - 1 do
    for col = 0 to sub_cells - 1 do
      let s = (row * sub_cells) + col in
      let origin = (lon +. (float col *. step), lat +. (float row *. step)) in
      let quantize verts qmax =
        quantize verts origin (float qmax /. step, float qmax /. step) qmax
      in
      let cover =
        Array.of_list
          (List.filter_map
             (fun (_area, sub, code, _, _, valid_i_count, verts, indices) ->
               if sub <> s then None
               else
                 let xs, ys = quantize verts land_qmax in
                 Some
                   {
                     Clc_codec.code;
                     xs;
                     ys;
                     tris = Array.sub indices 0 valid_i_count;
                   })
             sorted_features)
      in
      let water =
        Array.of_list
          (List.filter_map
             (fun (sub, code, verts, _, indices, _, i_count) ->
               if sub <> s then None
               else
                 let xs, ys = quantize verts water_qmax in
                 Some
                   {
                     Clc_codec.code;
                     xs;
                     ys;
                     tris = Array.sub indices 0 i_count;
                   })
             water_pieces)
      in
      let pois =
        List.filter
          (fun (poi : Poi_fetch.poi) ->
            let sub_of v v0 =
              max 0
                (min (sub_cells - 1)
                   (int_of_float (Float.floor ((v -. v0) /. step))))
            in
            sub_of poi.lat lat = row && sub_of poi.lon lon = col)
          (Lazy.force pois)
      in
      (* A sub-cell with nothing in it gets no tile: the client takes a
         missing one for open sea. *)
      if cover <> [||] || water <> [||] || pois <> [] then begin
        incr tiles;
        bytes :=
          !bytes
          + write_tile
              (Filename.concat output_dir
                 (sub_tile_name ~lat ~lon ~row ~col "clc"))
              origin step ~cover ~water ~pois
      end
    done
  done;
  ignore (Sqlite3.db_close db);
  Printf.printf
    "Done. Extracted %d CLC features (%d verts, %d indices) + %d water \
     features + %d POIs: %d tiles, %d bytes.\n\
     %!"
    !entry_count !total_vertices !total_indices (List.length water_pieces)
    (List.length (Lazy.force pois))
    !tiles !bytes

(* The POIs of a tile on their own, as a tile with no land cover and no water
   that the client reads like any other: what it takes of a cell it only sees
   from afar, whose cover comes with [Coarse]. The POI streams are copied as
   they are. *)
let extract_pois input output =
  let tile = Clc_file.read input in
  ignore (Clc_file.write output { tile with cover = [||]; water = [||] })

(* A tile, CLC5 or CLC6, rewritten as CLC6: the same features, read back from
   the file written and compared with those read. *)
let convert input output =
  let tile = Clc_file.read input in
  let sizes = Clc_file.write output tile in
  let back = Clc_file.read output in
  (match
     ( Clc_codec.differences tile.cover back.cover,
       Clc_codec.differences tile.water back.water )
   with
  | None, None -> ()
  | Some d, _ -> failwith (output ^ ": land cover differs, " ^ d)
  | _, Some d -> failwith (output ^ ": water differs, " ^ d));
  if back.pois <> tile.pois || back.poi_count <> tile.poi_count then
    failwith (output ^ ": POIs differ");
  Printf.printf "%s: %d -> %d bytes (%s)\n%!" output
    (Unix.stat input).Unix.st_size
    (List.fold_left ( + ) 112 (List.map (fun n -> n + 8) sizes))
    (String.concat " " (List.map string_of_int sizes))

let () =
  let rec options ?(coarse = false) ?overture ?poi_dir = function
    | "--coarse" :: rest -> options ~coarse:true ?overture ?poi_dir rest
    | "--overture" :: dir :: rest -> options ~coarse ~overture:dir ?poi_dir rest
    | "--poi-dir" :: dir :: rest -> options ~coarse ?overture ~poi_dir:dir rest
    | [ db; output_dir; tile ] ->
        process_tile ~coarse ?overture ?poi_dir db output_dir tile
    | _ ->
        Printf.printf
          "Usage: %s [--coarse] [--overture <dir>] [--poi-dir <dir>] <DB> \
           <Output Dir> <TileCode> (e.g. N45E006)\n\
          \  the cell's 36 sub-cells of 600 arcseconds, \
           N45_E006_<row>_<col>.clc, row 0 the southern one\n\
          \  --coarse: the far field's land cover instead, nine tiles of \
           palette indices at 4 arcseconds\n\
          \  --poi-dir: the cell's POIs on their own too, <dir>/N45E006.clc\n\
          \  --overture: the water, the sea included, and the POIs from the \
           files of <dir> (fetch_overture.sh) and not from the Overpass API\n\
          \       %s --pois <tile.clc> <output.clc>\n\
          \  the tile's POIs alone\n\
          \       %s --convert <tile.clc> <output.clc>\n\
          \  a CLC5 tile as CLC6, checked\n"
          Sys.argv.(0) Sys.argv.(0) Sys.argv.(0)
  in
  match List.tl (Array.to_list Sys.argv) with
  | [ "--pois"; input; output ] -> extract_pois input output
  | [ "--convert"; input; output ] -> convert input output
  | args -> options args
