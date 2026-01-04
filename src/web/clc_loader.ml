(* clc_loader.ml - Load and rasterize CLC tiles for web viewer *)

open Bigarray
open Brr

let ( >>= ) = Lwt.bind

(* CLC tile header info *)
type clc_header = {
  count : int;
  total_verts : int;
  total_indices : int;
  water_count : int; (* CLC4 only *)
  water_verts : int; (* CLC4 only *)
  water_indices : int; (* CLC4 only *)
  min_lon : float;
  min_lat : float;
  scale_x : float;
  scale_y : float;
  water_scale_x : float; (* CLC4 only *)
  water_scale_y : float; (* CLC4 only *)
  is_clc4 : bool;
}

(* Parsed CLC tile data ready for rasterization *)
type clc_tile = {
  header : clc_header;
  positions : (int, int16_unsigned_elt, c_layout) Array1.t; (* x,y pairs U16 *)
  colors : (int, int8_unsigned_elt, c_layout) Array1.t; (* palette indices *)
  indices : (int32, int32_elt, c_layout) Array1.t;
  (* Water layer (CLC4 only) - stored as U16 like CLC after scaling *)
  water_positions : (int, int16_unsigned_elt, c_layout) Array1.t;
  water_colors : (int, int8_unsigned_elt, c_layout) Array1.t;
  water_indices : (int32, int32_elt, c_layout) Array1.t;
}

(* Read big-endian 32-bit int from string at offset (OCaml's input_binary_int format) *)
let read_i32_be s offset =
  let b0 = Char.code s.[offset] in
  let b1 = Char.code s.[offset + 1] in
  let b2 = Char.code s.[offset + 2] in
  let b3 = Char.code s.[offset + 3] in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

(* Read little-endian 32-bit int from string at offset (for stream lengths) *)
let read_i32_le s offset =
  let b0 = Char.code s.[offset] in
  let b1 = Char.code s.[offset + 1] in
  let b2 = Char.code s.[offset + 2] in
  let b3 = Char.code s.[offset + 3] in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

(* Read little-endian 64-bit float from string *)
let read_f64_le s offset =
  let b0 = Int64.of_int (Char.code s.[offset]) in
  let b1 = Int64.of_int (Char.code s.[offset + 1]) in
  let b2 = Int64.of_int (Char.code s.[offset + 2]) in
  let b3 = Int64.of_int (Char.code s.[offset + 3]) in
  let b4 = Int64.of_int (Char.code s.[offset + 4]) in
  let b5 = Int64.of_int (Char.code s.[offset + 5]) in
  let b6 = Int64.of_int (Char.code s.[offset + 6]) in
  let b7 = Int64.of_int (Char.code s.[offset + 7]) in
  let bits =
    Int64.logor b0
      (Int64.logor (Int64.shift_left b1 8)
         (Int64.logor (Int64.shift_left b2 16)
            (Int64.logor (Int64.shift_left b3 24)
               (Int64.logor (Int64.shift_left b4 32)
                  (Int64.logor (Int64.shift_left b5 40)
                     (Int64.logor (Int64.shift_left b6 48)
                        (Int64.shift_left b7 56)))))))
  in
  Int64.float_of_bits bits

(* ZigZag decode - for delta-encoded coordinates *)
let zigzag_decode n = (n lsr 1) lxor -(n land 1)

(* Parse CLC file header - supports both CLC3 and CLC4 formats *)
let parse_header s =
  let magic = String.sub s 0 4 in
  let is_clc4 = magic = "CLC4" in
  if magic <> "CLC3" && magic <> "CLC4" then
    failwith ("Invalid CLC magic: " ^ magic);

  let count = read_i32_be s 4 in
  let total_verts = read_i32_be s 8 in
  let total_indices = read_i32_be s 12 in

  (* CLC4 has additional water counts after CLC counts *)
  let water_count, water_verts, water_indices, float_offset =
    if is_clc4 then (read_i32_be s 16, read_i32_be s 20, read_i32_be s 24, 28)
    else (0, 0, 0, 16)
  in

  let min_lon = read_f64_le s float_offset in
  let min_lat = read_f64_le s (float_offset + 8) in
  let scale_x = read_f64_le s (float_offset + 16) in
  let scale_y = read_f64_le s (float_offset + 24) in

  (* CLC4 has water scales *)
  let water_scale_x, water_scale_y =
    if is_clc4 then
      (read_f64_le s (float_offset + 32), read_f64_le s (float_offset + 40))
    else (0.0, 0.0)
  in

  {
    count;
    total_verts;
    total_indices;
    water_count;
    water_verts;
    water_indices;
    min_lon;
    min_lat;
    scale_x;
    scale_y;
    water_scale_x;
    water_scale_y;
    is_clc4;
  }

(* Get CLC tile name from lat/lon *)
let tile_name lat lon =
  let lat_int = int_of_float (floor lat) in
  let lon_int = int_of_float (floor lon) in
  let lat_str = if lat_int >= 0 then "N" else "S" in
  let lon_str = if lon_int >= 0 then "E" else "W" in
  Printf.sprintf "%s%02d%s%03d.clc" lat_str (abs lat_int) lon_str (abs lon_int)

(* Tile path for fetching *)
let tile_path lat lon = "data/clc/" ^ tile_name lat lon

(* Read a compressed stream from data at offset, return (decompressed, new_offset) *)
let read_stream data offset =
  let open Lwt.Syntax in
  let comp_len = read_i32_le data offset in
  let compressed = String.sub data (offset + 4) comp_len in
  (* Convert string to uint8 array for pako *)
  let tarray = Reader.uint8_of_string compressed in
  (* Decompress and return string directly *)
  let* decompressed = Reader.inflate_to_string tarray in
  Lwt.return (decompressed, offset + 4 + comp_len)

(* Decode CLC vertices and triangles from streams *)
let decode_clc_streams header meta_str high_x low_x high_y low_y high_indices
    low_indices =
  let n_verts = header.total_verts in
  let n_indices = header.total_indices in
  let arr_pos = Array1.create int16_unsigned c_layout (n_verts * 2) in
  let arr_col = Array1.create int8_unsigned c_layout n_verts in
  let arr_ebo = Array1.create int32 c_layout n_indices in

  let meta_pos = ref 0 in
  let v_pos = ref 0 in
  let i_pos = ref 0 in
  let global_v_offset = ref 0 in
  let global_i_offset = ref 0 in

  let read_u16 str pos =
    let b0 = Char.code str.[!pos] in
    let b1 = Char.code str.[!pos + 1] in
    pos := !pos + 2;
    b0 lor (b1 lsl 8)
  in

  for _ = 1 to header.count do
    let code = read_u16 meta_str meta_pos in
    let v_count = read_u16 meta_str meta_pos in
    let t_count = read_u16 meta_str meta_pos in
    let code_idx = Clc_palette.get_index code in
    let base_v = !global_v_offset in

    (* Decode vertices *)
    let prev_x = ref 0 in
    let prev_y = ref 0 in
    for k = 0 to v_count - 1 do
      let idx = !v_pos + k in
      let hx = Char.code high_x.[idx] in
      let lx = Char.code low_x.[idx] in
      let zx = lx lor (hx lsl 8) in
      let sdx = zigzag_decode zx in
      let qx = (!prev_x + sdx) land 0xFFFF in
      prev_x := qx;

      let hy = Char.code high_y.[idx] in
      let ly = Char.code low_y.[idx] in
      let zy = ly lor (hy lsl 8) in
      let sdy = zigzag_decode zy in
      let qy = (!prev_y + sdy) land 0xFFFF in
      prev_y := qy;

      let out_idx = base_v + k in
      Array1.set arr_pos (out_idx * 2) qx;
      Array1.set arr_pos ((out_idx * 2) + 1) qy;
      Array1.set arr_col out_idx code_idx
    done;
    v_pos := !v_pos + v_count;
    global_v_offset := !global_v_offset + v_count;

    (* Decode indices *)
    let prev_idx = ref 0 in
    let num_indices = t_count * 3 in
    for k = 0 to num_indices - 1 do
      let idx = !i_pos + k in
      let hi = Char.code high_indices.[idx] in
      let li = Char.code low_indices.[idx] in
      let zi = li lor (hi lsl 8) in
      let sdi = zigzag_decode zi in
      let qi = (!prev_idx + sdi) land 0xFFFF in
      prev_idx := qi;
      Array1.set arr_ebo (!global_i_offset + k) (Int32.of_int (base_v + qi))
    done;
    i_pos := !i_pos + num_indices;
    global_i_offset := !global_i_offset + num_indices
  done;
  (arr_pos, arr_col, arr_ebo)

(* Decode water vertices and triangles from CLC4 streams (3-byte coords) *)
let decode_water_streams header meta_str high_x mid_x low_x high_y mid_y low_y
    high_indices low_indices =
  let n_verts = header.water_verts in
  let n_indices = header.water_indices in
  let arr_pos = Array1.create int16_unsigned c_layout (n_verts * 2) in
  let arr_col = Array1.create int8_unsigned c_layout n_verts in
  let arr_ebo = Array1.create int32 c_layout n_indices in

  if n_verts = 0 then (arr_pos, arr_col, arr_ebo)
  else begin
    let meta_pos = ref 0 in
    let v_pos = ref 0 in
    let i_pos = ref 0 in
    let global_v_offset = ref 0 in
    let global_i_offset = ref 0 in

    let read_u16 str pos =
      let b0 = Char.code str.[!pos] in
      let b1 = Char.code str.[!pos + 1] in
      pos := !pos + 2;
      b0 lor (b1 lsl 8)
    in

    for _ = 1 to header.water_count do
      let code = read_u16 meta_str meta_pos in
      let v_count = read_u16 meta_str meta_pos in
      let t_count = read_u16 meta_str meta_pos in
      let code_idx = Clc_palette.get_index code in
      let base_v = !global_v_offset in

      (* Decode 3-byte coordinates and scale to 16-bit *)
      let prev_x = ref 0 in
      let prev_y = ref 0 in
      for k = 0 to v_count - 1 do
        let idx = !v_pos + k in
        let hx = Char.code high_x.[idx] in
        let mx = Char.code mid_x.[idx] in
        let lx = Char.code low_x.[idx] in
        let zx = lx lor (mx lsl 8) lor (hx lsl 16) in
        let sdx = zigzag_decode zx in
        let qx = (!prev_x + sdx) land 0xFFFFFF in
        prev_x := qx;

        let hy = Char.code high_y.[idx] in
        let my = Char.code mid_y.[idx] in
        let ly = Char.code low_y.[idx] in
        let zy = ly lor (my lsl 8) lor (hy lsl 16) in
        let sdy = zigzag_decode zy in
        let qy = (!prev_y + sdy) land 0xFFFFFF in
        prev_y := qy;

        (* Scale from water range (0-220000) to u16 range (0-65535) 
           Use float to avoid overflow on 32-bit JS integers *)
        let scaled_x = int_of_float (float_of_int qx *. 65535. /. 220000.) in
        let scaled_y = int_of_float (float_of_int qy *. 65535. /. 220000.) in

        let out_idx = base_v + k in
        Array1.set arr_pos (out_idx * 2) scaled_x;
        Array1.set arr_pos ((out_idx * 2) + 1) scaled_y;
        Array1.set arr_col out_idx code_idx
      done;
      v_pos := !v_pos + v_count;
      global_v_offset := !global_v_offset + v_count;

      (* Decode indices (same as CLC - 16-bit) *)
      let prev_idx = ref 0 in
      let num_indices = t_count * 3 in
      for k = 0 to num_indices - 1 do
        let idx = !i_pos + k in
        let hi = Char.code high_indices.[idx] in
        let li = Char.code low_indices.[idx] in
        let zi = li lor (hi lsl 8) in
        let sdi = zigzag_decode zi in
        let qi = (!prev_idx + sdi) land 0xFFFF in
        prev_idx := qi;
        Array1.set arr_ebo (!global_i_offset + k) (Int32.of_int (base_v + qi))
      done;
      i_pos := !i_pos + num_indices;
      global_i_offset := !global_i_offset + num_indices
    done;
    (arr_pos, arr_col, arr_ebo)
  end

(* Edge function for triangle rasterization *)
let edge_function x1 y1 x2 y2 px py =
  ((px - x1) * (y2 - y1)) - ((py - y1) * (x2 - x1))

(* Rasterize a single triangle to the texture *)
let rasterize_triangle data size x0 y0 x1 y1 x2 y2 color_idx =
  (* Convert normalized U16 coords (0-65535) to texture coords (0-size) *)
  let tx0 = x0 * size / 65536 in
  let ty0 = y0 * size / 65536 in
  let tx1 = x1 * size / 65536 in
  let ty1 = y1 * size / 65536 in
  let tx2 = x2 * size / 65536 in
  let ty2 = y2 * size / 65536 in

  (* Bounding box *)
  let min_x = max 0 (min tx0 (min tx1 tx2)) in
  let max_x = min (size - 1) (max tx0 (max tx1 tx2)) in
  let min_y = max 0 (min ty0 (min ty1 ty2)) in
  let max_y = min (size - 1) (max ty0 (max ty1 ty2)) in

  (* Rasterize using edge functions *)
  for py = min_y to max_y do
    for px = min_x to max_x do
      let w0 = edge_function tx1 ty1 tx2 ty2 px py in
      let w1 = edge_function tx2 ty2 tx0 ty0 px py in
      let w2 = edge_function tx0 ty0 tx1 ty1 px py in
      (* Check if point is inside triangle (all same sign) *)
      if (w0 >= 0 && w1 >= 0 && w2 >= 0) || (w0 <= 0 && w1 <= 0 && w2 <= 0) then
        Array1.set data ((py * size) + px) color_idx
    done
  done

(* Rasterize CLC tile to a texture, mapping geographic bounds.
   - tile: the loaded CLC tile with header containing min_lon, min_lat, scale_x, scale_y
   - tex_size: output texture size (e.g., 1024)
   - target_min_lon/lat, target_max_lon/lat: DEM coverage bounds in degrees
   - nominal_min/max_lon/lat: Tile's nominal 1° boundaries (to avoid overlap issues)
   The header's scale_x/scale_y are the quantization divisors: 
   geographic_coord = min + (quantized / scale) *)
let rasterize_clc_tile_to_bounds tile tex_size ~target_min_lon ~target_min_lat
    ~target_max_lon ~target_max_lat ~nominal_min_lon ~nominal_min_lat
    ~nominal_max_lon ~nominal_max_lat =
  let data = Array1.create int8_unsigned c_layout (tex_size * tex_size) in
  Array1.fill data 0;

  let header = tile.header in
  let positions = tile.positions in
  let colors = tile.colors in
  let indices = tile.indices in
  let n_triangles = Array1.dim indices / 3 in

  (* Target bounds in texture *)
  let target_width = target_max_lon -. target_min_lon in
  let target_height = target_max_lat -. target_min_lat in
  let tex_scale_x = float tex_size /. target_width in
  let tex_scale_y = float tex_size /. target_height in

  (* Debug: log bounds and scale *)
  Console.(
    log
      [
        Jstr.v
          (Printf.sprintf
             "  Rasterize: target=[%.4f,%.4f]-[%.4f,%.4f] scale=%.1f,%.1f"
             target_min_lon target_min_lat target_max_lon target_max_lat
             tex_scale_x tex_scale_y);
      ]);
  Console.(
    log
      [
        Jstr.v
          (Printf.sprintf "  Header: min=[%.4f,%.4f] scale=[%.1f,%.1f]"
             header.min_lon header.min_lat header.scale_x header.scale_y);
      ]);

  for t = 0 to n_triangles - 1 do
    let i0 = Int32.to_int (Array1.get indices (t * 3)) in
    let i1 = Int32.to_int (Array1.get indices ((t * 3) + 1)) in
    let i2 = Int32.to_int (Array1.get indices ((t * 3) + 2)) in

    (* Get quantized coords (0-65535 range) *)
    let qx0 = Array1.get positions (i0 * 2) in
    let qy0 = Array1.get positions ((i0 * 2) + 1) in
    let qx1 = Array1.get positions (i1 * 2) in
    let qy1 = Array1.get positions ((i1 * 2) + 1) in
    let qx2 = Array1.get positions (i2 * 2) in
    let qy2 = Array1.get positions ((i2 * 2) + 1) in

    (* Convert to geographic coords using header bounds *)
    let geo_x0 = header.min_lon +. (float qx0 /. header.scale_x) in
    let geo_y0 = header.min_lat +. (float qy0 /. header.scale_y) in
    let geo_x1 = header.min_lon +. (float qx1 /. header.scale_x) in
    let geo_y1 = header.min_lat +. (float qy1 /. header.scale_y) in
    let geo_x2 = header.min_lon +. (float qx2 /. header.scale_x) in
    let geo_y2 = header.min_lat +. (float qy2 /. header.scale_y) in

    (* NOTE: Nominal clipping disabled - was incorrectly rejecting valid triangles
       because CLC tiles extend beyond their nominal 1° boundaries *)
    let _ =
      (nominal_min_lon, nominal_max_lon, nominal_min_lat, nominal_max_lat)
    in

    begin
      (* Always process - no nominal clipping *)
      (* Convert to texture coords *)
      let tx0 = int_of_float ((geo_x0 -. target_min_lon) *. tex_scale_x) in
      let ty0 = int_of_float ((geo_y0 -. target_min_lat) *. tex_scale_y) in
      let tx1 = int_of_float ((geo_x1 -. target_min_lon) *. tex_scale_x) in
      let ty1 = int_of_float ((geo_y1 -. target_min_lat) *. tex_scale_y) in
      let tx2 = int_of_float ((geo_x2 -. target_min_lon) *. tex_scale_x) in
      let ty2 = int_of_float ((geo_y2 -. target_min_lat) *. tex_scale_y) in

      let color_idx = Array1.get colors i0 in

      (* Early rejection: skip triangles entirely outside texture bounds *)
      let all_left = tx0 < 0 && tx1 < 0 && tx2 < 0 in
      let all_right = tx0 >= tex_size && tx1 >= tex_size && tx2 >= tex_size in
      let all_below = ty0 < 0 && ty1 < 0 && ty2 < 0 in
      let all_above = ty0 >= tex_size && ty1 >= tex_size && ty2 >= tex_size in

      (* Also skip huge triangles that could cause overflow (> 2x texture size) *)
      let max_dim = tex_size * 2 in
      let width = max tx0 (max tx1 tx2) - min tx0 (min tx1 tx2) in
      let height = max ty0 (max ty1 ty2) - min ty0 (min ty1 ty2) in
      let too_big = width > max_dim || height > max_dim in

      if not (all_left || all_right || all_below || all_above || too_big) then begin
        (* Bounding box with clipping to texture bounds *)
        let min_x = max 0 (min tx0 (min tx1 tx2)) in
        let max_x = min (tex_size - 1) (max tx0 (max tx1 tx2)) in
        let min_y = max 0 (min ty0 (min ty1 ty2)) in
        let max_y = min (tex_size - 1) (max ty0 (max ty1 ty2)) in

        (* Rasterize *)
        for py = min_y to max_y do
          for px = min_x to max_x do
            let w0 = edge_function tx1 ty1 tx2 ty2 px py in
            let w1 = edge_function tx2 ty2 tx0 ty0 px py in
            let w2 = edge_function tx0 ty0 tx1 ty1 px py in
            if (w0 >= 0 && w1 >= 0 && w2 >= 0) || (w0 <= 0 && w1 <= 0 && w2 <= 0)
            then Array1.set data ((py * tex_size) + px) color_idx
          done
        done
      end
    end
    (* end nominal bounds check *)
  done;

  (* Count non-zero pixels for debugging *)
  let non_zero = ref 0 in
  for i = 0 to (tex_size * tex_size) - 1 do
    if Array1.get data i <> 0 then incr non_zero
  done;

  Console.(
    log
      [
        Jstr.v
          (Printf.sprintf "Rasterized %d triangles, %d pixels filled"
             n_triangles !non_zero);
      ]);
  data

(* Legacy function for backward compatibility - assumes 1:1 quantized to texture mapping *)
let rasterize_clc_tile tile size =
  let data = Array1.create int8_unsigned c_layout (size * size) in
  (* Fill with default (fallback color) - use 0 which maps to magenta for visibility *)
  Array1.fill data 0;

  let positions = tile.positions in
  let colors = tile.colors in
  let indices = tile.indices in
  let n_triangles = Array1.dim indices / 3 in
  let pixels_written = ref 0 in
  let color_counts = Array.make 50 0 in

  for t = 0 to n_triangles - 1 do
    let i0 = Int32.to_int (Array1.get indices (t * 3)) in
    let i1 = Int32.to_int (Array1.get indices ((t * 3) + 1)) in
    let i2 = Int32.to_int (Array1.get indices ((t * 3) + 2)) in

    let x0 = Array1.get positions (i0 * 2) in
    let y0 = Array1.get positions ((i0 * 2) + 1) in
    let x1 = Array1.get positions (i1 * 2) in
    let y1 = Array1.get positions ((i1 * 2) + 1) in
    let x2 = Array1.get positions (i2 * 2) in
    let y2 = Array1.get positions ((i2 * 2) + 1) in

    (* Use color from first vertex *)
    let color_idx = Array1.get colors i0 in
    if color_idx < 50 then
      color_counts.(color_idx) <- color_counts.(color_idx) + 1;

    (* Rasterize with pixel counting *)
    let tx0 = x0 * size / 65536 in
    let ty0 = y0 * size / 65536 in
    let tx1 = x1 * size / 65536 in
    let ty1 = y1 * size / 65536 in
    let tx2 = x2 * size / 65536 in
    let ty2 = y2 * size / 65536 in
    let min_x = max 0 (min tx0 (min tx1 tx2)) in
    let max_x = min (size - 1) (max tx0 (max tx1 tx2)) in
    let min_y = max 0 (min ty0 (min ty1 ty2)) in
    let max_y = min (size - 1) (max ty0 (max ty1 ty2)) in
    for py = min_y to max_y do
      for px = min_x to max_x do
        let w0 = edge_function tx1 ty1 tx2 ty2 px py in
        let w1 = edge_function tx2 ty2 tx0 ty0 px py in
        let w2 = edge_function tx0 ty0 tx1 ty1 px py in
        if (w0 >= 0 && w1 >= 0 && w2 >= 0) || (w0 <= 0 && w1 <= 0 && w2 <= 0)
        then begin
          Array1.set data ((py * size) + px) color_idx;
          incr pixels_written
        end
      done
    done
  done;

  (* Log statistics *)
  Console.(
    log
      [
        Jstr.v
          ("Triangles: " ^ string_of_int n_triangles ^ ", Pixels written: "
          ^ string_of_int !pixels_written);
      ]);
  (* Log first few color counts *)
  let color_info = Buffer.create 100 in
  for i = 0 to 10 do
    if color_counts.(i) > 0 then
      Buffer.add_string color_info (Printf.sprintf " c%d=%d" i color_counts.(i))
  done;
  Console.(log [ Jstr.v ("Color distribution:" ^ Buffer.contents color_info) ]);
  data

(* Full CLC tile loading: decompress all streams and decode geometry *)
let load_full_clc_tile path =
  let open Lwt.Syntax in
  let* data = Reader.read_file path in
  let header = parse_header data in
  (* Header size varies by format *)
  let offset = if header.is_clc4 then 76 else 48 in

  (* Read CLC streams (7 for CLC3, same for CLC4) *)
  let* meta_str, offset = read_stream data offset in
  let* high_x, offset = read_stream data offset in
  let* low_x, offset = read_stream data offset in
  let* high_y, offset = read_stream data offset in
  let* low_y, offset = read_stream data offset in
  let* high_indices, offset = read_stream data offset in
  let* low_indices, offset = read_stream data offset in

  (* Decode CLC geometry *)
  let clc_pos, clc_col, clc_ebo =
    decode_clc_streams header meta_str high_x low_x high_y low_y high_indices
      low_indices
  in

  (* Read and decode water streams if CLC4 *)
  let* water_pos, water_col, water_ebo =
    if header.is_clc4 && header.water_count > 0 then begin
      let* w_meta, offset = read_stream data offset in
      let* w_high_x, offset = read_stream data offset in
      let* w_mid_x, offset = read_stream data offset in
      let* w_low_x, offset = read_stream data offset in
      let* w_high_y, offset = read_stream data offset in
      let* w_mid_y, offset = read_stream data offset in
      let* w_low_y, offset = read_stream data offset in
      let* w_high_idx, offset = read_stream data offset in
      let* w_low_idx, _ = read_stream data offset in
      let wp, wc, we =
        decode_water_streams header w_meta w_high_x w_mid_x w_low_x w_high_y
          w_mid_y w_low_y w_high_idx w_low_idx
      in
      Lwt.return (wp, wc, we)
    end
    else begin
      (* Empty water arrays *)
      Lwt.return
        ( Array1.create int16_unsigned c_layout 0,
          Array1.create int8_unsigned c_layout 0,
          Array1.create int32 c_layout 0 )
    end
  in

  Lwt.return
    {
      header;
      positions = clc_pos;
      colors = clc_col;
      indices = clc_ebo;
      water_positions = water_pos;
      water_colors = water_col;
      water_indices = water_ebo;
    }

(* Load CLC tile and rasterize to texture data *)
let load_and_rasterize_clc path size =
  let open Lwt.Syntax in
  let* tile = load_full_clc_tile path in
  Console.(
    log
      [
        Jstr.v
          ("Rasterizing to " ^ string_of_int size ^ "x" ^ string_of_int size
         ^ " texture...");
      ]);
  let data = rasterize_clc_tile tile size in
  Console.(log [ Jstr.v "CLC rasterization complete" ]);
  Lwt.return (tile.header, data)

(* Load and parse CLC tile header only (for quick checks) *)
let load_clc_tile path =
  let open Lwt.Syntax in
  let* data = Reader.read_file path in
  let header = parse_header data in
  Console.(
    log
      [
        Jstr.v ("Loaded CLC: " ^ path ^ " with ");
        Jstr.v (string_of_int header.count);
        Jstr.v " polygons";
      ]);
  Lwt.return header

(* Load and rasterize CLC tiles to cover DEM geographic area.
   - lat, lon: camera position in degrees
   - size: DEM size in arcseconds (pixels)
   - tex_size: output texture resolution (e.g., 1024)
   Returns (min_lat_deg, min_lon_deg, width_deg, height_deg, data) *)
let load_and_rasterize_multi ~lat ~lon ~size ~tex_size =
  let open Lwt.Syntax in
  (* Calculate DEM geographic bounds in degrees *)
  let size_deg = float size /. 3600. in
  (* Convert arcseconds to degrees *)
  let dem_min_lat = lat -. (size_deg /. 2.) in
  let dem_max_lat = lat +. (size_deg /. 2.) in
  let dem_min_lon = lon -. (size_deg /. 2.) in
  let dem_max_lon = lon +. (size_deg /. 2.) in

  Console.(
    log
      [
        Jstr.v
          (Printf.sprintf
             "CLC: DEM bounds lat=%.4f-%.4f lon=%.4f-%.4f (%.3f deg)"
             dem_min_lat dem_max_lat dem_min_lon dem_max_lon size_deg);
      ]);

  (* Determine which CLC tiles might contribute (with margin for overlap) *)
  let min_tile_lat = int_of_float (floor dem_min_lat) - 1 in
  let max_tile_lat = int_of_float (ceil dem_max_lat) in
  let min_tile_lon = int_of_float (floor dem_min_lon) - 1 in
  let max_tile_lon = int_of_float (ceil dem_max_lon) in

  Console.(
    log
      [
        Jstr.v
          (Printf.sprintf "CLC: Tile range lat=%d-%d lon=%d-%d" min_tile_lat
             max_tile_lat min_tile_lon max_tile_lon);
      ]);

  (* Create output texture *)
  let data = Array1.create int8_unsigned c_layout (tex_size * tex_size) in
  Array1.fill data 0;

  (* Load and rasterize each tile directly to DEM bounds *)
  let rec load_tiles tile_lat tile_lon =
    if tile_lat > max_tile_lat then Lwt.return ()
    else if tile_lon > max_tile_lon then load_tiles (tile_lat + 1) min_tile_lon
    else begin
      let path = tile_path (float tile_lat +. 0.5) (float tile_lon +. 0.5) in
      Lwt.catch
        (fun () ->
          let* tile = load_full_clc_tile path in
          (* Rasterize this tile using proper geographic transform, 
             clipped to nominal 1° tile bounds *)
          let nominal_min_lon = float tile_lon in
          let nominal_max_lon = float (tile_lon + 1) in
          let nominal_min_lat = float tile_lat in
          let nominal_max_lat = float (tile_lat + 1) in
          let tile_data =
            rasterize_clc_tile_to_bounds tile tex_size
              ~target_min_lon:dem_min_lon ~target_min_lat:dem_min_lat
              ~target_max_lon:dem_max_lon ~target_max_lat:dem_max_lat
              ~nominal_min_lon ~nominal_min_lat ~nominal_max_lon
              ~nominal_max_lat
          in
          (* Merge: FIRST WINS - only write to empty pixels (prevents overlap artifacts) *)
          for i = 0 to (tex_size * tex_size) - 1 do
            let v = Array1.get tile_data i in
            let existing = Array1.get data i in
            if v <> 0 && existing = 0 then Array1.set data i v
          done;
          Lwt.return ())
        (fun _exn ->
          (* Tile not found - skip silently *)
          Lwt.return ())
      >>= fun () -> load_tiles tile_lat (tile_lon + 1)
    end
  in
  let* () = load_tiles min_tile_lat min_tile_lon in

  Console.(log [ Jstr.v "CLC: Multi-tile rasterization complete" ]);

  (* Return DEM geographic bounds and data *)
  let width_deg = dem_max_lon -. dem_min_lon in
  let height_deg = dem_max_lat -. dem_min_lat in
  Lwt.return (dem_min_lat, dem_min_lon, width_deg, height_deg, data)

(* Load CLC tiles for GPU rasterization (no CPU rasterization).
   Returns: (dem_min_lon, dem_min_lat, dem_range_lon, dem_range_lat, tiles list)
   Each tile in the list has: (tile_data, tile_range_lon, tile_range_lat) *)
let load_tiles_for_gpu ~lat ~lon ~size =
  let open Lwt.Syntax in
  (* Calculate DEM geographic bounds in degrees *)
  let size_deg = float size /. 3600. in
  let dem_min_lat = lat -. (size_deg /. 2.) in
  let dem_max_lat = lat +. (size_deg /. 2.) in
  let dem_min_lon = lon -. (size_deg /. 2.) in
  let dem_max_lon = lon +. (size_deg /. 2.) in
  let dem_range_lon = dem_max_lon -. dem_min_lon in
  let dem_range_lat = dem_max_lat -. dem_min_lat in

  (* Determine which CLC tiles contribute *)
  (* Tiles are named by lower-left corner: N45 covers [45,46), so use floor for all *)
  let min_tile_lat = int_of_float (floor dem_min_lat) in
  let max_tile_lat = int_of_float (floor dem_max_lat) in
  let min_tile_lon = int_of_float (floor dem_min_lon) in
  let max_tile_lon = int_of_float (floor dem_max_lon) in

  (* Accumulate loaded tiles *)
  let tiles = ref [] in

  let rec load_tiles tile_lat tile_lon =
    if tile_lat > max_tile_lat then Lwt.return ()
    else if tile_lon > max_tile_lon then load_tiles (tile_lat + 1) min_tile_lon
    else begin
      let path = tile_path (float tile_lat +. 0.5) (float tile_lon +. 0.5) in
      Lwt.catch
        (fun () ->
          let* tile = load_full_clc_tile path in
          (* Calculate tile range in degrees (for shader uniforms) *)
          let tile_range_lon = 65535. /. tile.header.scale_x in
          let tile_range_lat = 65535. /. tile.header.scale_y in
          tiles := (tile, tile_range_lon, tile_range_lat) :: !tiles;
          Lwt.return ())
        (fun _exn -> Lwt.return ())
      >>= fun () -> load_tiles tile_lat (tile_lon + 1)
    end
  in
  let* () = load_tiles min_tile_lat min_tile_lon in

  (* Reverse to get correct draw order (smaller features first) *)
  Lwt.return
    (dem_min_lon, dem_min_lat, dem_range_lon, dem_range_lat, List.rev !tiles)
