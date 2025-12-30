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
    high_x : Buffer.t;
    low_x : Buffer.t;
    high_y : Buffer.t;
    low_y : Buffer.t;
    indices : Buffer.t;
  }

  let create () =
    {
      high_x = Buffer.create 1024;
      low_x = Buffer.create 1024;
      high_y = Buffer.create 1024;
      low_y = Buffer.create 1024;
      indices = Buffer.create 1024;
    }

  let write_u8 buf v = Buffer.add_char buf (Char.chr (v land 0xFF))

  let write_u16 buf v =
    write_u8 buf (v land 0xFF);
    write_u8 buf ((v lsr 8) land 0xFF)

  let encode_vertices t (verts : float array) num_verts (min_x, min_y)
      (scale_x, scale_y) =
    let count = ref 0 in
    for i = 0 to num_verts - 1 do
      let x = verts.(i * 2) in
      let y = verts.((i * 2) + 1) in

      (* Quantize *)
      let qx = int_of_float ((x -. min_x) *. scale_x) in
      let qy = int_of_float ((y -. min_y) *. scale_y) in

      (* Clamp to u16 range just in case *)
      let qx = max 0 (min 65535 qx) in
      let qy = max 0 (min 65535 qy) in

      (* Split Bytes (Absolute) *)
      (* We write High/Low of the absolute encoded value *)
      write_u8 t.low_x (qx land 0xFF);
      write_u8 t.high_x ((qx lsr 8) land 0xFF);

      write_u8 t.low_y (qy land 0xFF);
      write_u8 t.high_y ((qy lsr 8) land 0xFF);

      incr count
    done;
    !count

  let encode_indices t indices count =
    (* Ensure we only write 'count' indices *)
    for i = 0 to count - 1 do
      write_u16 t.indices indices.(i)
    done
end

(* --- Main Pipeline --- *)

let process_tile db_path output_dir tile_name =
  let lat, lon = parse_tile_string tile_name in
  Printf.printf "Processing Tile: %s (Lat: %.2f, Lon: %.2f)\n%!" tile_name lat
    lon;

  (* Define Bounds with Margin *)
  let margin_deg = 1.0 /. 3600.0 in
  (* 1 pixel margin *)
  let min_lat = lat -. margin_deg in
  let max_lat = lat +. 1.0 +. margin_deg in
  let min_lon = lon -. margin_deg in
  let max_lon = lon +. 1.0 +. margin_deg in

  (* Calculate Scale Factors for 16-bit Quantization *)
  let range_x = max_lon -. min_lon in
  let range_y = max_lat -. min_lat in
  (* Map range to 0..65535 *)
  let scale_x = 65535.0 /. range_x in
  let scale_y = 65535.0 /. range_y in

  (* Open DB *)
  let db = Sqlite3.db_open db_path in

  let laea_pts =
    [
      Proj_3035.wgs84_to_laea min_lon min_lat;
      Proj_3035.wgs84_to_laea max_lon min_lat;
      Proj_3035.wgs84_to_laea max_lon max_lat;
      Proj_3035.wgs84_to_laea min_lon max_lat;
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

  (* Query *)
  let sql =
    Printf.sprintf
      "SELECT c.Shape, c.Code_18 \n\
      \       FROM CLC2018_CLC2018_V2018_20 c \n\
      \       JOIN rtree_CLC2018_CLC2018_V2018_20_Shape r ON c.OBJECTID = r.id \n\
      \       WHERE r.maxx >= %f AND r.minx <= %f AND r.maxy >= %f AND r.miny \
       <= %f"
      min_laea_x max_laea_x min_laea_y max_laea_y
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

  let output_file_path = Filename.concat output_dir (tile_name ^ ".clc") in
  let out_ch = open_out_bin output_file_path in

  (* Write Header Placeholder (Magic + Count + Bounds) *)
  output_string out_ch "CLC1";
  output_binary_int out_ch 0;

  (* Count placeholder *)

  (* Write float params (bounds + scale) for reconstruction *)
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

  let rec process_rows () =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
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
                                     let lon, lat =
                                       Proj_3035.laea_to_wgs84 p.x p.y
                                     in
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
                                                Proj_3035.laea_to_wgs84 p.x p.y
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

                      (* 2. Flatten for Clipping *)
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
                                  start = 0;
                                  len = Array.length outer_verts / 2;
                                };
                              holes = proper_holes;
                            }
                          in

                          (* 3. Clip *)
                          match
                            Polygon_clipping.Clipper.clip_polygon flat_verts
                              proper_poly clipper_region
                          with
                          | None -> ()
                          | Some (clipped_verts, clipped_poly) ->
                              (* 4. Triangulate *)
                              let tris =
                                try
                                  Polygon_triangulation.Triangulator
                                  .triangulate_multi clipped_verts
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

                                (* Encode *)
                                incr entry_count;
                                (* Code: u16 *)
                                output_byte out_ch (code land 0xFF);
                                output_byte out_ch ((code lsr 8) land 0xFF);

                                (* Store raw data in buffers *)
                                let v_count = !next_idx in
                                let i_count = Array.length final_indices in

                                (* ALIGNMENT FIX: Ensure we only write 3*N indices *)
                                let tri_count_val = i_count / 3 in
                                let valid_i_count = tri_count_val * 3 in

                                if !entry_count <= 10 then
                                  Printf.printf
                                    "Writing Feature %d: Code %d, Tris %d, \
                                     Verts %d\n\
                                     %!"
                                    !entry_count code tri_count_val v_count;

                                (* Write num_triangles (u32) *)
                                output_byte out_ch (tri_count_val land 0xFF);
                                output_byte out_ch
                                  ((tri_count_val lsr 8) land 0xFF);
                                output_byte out_ch
                                  ((tri_count_val lsr 16) land 0xFF);
                                output_byte out_ch
                                  ((tri_count_val lsr 24) land 0xFF);

                                (* Encode payload *)
                                Buffer.clear encoder.high_x;
                                Buffer.clear encoder.low_x;
                                Buffer.clear encoder.high_y;
                                Buffer.clear encoder.low_y;
                                Buffer.clear encoder.indices;

                                let _ =
                                  Encoder.encode_vertices encoder final_verts
                                    v_count (min_lon, min_lat) (scale_x, scale_y)
                                in
                                let _ =
                                  Encoder.encode_indices encoder final_indices
                                    valid_i_count
                                in

                                (* Write num_vertices (u32) *)
                                output_byte out_ch (v_count land 0xFF);
                                output_byte out_ch ((v_count lsr 8) land 0xFF);
                                output_byte out_ch ((v_count lsr 16) land 0xFF);
                                output_byte out_ch ((v_count lsr 24) land 0xFF);

                                (* Write Streams *)
                                Buffer.output_buffer out_ch encoder.high_x;
                                Buffer.output_buffer out_ch encoder.low_x;
                                Buffer.output_buffer out_ch encoder.high_y;
                                Buffer.output_buffer out_ch encoder.low_y;
                                Buffer.output_buffer out_ch encoder.indices))
                        float_polys
                | None -> ()
              end
            | None -> ()
            end
        | _ -> ()
        end;
        process_rows ()
    | Sqlite3.Rc.DONE -> ()
    | _ -> failwith "DB Error"
  in

  process_rows ();

  (* Backpatch count *)
  seek_out out_ch 4;
  output_binary_int out_ch !entry_count;
  close_out out_ch;
  ignore (Sqlite3.db_close db);
  Printf.printf "Done. Extracted %d features.\n%!" !entry_count

let () =
  if Array.length Sys.argv < 4 then
    Printf.printf "Usage: %s <DB> <Output Dir> <TileCode> (e.g. N45E006)\n"
      Sys.argv.(0)
  else process_tile Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
