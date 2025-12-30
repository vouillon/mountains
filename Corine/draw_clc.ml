(* draw_clc.ml *)
(* Compile command: 
   ocamlfind ocamlopt -package sqlite3,str -linkpkg wkb_decode.ml draw_clc.ml -o draw_clc 
*)

type dataset = { code : string; geoms : Wkb_decode.geometry list }

(* Reuse existing header parser just to strip it *)
let get_wkb_from_gpkg_blob blob_str =
  let len = String.length blob_str in
  if len < 8 || String.sub blob_str 0 2 <> "GP" then None
  else
    let flags = Char.code blob_str.[3] in
    let envelope_indicator = (flags lsr 1) land 0x07 in
    let header_len =
      match envelope_indicator with 1 -> 40 | 2 | 3 -> 56 | 4 -> 72 | _ -> 8
    in
    if len < header_len then None
    else Some (String.sub blob_str header_len (len - header_len))

(* --- SVG Helpers --- *)
let svg_header width height =
  Printf.sprintf
    "<?xml version=\"1.0\" standalone=\"no\"?>\n\
     <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \
     \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n\
     <svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\" \
     xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n\
     <rect width=\"100%%\" height=\"100%%\" fill=\"#f0f0f0\"/>\n"
    width height width height

let svg_footer = "</svg>"

(* Official CORINE Land Cover Color Palette *)
let get_color code =
  match code with
  (* 1. Artificial Surfaces (Red/Purple) *)
  | "111" -> "#e6004d" (* Continuous urban fabric *)
  | "112" -> "#ff0000" (* Discontinuous urban fabric *)
  | "121" -> "#cc4df2" (* Industrial or commercial units *)
  | "122" -> "#cc0000" (* Road and rail networks *)
  | "123" -> "#e6cce6" (* Port areas *)
  | "124" -> "#e6cce6" (* Airports *)
  | "131" -> "#a600cc" (* Mineral extraction sites *)
  | "132" -> "#a64d00" (* Dump sites *)
  | "133" -> "#ff4dff" (* Construction sites *)
  | "141" -> "#ffa6ff" (* Green urban areas *)
  | "142" -> "#ffe6ff" (* Sport and leisure facilities *)
  (* 2. Agricultural Areas (Yellow/Orange) *)
  | "211" -> "#ffffa8" (* Non-irrigated arable land *)
  | "212" -> "#ffff00" (* Permanently irrigated land *)
  | "213" -> "#e6e600" (* Rice fields *)
  | "221" -> "#e68000" (* Vineyards *)
  | "222" -> "#f2a64d" (* Fruit trees and berry plantations *)
  | "223" -> "#e6a600" (* Olive groves *)
  | "231" -> "#e6e64d" (* Pastures *)
  | "241" -> "#ffe6a6" (* Annual crops associated with permanent crops *)
  | "242" -> "#ffe64d" (* Complex cultivation patterns *)
  | "243" -> "#e6cc4d" (* Land principally occupied by agriculture + natural *)
  | "244" -> "#f2cca6" (* Agro-forestry areas *)
  (* 3. Forest and Semi-natural Areas (Green) *)
  | "311" -> "#80ff00" (* Broad-leaved forest *)
  | "312" -> "#00a600" (* Coniferous forest *)
  | "313" -> "#4dff00" (* Mixed forest *)
  | "321" -> "#ccf24d" (* Natural grasslands *)
  | "322" -> "#a6ff80" (* Moors and heathland *)
  | "323" -> "#a6e64d" (* Sclerophyllous vegetation *)
  | "324" -> "#a6f200" (* Transitional woodland-shrub *)
  | "331" -> "#e6e6e6" (* Beaches, dunes, sands *)
  | "332" -> "#cccccc" (* Bare rocks *)
  | "333" -> "#ccffcc" (* Sparsely vegetated areas *)
  | "334" -> "#000000" (* Burnt areas *)
  | "335" -> "#a6e6cc" (* Glaciers and perpetual snow *)
  (* 4. Wetlands (Purple/Blue) *)
  | "411" -> "#a6a6ff" (* Inland marshes *)
  | "412" -> "#4d4dff" (* Peat bogs *)
  | "421" -> "#ccccff" (* Salt marshes *)
  | "422" -> "#e6e6ff" (* Salines *)
  | "423" -> "#a6a6e6" (* Intertidal flats *)
  (* 5. Water Bodies (Blue) *)
  | "511" -> "#00ccf2" (* Water courses *)
  | "512" -> "#80f2e6" (* Water bodies *)
  | "521" -> "#00ffa6" (* Coastal lagoons *)
  | "522" -> "#a6ffe6" (* Estuaries *)
  | "523" -> "#e6f2ff" (* Sea and ocean *)
  (* Fallbacks for broader categories if 3-digit code is missing *)
  | s when String.length s > 0 && s.[0] = '1' ->
      "#ff0000" (* Artificial default *)
  | s when String.length s > 0 && s.[0] = '2' ->
      "#ffff00" (* Agriculture default *)
  | s when String.length s > 0 && s.[0] = '3' -> "#00cc00" (* Forest default *)
  | s when String.length s > 0 && s.[0] = '4' -> "#a6a6ff" (* Wetland default *)
  | s when String.length s > 0 && s.[0] = '5' -> "#00ccf2" (* Water default *)
  | _ -> "#a6a6a6" (* Unknown Gray *)

(* --- Coordinate Transformation (Fixed Aspect Ratio) --- *)
let transform_point p bbox width height =
  let range_x = bbox.Wkb_decode.max_x -. bbox.Wkb_decode.min_x in
  let range_y = bbox.Wkb_decode.max_y -. bbox.Wkb_decode.min_y in

  let scale_x = float_of_int width /. range_x in
  let scale_y = float_of_int height /. range_y in
  let scale = min scale_x scale_y *. 0.95 in

  let map_pixel_width = range_x *. scale in
  let map_pixel_height = range_y *. scale in
  let offset_x = (float_of_int width -. map_pixel_width) /. 2.0 in
  let offset_y = (float_of_int height -. map_pixel_height) /. 2.0 in

  let px = offset_x +. ((p.Wkb_decode.x -. bbox.Wkb_decode.min_x) *. scale) in
  let py = offset_y +. ((bbox.Wkb_decode.max_y -. p.Wkb_decode.y) *. scale) in
  (px, py)

(* Convert ring to SVG Path command (M ... L ... Z) *)
let ring_to_svg_path_d pts bbox width height =
  match pts with
  | [] -> ""
  | start_pt :: rest ->
      let sx, sy = transform_point start_pt bbox width height in
      let start_cmd = Printf.sprintf "M %.1f,%.1f" sx sy in
      let line_cmds =
        List.map
          (fun p ->
            let tx, ty = transform_point p bbox width height in
            Printf.sprintf "L %.1f,%.1f" tx ty)
          rest
      in
      String.concat " " (start_cmd :: line_cmds) ^ " Z"

(* Approximate Area for Sorting (Width * Height of BBox) *)
let get_approx_area geom =
  let bbox = Wkb_decode.get_bbox geom in
  let w = bbox.Wkb_decode.max_x -. bbox.Wkb_decode.min_x in
  let h = bbox.Wkb_decode.max_y -. bbox.Wkb_decode.min_y in
  w *. h

(* --- Main Processing --- *)
let draw_clc db_path target_code output_file =
  Printf.printf "Opening DB: %s\n%!" db_path;
  let db = Sqlite3.db_open db_path in

  try
    (* 1. Find Table and Columns *)
    let sql_geom =
      "SELECT table_name, column_name FROM gpkg_geometry_columns LIMIT 1"
    in
    let stmt = Sqlite3.prepare db sql_geom in
    let table_name, geom_col =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let t = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 0) in
          let c = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 1) in
          (t, c)
      | _ -> failwith "No geometry table found"
    in
    ignore (Sqlite3.finalize stmt);

    (* 2. Get Global BBox *)
    let sql_bbox =
      Printf.sprintf
        "SELECT min_x, min_y, max_x, max_y FROM gpkg_contents WHERE table_name \
         = '%s'"
        table_name
    in
    let stmt_bbox = Sqlite3.prepare db sql_bbox in
    let global_bbox =
      match Sqlite3.step stmt_bbox with
      | Sqlite3.Rc.ROW ->
          let min_x = Sqlite3.Data.to_float_exn (Sqlite3.column stmt_bbox 0) in
          let min_y = Sqlite3.Data.to_float_exn (Sqlite3.column stmt_bbox 1) in
          let max_x = Sqlite3.Data.to_float_exn (Sqlite3.column stmt_bbox 2) in
          let max_y = Sqlite3.Data.to_float_exn (Sqlite3.column stmt_bbox 3) in
          { Wkb_decode.min_x; min_y; max_x; max_y }
      | _ -> failwith "Could not find bounding box"
    in
    ignore (Sqlite3.finalize stmt_bbox);

    (* 3. Find Code Column *)
    let sql_cols = Printf.sprintf "PRAGMA table_info(%s)" table_name in
    let stmt_cols = Sqlite3.prepare db sql_cols in
    let rec collect_cols acc =
      match Sqlite3.step stmt_cols with
      | Sqlite3.Rc.ROW ->
          collect_cols
            (Sqlite3.Data.to_string_exn (Sqlite3.column stmt_cols 1) :: acc)
      | _ -> List.rev acc
    in
    let col_names = collect_cols [] in
    ignore (Sqlite3.finalize stmt_cols);
    let code_col =
      match
        List.find_opt
          (fun c ->
            try
              ignore
                (Str.search_forward (Str.regexp_string_case_fold "Code") c 0);
              true
            with _ -> false)
          col_names
      with
      | Some c -> c
      | None -> List.hd col_names
    in

    (* 4. Query Data *)
    let sql_query =
      match target_code with
      | Some c ->
          Printf.sprintf "SELECT %s, %s FROM %s WHERE %s = '%s'" code_col
            geom_col table_name code_col c
      (* Limit removed to fetch all features *)
      | None ->
          Printf.sprintf "SELECT %s, %s FROM %s" code_col geom_col table_name
    in

    Printf.printf "Fetching features...\n%!";
    let stmt_query = Sqlite3.prepare db sql_query in

    let rec fetch_all acc =
      match Sqlite3.step stmt_query with
      | Sqlite3.Rc.ROW -> (
          let code_val =
            Sqlite3.Data.to_string_exn (Sqlite3.column stmt_query 0)
          in
          let geom_blob = Sqlite3.column stmt_query 1 in
          match geom_blob with
          | Sqlite3.Data.BLOB b -> (
              match get_wkb_from_gpkg_blob b with
              | Some wkb_bytes -> (
                  match Wkb_decode.decode_wkb wkb_bytes with
                  | Some geom ->
                      let area = get_approx_area geom in
                      fetch_all ((area, code_val, geom) :: acc)
                  | None -> fetch_all acc)
              | None -> fetch_all acc)
          | _ -> fetch_all acc)
      | _ -> acc
    in

    let all_features = fetch_all [] in
    ignore (Sqlite3.finalize stmt_query);
    Printf.printf "Fetched %d features. Sorting by size...\n%!"
      (List.length all_features);

    (* 5. SORT: Largest Area First *)
    let sorted_features =
      List.sort (fun (a1, _, _) (a2, _, _) -> compare a2 a1) all_features
    in

    (* 6. Generate SVG *)
    let width = 1000 in
    let height = 1000 in
    let oc = open_out output_file in
    Printf.fprintf oc "%s" (svg_header width height);

    List.iter
      (fun (_, code, geom) ->
        let color = get_color code in

        (* Helper to draw a list of rings as a single path with holes *)
        let draw_rings rings =
          let d_str =
            String.concat " "
              (List.map
                 (fun r -> ring_to_svg_path_d r global_bbox width height)
                 rings)
          in
          (* fill-rule="evenodd" is MAGIC: it handles holes automatically *)
          if d_str <> "" then
            Printf.fprintf oc
              "<path d=\"%s\" fill=\"%s\" stroke=\"none\" fill-opacity=\"0.7\" \
               fill-rule=\"evenodd\"/>\n"
              d_str color
        in

        match geom with
        | Wkb_decode.Polygon rings -> draw_rings rings
        | Wkb_decode.MultiPolygon polys ->
            (assert false : unit);
            List.iter draw_rings polys
        | _ -> ())
      sorted_features;

    Printf.fprintf oc "%s" svg_footer;
    close_out oc;
    Printf.printf "Done. Output: %s\n%!" output_file
  with
  | Sqlite3.Error e -> Printf.printf "SQLite Error: %s\n%!" e
  | exn ->
      Printf.printf "Error: %s\n%!" (Printexc.to_string exn);
      ignore (Sqlite3.db_close db)

let () =
  (* Use None to extract everything, or (Some "111") to test *)
  draw_clc "clc2018-R94.gpkg" None "map_output.svg"
