(* extract_clc_decoded.ml *)
(* Compile command: 
   ocamlfind ocamlopt -package sqlite3 -linkpkg wkb_decode.ml extract_clc_decoded.ml -o extract_clc_decoded 
*)

(* --- Reuse the GPKG Header Parser --- *)
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

let extract_clc_data db_path target_code =
  Printf.printf "Opening database: %s\n%!" db_path;
  let db = Sqlite3.db_open db_path in

  try
    let sql_geom =
      "SELECT table_name, column_name FROM gpkg_geometry_columns LIMIT 1"
    in
    let stmt = Sqlite3.prepare db sql_geom in
    let table_info =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let t = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 0) in
          let c = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 1) in
          ignore (Sqlite3.finalize stmt);
          Some (t, c)
      | _ ->
          ignore (Sqlite3.finalize stmt);
          None
    in

    match table_info with
    | None -> Printf.printf "No geometry tables found.\n%!"
    | Some (table_name, geom_col) ->
        let sql_cols = Printf.sprintf "PRAGMA table_info(%s)" table_name in
        let stmt_cols = Sqlite3.prepare db sql_cols in
        let rec collect_cols acc =
          match Sqlite3.step stmt_cols with
          | Sqlite3.Rc.ROW ->
              let n = Sqlite3.Data.to_string_exn (Sqlite3.column stmt_cols 1) in
              collect_cols (n :: acc)
          | _ -> List.rev acc
        in
        let col_names = collect_cols [] in
        ignore (Sqlite3.finalize stmt_cols);

        let code_col =
          match
            List.find_opt
              (fun c ->
                let sub s =
                  try
                    ignore
                      (Str.search_forward (Str.regexp_string_case_fold s) c 0);
                    true
                  with _ -> false
                in
                sub "Code" || sub "CLC")
              col_names
          with
          | Some c -> c
          | None -> List.hd col_names
        in

        let sql_query =
          match target_code with
          | Some c ->
              Printf.sprintf "SELECT %s, %s FROM %s WHERE %s = '%s'" code_col
                geom_col table_name code_col c
          | None ->
              Printf.sprintf "SELECT %s, %s FROM %s" code_col geom_col
                table_name
        in

        Printf.printf "Executing: %s\n%!" sql_query;
        let stmt_query = Sqlite3.prepare db sql_query in

        let rec process_rows index =
          match Sqlite3.step stmt_query with
          | Sqlite3.Rc.ROW ->
              let clc_val =
                Sqlite3.Data.to_string_exn (Sqlite3.column stmt_query 0)
              in
              let geom_data = Sqlite3.column stmt_query 1 in

              let output_str =
                match geom_data with
                | Sqlite3.Data.BLOB b -> (
                    match get_wkb_from_gpkg_blob b with
                    | Some raw_wkb -> (
                        (* DECODE USING THE NEW MODULE *)
                        match Wkb_decode.decode_wkb raw_wkb with
                        | Some geom -> Wkb_decode.to_string geom
                        | None -> "Failed to decode WKB")
                    | None -> "Invalid GPKG Blob")
                | _ -> "No BLOB data"
              in

              (* Truncate long geometry strings for display *)
              let display_geom =
                if String.length output_str > 80 then
                  String.sub output_str 0 80 ^ "..."
                else output_str
              in

              Printf.printf "Feature %d | Code: %s | %s\n%!" (index + 1) clc_val
                display_geom;
              process_rows (index + 1)
          | Sqlite3.Rc.DONE -> ()
          | err -> Printf.printf "Step error: %s\n%!" (Sqlite3.Rc.to_string err)
        in

        process_rows 0;
        ignore (Sqlite3.finalize stmt_query)
  with
  | Sqlite3.Error msg -> Printf.printf "SQLite Error: %s\n%!" msg
  | exn ->
      Printf.printf "Error: %s\n%!" (Printexc.to_string exn);

      ignore (Sqlite3.db_close db)

let () =
  extract_clc_data
    (if Array.length Sys.argv > 1 then Sys.argv.(1) else "clc2018-R94.gpkg")
    None
