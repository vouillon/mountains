(* polygon_test_utils.ml - Utilities for saving/loading polygons for testing *)

(* JSON format:
   {
     "tile": "N46E006",
     "type": "water",
     "expected_area": 0.00123,
     "actual_area": 0.00456,
     "outer": [[x1,y1], [x2,y2], ...],
     "holes": [[[x1,y1], ...], ...]
   }
*)

let test_polygon_dir = "test_polygons"

(* Ensure the test directory exists *)
let ensure_dir_exists () =
  if not (Sys.file_exists test_polygon_dir) then
    try Unix.mkdir test_polygon_dir 0o755 with _ -> ()

(* Convert ring to JSON array of [x,y] pairs *)
let ring_to_json (ring : float array) : Yojson.Safe.t =
  let n = Array.length ring / 2 in
  let coords =
    List.init n (fun i ->
        `List [ `Float ring.(i * 2); `Float ring.((i * 2) + 1) ])
  in
  `List coords

(* Convert JSON array of [x,y] pairs to ring *)
let json_to_ring (json : Yojson.Safe.t) : float array =
  let open Yojson.Safe.Util in
  let coords = to_list json in
  let pairs =
    List.map
      (fun c ->
        match to_list c with
        | [ `Float x; `Float y ] -> [ x; y ]
        | _ -> failwith "Invalid coordinate pair")
      coords
  in
  Array.of_list (List.flatten pairs)

(* Generate unique filename for a mismatch *)
let mismatch_counter = ref 0

let generate_filename ~tile ~feature_type =
  ensure_dir_exists ();
  incr mismatch_counter;
  Printf.sprintf "%s/mismatch_%s_%s_%03d.json" test_polygon_dir tile
    feature_type !mismatch_counter

(* Save polygon to JSON file *)
let save_polygon_json ~tile ~feature_type ~expected_area ~actual_area
    ~(outer : float array) ~(holes : float array array)
    ?(validation_errors = []) () =
  let filename = generate_filename ~tile ~feature_type in
  let holes_json = `List (Array.to_list (Array.map ring_to_json holes)) in
  let errors_json = `List (List.map (fun s -> `String s) validation_errors) in
  let json =
    `Assoc
      [
        ("tile", `String tile);
        ("type", `String feature_type);
        ("expected_area", `Float expected_area);
        ("actual_area", `Float actual_area);
        ( "ratio",
          `Float
            (if expected_area > 0. then actual_area /. expected_area else 0.) );
        ("outer", ring_to_json outer);
        ("holes", holes_json);
        ("validation_errors", errors_json);
      ]
  in
  let oc = open_out filename in
  Yojson.Safe.pretty_to_channel oc json;
  close_out oc;
  Printf.printf "Saved failing polygon to %s\n%!" filename;
  filename

(* Load polygon from JSON file *)
let load_polygon_json filename =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_file filename in
  let tile = json |> member "tile" |> to_string in
  let feature_type = json |> member "type" |> to_string in
  let expected_area = json |> member "expected_area" |> to_float in
  let actual_area = json |> member "actual_area" |> to_float in
  let outer = json |> member "outer" |> json_to_ring in
  let holes_json = json |> member "holes" |> to_list in
  let holes = Array.of_list (List.map json_to_ring holes_json) in
  (tile, feature_type, expected_area, actual_area, outer, holes)

(* List all test polygon files *)
let list_test_polygons () =
  ensure_dir_exists ();
  if Sys.file_exists test_polygon_dir && Sys.is_directory test_polygon_dir then
    let files = Sys.readdir test_polygon_dir in
    Array.to_list files
    |> List.filter (fun f -> Filename.check_suffix f ".json")
    |> List.map (fun f -> Filename.concat test_polygon_dir f)
    |> List.sort String.compare
  else []
