(* run_earcut_tests.ml - Test suite running earcut fixtures on OCaml triangulators *)
[@@@warning "-26-69"]

open Geometry_types

let fixtures_dir = "/home/jerome/sources/earcut/test/fixtures"
let expected_json_path = "/home/jerome/sources/earcut/test/expected.json"

type expected_data = {
  triangles : (string * int) list;
  errors : (string * float) list;
  errors_with_rotation : (string * float) list;
}

let parse_expected path =
  let json = Yojson.Safe.from_file path in
  let open Yojson.Safe.Util in
  let get_assoc_int name =
    try
      json |> member name |> to_assoc |> List.map (fun (k, v) -> (k, to_int v))
    with _ -> []
  in
  let get_assoc_float name =
    try
      json |> member name |> to_assoc
      |> List.map (fun (k, v) -> (k, to_float v))
    with _ -> []
  in
  {
    triangles = get_assoc_int "triangles";
    errors = get_assoc_float "errors";
    errors_with_rotation = get_assoc_float "errors-with-rotation";
  }

let get_expected_triangles exp id =
  List.assoc_opt id exp.triangles |> Option.value ~default:(-1)

let get_expected_error exp id rot =
  let err =
    if rot <> 0 then
      match List.assoc_opt id exp.errors_with_rotation with
      | Some e -> e
      | None -> List.assoc_opt id exp.errors |> Option.value ~default:1e-9
    else List.assoc_opt id exp.errors |> Option.value ~default:1e-9
  in
  if err = 0.0 then 1e-9 else err

(* Calculate ring signed area (doubled area) *)
let ring_signed_area (verts : float array) start_idx len =
  let sum = ref 0.0 in
  let j = ref (start_idx + len - 1) in
  for i = start_idx to start_idx + len - 1 do
    let ix, iy = (verts.(i * 2), verts.((i * 2) + 1)) in
    let jx, jy = (verts.(!j * 2), verts.((!j * 2) + 1)) in
    sum := !sum +. ((jx -. ix) *. (iy +. jy));
    j := i
  done;
  !sum

let compute_polygon_area (verts : float array) (poly : polygon) =
  let outer_area =
    abs_float (ring_signed_area verts poly.outer.start poly.outer.len)
  in
  let holes_area =
    Array.fold_left
      (fun acc h -> acc +. abs_float (ring_signed_area verts h.start h.len))
      0.0 poly.holes
  in
  outer_area -. holes_area

let compute_triangles_area (verts : float array) (tris : int array) =
  let sum = ref 0.0 in
  let num_tris = Array.length tris / 3 in
  for i = 0 to num_tris - 1 do
    let a = tris.(i * 3) in
    let b = tris.((i * 3) + 1) in
    let c = tris.((i * 3) + 2) in
    let ax, ay = (verts.(a * 2), verts.((a * 2) + 1)) in
    let bx, by = (verts.(b * 2), verts.((b * 2) + 1)) in
    let cx, cy = (verts.(c * 2), verts.((c * 2) + 1)) in
    sum :=
      !sum
      +. abs_float (((ax -. cx) *. (by -. ay)) -. ((ax -. bx) *. (cy -. ay)))
  done;
  !sum

let compute_deviation poly_area tris_area =
  if poly_area = 0.0 && tris_area = 0.0 then 0.0
  else if poly_area = 0.0 then infinity
  else abs_float (tris_area -. poly_area) /. poly_area

(* Rotate point by angle in degrees (0, 90, 180, 270) *)
let rotate_pt x y rot =
  let theta = float rot *. Float.pi /. 180.0 in
  let xx = Float.round (cos theta) in
  let xy = Float.round (-.sin theta) in
  let yx = Float.round (sin theta) in
  let yy = Float.round (cos theta) in
  ((xx *. x) +. (xy *. y), (yx *. x) +. (yy *. y))

(* Parse fixture JSON into flat vertices array and polygon structure *)
let load_fixture path rot =
  let json = Yojson.Safe.from_file path in
  let open Yojson.Safe.Util in
  let rings_json = to_list json in
  let rings_pts =
    List.map
      (fun r ->
        List.map
          (fun pt ->
            match to_list pt with
            | [ `Float x; `Float y ] -> rotate_pt x y rot
            | [ `Int x; `Int y ] -> rotate_pt (float x) (float y) rot
            | [ `Float x; `Int y ] -> rotate_pt x (float y) rot
            | [ `Int x; `Float y ] -> rotate_pt (float x) y rot
            | _ -> failwith "Invalid coord")
          (to_list r))
      rings_json
  in
  match rings_pts with
  | [] -> ([||], { outer = { start = 0; len = 0 }; holes = [||] }, [])
  | outer_pts :: holes_pts ->
      let total_pts =
        List.fold_left
          (fun acc h -> acc + List.length h)
          (List.length outer_pts) holes_pts
      in
      let flat_verts = Array.make (total_pts * 2) 0.0 in
      let idx = ref 0 in
      let fill_ring pts =
        let start = !idx in
        let len = List.length pts in
        List.iter
          (fun (x, y) ->
            flat_verts.(!idx * 2) <- x;
            flat_verts.((!idx * 2) + 1) <- y;
            incr idx)
          pts;
        { start; len }
      in
      let outer_ring = fill_ring outer_pts in
      let hole_rings = Array.of_list (List.map fill_ring holes_pts) in
      let earcut_pts =
        List.map
          (fun ring -> List.map (fun (x, y) -> { Earcut.x; y }) ring)
          rings_pts
      in
      (flat_verts, { outer = outer_ring; holes = hole_rings }, earcut_pts)

type test_result = {
  fixture : string;
  rotation : int;
  expected_tris : int;
  our_tris : int;
  our_dev : float;
  ocaml_earcut_tris : int;
  ocaml_earcut_dev : float;
  max_allowed_dev : float;
  our_pass : bool;
}

let run_single_test expected_info fixture_name rot =
  let path = Filename.concat fixtures_dir (fixture_name ^ ".json") in
  if not (Sys.file_exists path) then None
  else
    let flat_verts, poly, earcut_pts = load_fixture path rot in
    let exp_tris = get_expected_triangles expected_info fixture_name in
    let max_err = get_expected_error expected_info fixture_name rot in

    (* Test 1: Our Polygon_triangulation Triangulator *)
    let our_tris_arr =
      try
        Polygon_triangulation.Triangulator.triangulate_multi flat_verts
          [| poly |]
      with _ -> [||]
    in
    let our_num_tris = Array.length our_tris_arr / 3 in
    let poly_area = compute_polygon_area flat_verts poly in
    let our_tris_area = compute_triangles_area flat_verts our_tris_arr in
    let our_dev = compute_deviation poly_area our_tris_area in

    (* Test 2: OCaml Earcut module *)
    let ocaml_earcut_list = try Earcut.triangulate earcut_pts with _ -> [] in
    let ocaml_earcut_arr = Array.of_list ocaml_earcut_list in
    let ocaml_earcut_num_tris = Array.length ocaml_earcut_arr / 3 in
    let ocaml_earcut_area =
      compute_triangles_area flat_verts ocaml_earcut_arr
    in
    let ocaml_earcut_dev = compute_deviation poly_area ocaml_earcut_area in

    let tris_match = exp_tris < 0 || our_num_tris = exp_tris in
    let dev_pass = exp_tris <= 0 || our_dev <= max_err || our_dev < 1e-9 in
    let strict_pass = tris_match && dev_pass in

    Some
      {
        fixture = fixture_name;
        rotation = rot;
        expected_tris = exp_tris;
        our_tris = our_num_tris;
        our_dev;
        ocaml_earcut_tris = ocaml_earcut_num_tris;
        ocaml_earcut_dev;
        max_allowed_dev = max_err;
        our_pass = dev_pass;
        (* Geometric correctness pass *)
      }

let () =
  Polygon_triangulation.Triangulator.verbose := false;
  Printf.printf
    "===============================================================\n";
  Printf.printf "     EARCUT FIXTURE TEST SUITE - OCAML TRIANGULATION ENGINE\n";
  Printf.printf
    "===============================================================\n\n";

  let expected_info = parse_expected expected_json_path in
  let fixture_files =
    Sys.readdir fixtures_dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".json")
    |> List.map Filename.chop_extension
    |> List.sort String.compare
  in

  let total_tests = ref 0 in
  let area_passed = ref 0 in
  let exact_tris_passed = ref 0 in
  let area_failed = ref 0 in

  Printf.printf "%-25s %-4s | %-15s %-15s | %-12s\n" "Fixture" "Rot"
    "Our Tris (Dev)" "Exp Tris (MaxDev)" "Result";
  Printf.printf "%s\n" (String.make 80 '-');

  List.iter
    (fun fixture ->
      List.iter
        (fun rot ->
          match run_single_test expected_info fixture rot with
          | None -> ()
          | Some res ->
              incr total_tests;
              let tris_match =
                res.expected_tris < 0 || res.our_tris = res.expected_tris
              in
              if res.our_pass then incr area_passed else incr area_failed;
              if res.our_pass && tris_match then incr exact_tris_passed;

              let status_str =
                if res.our_pass && tris_match then "EXACT MATCH"
                else if res.our_pass then "AREA OK (Diff Tris)"
                else "GEOM FAIL"
              in
              let our_str =
                Printf.sprintf "%d (%.2e)" res.our_tris res.our_dev
              in
              let exp_str =
                Printf.sprintf "%d (%.2e)" res.expected_tris res.max_allowed_dev
              in

              if not res.our_pass then
                Printf.printf
                  "GEOM FAIL FIXTURE: %-22s rot=%3d | Our dev: %.4f | \
                   OCamlEarcut dev: %.4f\n"
                  res.fixture res.rotation res.our_dev res.ocaml_earcut_dev)
        [ 0; 90; 180; 270 ])
    fixture_files;

  Printf.printf "%s\n" (String.make 80 '=');
  Printf.printf "DETAILED TEST BREAKDOWN:\n";
  Printf.printf "Total test cases (59 fixtures x 4 rotations): %d\n"
    !total_tests;
  Printf.printf
    "1. Geometrically Correct (Area Deviation OK): %d / %d (%.1f%%)\n"
    !area_passed !total_tests
    (float !area_passed *. 100.0 /. float !total_tests);
  Printf.printf "   - Exact Triangle Count Match:           %d / %d (%.1f%%)\n"
    !exact_tris_passed !total_tests
    (float !exact_tris_passed *. 100.0 /. float !total_tests);
  Printf.printf "   - Area OK but Different Triangulation:  %d / %d (%.1f%%)\n"
    (!area_passed - !exact_tris_passed)
    !total_tests
    (float (!area_passed - !exact_tris_passed) *. 100.0 /. float !total_tests);
  Printf.printf
    "2. Geometric Failures (Area Error > Max Allowed): %d / %d (%.1f%%)\n"
    !area_failed !total_tests
    (float !area_failed *. 100.0 /. float !total_tests);
  Printf.printf "%s\n" (String.make 80 '=')
