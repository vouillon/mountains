(* triangulate_test.ml - Standalone triangulation test tool *)

let () =
  let usage = "Usage: " ^ Sys.argv.(0) ^ " <polygon.json> [--verbose]" in
  let filename = ref "" in
  let verbose = ref false in

  Arg.parse
    [
      ( "--verbose",
        Arg.Set verbose,
        "Enable verbose logging during triangulation" );
    ]
    (fun s -> filename := s)
    usage;

  if !filename = "" then (
    print_endline usage;
    exit 1);

  Printf.printf "Loading polygon from %s...\n%!" !filename;
  let tile, typ, expected, actual, outer, holes =
    Polygon_test_utils.load_polygon_json !filename
  in

  Printf.printf "Tile: %s, Type: %s\n" tile typ;
  Printf.printf "Expected Area: %g, Captured Actual Area: %g (ratio: %.4f)\n%!"
    expected actual
    (if expected > 0. then actual /. expected else 0.);

  let poly =
    {
      Geometry_types.outer = { start = 0; len = Array.length outer / 2 };
      holes =
        Array.mapi
          (fun i h ->
            let start =
              (Array.length outer / 2)
              + Array.fold_left
                  (fun acc arr -> acc + (Array.length arr / 2))
                  0 (Array.sub holes 0 i)
            in
            { Geometry_types.start; len = Array.length h / 2 })
          holes;
    }
  in

  let flat_verts = Array.concat (outer :: Array.to_list holes) in

  (* Run Cheap Validation *)
  Printf.printf "\n--- Cheap Validation ---\n";
  let cheap_errors = Polygon_validation.validate_cheap flat_verts poly in
  if cheap_errors = [] then print_endline "Passed."
  else
    List.iter
      (fun e ->
        Printf.printf "ERROR: %s\n" (Polygon_validation.string_of_error e))
      cheap_errors;

  (* Run Expensive Validation *)
  Printf.printf "\n--- Expensive Validation ---\n";
  let expensive_errors =
    Polygon_validation.validate_expensive flat_verts poly
  in
  if expensive_errors = [] then print_endline "Passed."
  else
    List.iter
      (fun e ->
        Printf.printf "ERROR: %s\n" (Polygon_validation.string_of_error e))
      expensive_errors;

  (* Run Triangulation *)
  Printf.printf "\n--- Triangulation ---\n";
  if !verbose then Polygon_triangulation.Triangulator.verbose := true;

  let start_time = Unix.gettimeofday () in
  let tris =
    Polygon_triangulation.Triangulator.triangulate_multi ~tile ~feature_type:typ
      flat_verts [| poly |]
  in
  let end_time = Unix.gettimeofday () in

  let num_tris = Array.length tris / 3 in
  let recomputed_actual = ref 0.0 in
  for i = 0 to num_tris - 1 do
    let i1 = tris.(i * 3) in
    let i2 = tris.((i * 3) + 1) in
    let i3 = tris.((i * 3) + 2) in
    let ax, ay = (flat_verts.(i1 * 2), flat_verts.((i1 * 2) + 1)) in
    let bx, by = (flat_verts.(i2 * 2), flat_verts.((i2 * 2) + 1)) in
    let cx, cy = (flat_verts.(i3 * 2), flat_verts.((i3 * 2) + 1)) in
    recomputed_actual :=
      !recomputed_actual
      +. abs_float
           (((ax *. (by -. cy)) +. (bx *. (cy -. ay)) +. (cx *. (ay -. by)))
           *. 0.5)
  done;

  Printf.printf "\nResults:\n";
  Printf.printf "Triangles: %d\n" num_tris;
  Printf.printf "Recomputed Area: %g\n" !recomputed_actual;
  Printf.printf "Ratio to expected: %.4f\n"
    (if expected > 0. then !recomputed_actual /. expected else 0.);
  Printf.printf "Time: %.4f seconds\n" (end_time -. start_time);

  if abs_float (!recomputed_actual -. expected) > 1e-7 *. expected then
    print_endline "\nFAILURE: Area mismatch persisted."
  else print_endline "\nSUCCESS: Area matches expected (within tolerance)."
