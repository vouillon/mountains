(* triangulation_tests.ml - Regression test suite for triangulation *)

let () =
  let files = Polygon_test_utils.list_test_polygons () in
  let total = List.length files in
  Printf.printf "Running %d regression tests...\n" total;

  let passed = ref 0 in
  let failed = ref 0 in

  List.iter
    (fun filename ->
      let tile, typ, expected, _, outer, holes =
        Polygon_test_utils.load_polygon_json filename
      in

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

      let tris =
        Polygon_triangulation.Triangulator.triangulate_multi ~feature_type:typ
          flat_verts [| poly |]
      in

      let num_tris = Array.length tris / 3 in
      let actual_area = ref 0.0 in
      for i = 0 to num_tris - 1 do
        let i1 = tris.(i * 3) in
        let i2 = tris.((i * 3) + 1) in
        let i3 = tris.((i * 3) + 2) in
        let ax, ay = (flat_verts.(i1 * 2), flat_verts.((i1 * 2) + 1)) in
        let bx, by = (flat_verts.(i2 * 2), flat_verts.((i2 * 2) + 1)) in
        let cx, cy = (flat_verts.(i3 * 2), flat_verts.((i3 * 2) + 1)) in
        actual_area :=
          !actual_area
          +. abs_float
               (((ax *. (by -. cy)) +. (bx *. (cy -. ay)) +. (cx *. (ay -. by)))
               *. 0.5)
      done;

      let mismatch = abs_float (!actual_area -. expected) in
      if
        (* ~ 1 per one thousand *)
        mismatch <= 1e-3 *. expected
        ||
        (* ~ 0.1 m^2 *)
        mismatch < 1e-11
      then (
        prerr_endline filename;
        incr passed)
      else (
        incr failed;
        Printf.printf "FAIL: %s (expected=%g, actual=%g, ratio=%.4f)\n" filename
          expected !actual_area
          (if expected > 0. then !actual_area /. expected else 0.)))
    files;

  Printf.printf "\nTest Results:\n";
  Printf.printf "Total: %d\n" total;
  Printf.printf "Passed: %d\n" !passed;
  Printf.printf "Failed: %d\n" !failed;

  if !failed > 0 then (
    print_endline "Regression suite FAILED.";
    exit 1)
  else (
    print_endline "Regression suite PASSED.";
    exit 0)
