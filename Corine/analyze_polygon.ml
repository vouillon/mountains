open Yojson.Safe.Util
open Geometry_types

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <polygon.json>\n" Sys.argv.(0);
    exit 1);
  let json = Yojson.Safe.from_file Sys.argv.(1) in
  let outer_coords = json |> member "outer" |> to_list in
  let holes = json |> member "holes" |> to_list in

  (* Convert to flat array and Geometry_types structure *)
  let total_verts =
    List.length outer_coords
    + List.fold_left (fun acc h -> acc + List.length (to_list h)) 0 holes
  in
  let verts = Array.make (total_verts * 2) 0.0 in

  let next_v = ref 0 in
  let fill_ring ring_coords =
    let start = !next_v in
    List.iter
      (fun pt ->
        match pt with
        | `List [ `Float x; `Float y ] ->
            verts.(!next_v * 2) <- x;
            verts.((!next_v * 2) + 1) <- y;
            incr next_v
        | _ -> ())
      ring_coords;
    let len = !next_v - start in
    { start; len }
  in

  let outer_ring = fill_ring outer_coords in
  let hole_rings =
    List.map (fun h -> fill_ring (to_list h)) holes |> Array.of_list
  in
  let poly = { outer = outer_ring; holes = hole_rings } in

  Printf.printf "--- Polygon Analysis: %s ---\n" Sys.argv.(1);
  Printf.printf "Outer ring: start=%d len=%d\n" outer_ring.start outer_ring.len;
  Printf.printf "Number of holes: %d\n" (Array.length hole_rings);

  Printf.printf "\n--- Validation Analysis ---\n";
  let cheap_errs = Polygon_validation.validate_cheap verts poly in
  if cheap_errs = [] then Printf.printf "Cheap checks: PASS\n"
  else
    List.iter
      (fun e ->
        Printf.printf "Cheap Error: %s\n" (Polygon_validation.string_of_error e))
      cheap_errs;

  let expensive_errs = Polygon_validation.validate_expensive verts poly in
  if expensive_errs = [] then Printf.printf "Expensive checks: PASS\n"
  else
    List.iter
      (fun e ->
        Printf.printf "Expensive Error: %s\n"
          (Polygon_validation.string_of_error e))
      expensive_errs;

  Printf.printf "\n--- Area Analysis ---\n";
  let total_area = Polygon_triangulation.Triangulator.polygon_area verts poly in
  let expected_area =
    try json |> member "expected_area" |> to_float with _ -> 0.0
  in

  Printf.printf "Calculated Area: %.10f\n" total_area;
  Printf.printf "Expected Area:   %.10f\n" expected_area;
  if expected_area <> 0.0 then
    Printf.printf "Ratio:           %.6f\n" (total_area /. expected_area);
  ()
