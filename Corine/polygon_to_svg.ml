(* polygon_to_svg.ml - Convert polygon JSON to SVG for debugging *)

let generate_svg filename output_file =
  let tile, typ, expected, actual, outer, holes =
    Polygon_test_utils.load_polygon_json filename
  in

  Printf.printf "Generating SVG for %s (tile %s, type %s)\n%!" filename tile typ;

  (* Helper to find bounds *)
  let min_x = ref infinity in
  let min_y = ref infinity in
  let max_x = ref (-.infinity) in
  let max_y = ref (-.infinity) in

  let update_bounds ring =
    for i = 0 to (Array.length ring / 2) - 1 do
      let x, y = (ring.(i * 2), ring.((i * 2) + 1)) in
      if x < !min_x then min_x := x;
      if y < !min_y then min_y := y;
      if x > !max_x then max_x := x;
      if y > !max_y then max_y := y
    done
  in

  update_bounds outer;
  Array.iter update_bounds holes;

  let width = !max_x -. !min_x in
  let height = !max_y -. !min_y in
  let margin = max width height *. 0.05 in

  let view_min_x = !min_x -. margin in
  let view_min_y = !min_y -. margin in
  let view_width = width +. (2.0 *. margin) in
  let view_height = height +. (2.0 *. margin) in

  let oc = open_out output_file in
  Printf.fprintf oc
    "<svg viewBox=\"%f %f %f %f\" xmlns=\"http://www.w3.org/2000/svg\">\n"
    view_min_x view_min_y view_width view_height;

  (* Style *)
  Printf.fprintf oc
    "<style>\n\
    \  .outer { fill: #add8e6; fill-opacity: 0.3; stroke: blue; stroke-width: \
     %f; }\n"
    (margin *. 0.1);
  Printf.fprintf oc
    "  .hole { fill: white; fill-opacity: 0.8; stroke: red; stroke-width: %f; }\n"
    (margin *. 0.1);
  Printf.fprintf oc
    "  .tri { fill: none; stroke: green; stroke-width: %f; opacity: 0.5; }\n"
    (margin *. 0.05);
  Printf.fprintf oc "</style>\n";

  (* Draw Outer *)
  let points_to_string ring =
    let pts = ref [] in
    for i = 0 to (Array.length ring / 2) - 1 do
      pts := Printf.sprintf "%f,%f" ring.(i * 2) ring.((i * 2) + 1) :: !pts
    done;
    String.concat " " (List.rev !pts)
  in

  Printf.fprintf oc "<polygon class=\"outer\" points=\"%s\" />\n"
    (points_to_string outer);

  (* Draw Holes *)
  Array.iter
    (fun h ->
      Printf.fprintf oc "<polygon class=\"hole\" points=\"%s\" />\n"
        (points_to_string h))
    holes;

  (* Triangulate and show triangles *)
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
    Polygon_triangulation.Triangulator.triangulate_multi flat_verts [| poly |]
  in
  for i = 0 to (Array.length tris / 3) - 1 do
    let i1 = tris.(i * 3) in
    let i2 = tris.((i * 3) + 1) in
    let i3 = tris.((i * 3) + 2) in
    Printf.fprintf oc "<polygon class=\"tri\" points=\"%f,%f %f,%f %f,%f\" />\n"
      flat_verts.(i1 * 2)
      flat_verts.((i1 * 2) + 1)
      flat_verts.(i2 * 2)
      flat_verts.((i2 * 2) + 1)
      flat_verts.(i3 * 2)
      flat_verts.((i3 * 2) + 1)
  done;

  Printf.fprintf oc "</svg>\n";
  close_out oc;
  Printf.printf "SVG written to %s\n%!" output_file

let () =
  if Array.length Sys.argv < 3 then
    Printf.printf "Usage: %s <polygon.json> <output.svg>\n" Sys.argv.(0)
  else generate_svg Sys.argv.(1) Sys.argv.(2)
