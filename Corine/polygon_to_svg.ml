(* polygon_to_svg.ml - Convert polygon JSON to SVG for debugging *)

let generate_svg filename output_file use_normalized =
  let tile, typ, expected, actual, outer, holes =
    Polygon_test_utils.load_polygon_json filename
  in

  Printf.printf "Generating SVG for %s (tile %s, type %s, normalized=%b)\n%!"
    filename tile typ use_normalized;

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

  (* Clear bridges and stalled loops from any previous run and triangulate *)
  Polygon_triangulation.Triangulator.clear_bridges ();
  Polygon_triangulation.Triangulator.clear_stalled_loops ();
  let tris =
    Polygon_triangulation.Triangulator.triangulate_multi flat_verts [| poly |]
  in

  let draw_verts =
    if use_normalized then
      Polygon_triangulation.Triangulator.get_last_normalized_coords ()
    else flat_verts
  in

  (* Calculate bounds from the coordinates we will actually DRAW *)
  let min_x = ref infinity in
  let min_y = ref infinity in
  let max_x = ref (-.infinity) in
  let max_y = ref (-.infinity) in

  for i = 0 to (Array.length draw_verts / 2) - 1 do
    let x, y = (draw_verts.(i * 2), draw_verts.((i * 2) + 1)) in
    if x < !min_x then min_x := x;
    if y < !min_y then min_y := y;
    if x > !max_x then max_x := x;
    if y > !max_y then max_y := y
  done;

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
  Printf.fprintf oc
    "  .bridge { stroke: magenta; stroke-width: %f; stroke-dasharray: %f; }\n"
    (margin *. 0.15) (margin *. 0.1);
  Printf.fprintf oc
    "  .stalled { fill: none; stroke: red; stroke-width: %f; stroke-dasharray: \
     %f; opacity: 0.8; }\n"
    (margin *. 0.2) (margin *. 0.15);
  Printf.fprintf oc "</style>\n";

  (* Draw Outer *)
  let points_to_string ring =
    let pts = ref [] in
    for i = 0 to (Array.length ring / 2) - 1 do
      pts := Printf.sprintf "%f,%f" ring.(i * 2) ring.((i * 2) + 1) :: !pts
    done;
    String.concat " " (List.rev !pts)
  in

  (* Helper to render a ring from vertex indices *)
  let ring_from_indices indices =
    let arr = Array.make (List.length indices * 2) 0.0 in
    List.iteri
      (fun i v_idx ->
        arr.(i * 2) <- draw_verts.(v_idx * 2);
        arr.((i * 2) + 1) <- draw_verts.((v_idx * 2) + 1))
      indices;
    arr
  in

  Printf.fprintf oc "<polygon class=\"outer\" points=\"%s\" />\n"
    (points_to_string
       (ring_from_indices (List.init (Array.length outer / 2) (fun i -> i))));

  (* Draw Holes *)
  let hole_offset = ref (Array.length outer / 2) in
  Array.iter
    (fun h ->
      let len = Array.length h / 2 in
      let indices = List.init len (fun i -> !hole_offset + i) in
      Printf.fprintf oc "<polygon class=\"hole\" points=\"%s\" />\n"
        (points_to_string (ring_from_indices indices));
      hole_offset := !hole_offset + len)
    holes;

  (* Draw triangles *)
  for i = 0 to (Array.length tris / 3) - 1 do
    let i1 = tris.(i * 3)
    and i2 = tris.((i * 3) + 1)
    and i3 = tris.((i * 3) + 2) in
    Printf.fprintf oc "<polygon class=\"tri\" points=\"%f,%f %f,%f %f,%f\" />\n"
      draw_verts.(i1 * 2)
      draw_verts.((i1 * 2) + 1)
      draw_verts.(i2 * 2)
      draw_verts.((i2 * 2) + 1)
      draw_verts.(i3 * 2)
      draw_verts.((i3 * 2) + 1)
  done;

  (* Draw bridges *)
  let bridges = Polygon_triangulation.Triangulator.get_bridges () in
  Printf.printf "Found %d bridges\n%!" (List.length bridges);
  List.iter
    (fun (hole_idx, outer_idx) ->
      let hx = draw_verts.(hole_idx * 2) in
      let hy = draw_verts.((hole_idx * 2) + 1) in
      let ox = draw_verts.(outer_idx * 2) in
      let oy = draw_verts.((outer_idx * 2) + 1) in
      Printf.fprintf oc
        "<line class=\"bridge\" x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" />\n" hx
        hy ox oy)
    bridges;

  (* Draw stalled loops *)
  let stalled = Polygon_triangulation.Triangulator.get_stalled_loops () in
  Printf.printf "Found %d stalled loops\n%!" (List.length stalled);
  List.iter
    (fun indices ->
      Printf.fprintf oc "<polygon class=\"stalled\" points=\"%s\" />\n"
        (points_to_string (ring_from_indices indices)))
    stalled;

  Printf.fprintf oc "</svg>\n";
  close_out oc;
  Printf.printf "SVG written to %s\n%!" output_file

let () =
  let normalized = ref false in
  let usage =
    "Usage: " ^ Sys.argv.(0) ^ " <polygon.json> <output.svg> [--normalized]"
  in

  let pos_args = ref [] in
  Arg.parse
    [
      ("--normalized", Arg.Set normalized, "Use normalized coordinates for SVG");
    ]
    (fun s -> pos_args := s :: !pos_args)
    usage;

  match List.rev !pos_args with
  | [ f; o ] -> generate_svg f o !normalized
  | _ ->
      Printf.printf "%s\n" usage;
      exit 1
