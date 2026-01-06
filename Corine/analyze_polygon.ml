(* analyze_polygon.ml - Diagnostic tool for polygon geometry *)

open Yojson.Safe.Util

type ring = {
  name : string;
  min_x : float;
  max_x : float;
  min_y : float;
  max_y : float;
  p0_x : float;
  p0_y : float;
  count : int;
}

let get_bbox name coords =
  let min_x = ref infinity in
  let max_x = ref neg_infinity in
  let min_y = ref infinity in
  let max_y = ref neg_infinity in
  let count = List.length coords in
  List.iter
    (fun pt ->
      match pt with
      | `List [ `Float x; `Float y ] ->
          min_x := min !min_x x;
          max_x := max !max_x x;
          min_y := min !min_y y;
          max_y := max !max_y y
      | _ -> ())
    coords;
  let p0_x, p0_y =
    match List.hd coords with
    | `List [ `Float x; `Float y ] -> (x, y)
    | _ -> (0.0, 0.0)
  in
  {
    name;
    min_x = !min_x;
    max_x = !max_x;
    min_y = !min_y;
    max_y = !max_y;
    p0_x;
    p0_y;
    count;
  }

let print_ring r =
  Printf.printf
    "%s: count=%d, BB=[%.6f, %.6f] x [%.6f, %.6f], P0=(%.6f, %.6f)\n" r.name
    r.count r.min_x r.max_x r.min_y r.max_y r.p0_x r.p0_y

let contains outer inner =
  inner.min_x >= outer.min_x && inner.max_x <= outer.max_x
  && inner.min_y >= outer.min_y && inner.max_y <= outer.max_y

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <polygon.json>\n" Sys.argv.(0);
    exit 1);
  let file = Sys.argv.(1) in
  let json = Yojson.Safe.from_file file in
  let outer_coords = json |> member "outer" |> to_list in
  let holes = json |> member "holes" |> to_list in

  let outer_r = get_bbox "Outer" outer_coords in
  print_ring outer_r;

  List.iteri
    (fun i h ->
      let name = Printf.sprintf "Hole %d" i in
      let coords = to_list h in
      let r = get_bbox name coords in
      let on_boundary =
        abs_float (r.min_x -. outer_r.min_x) < 1e-9
        || abs_float (r.max_x -. outer_r.max_x) < 1e-9
        || abs_float (r.min_y -. outer_r.min_y) < 1e-9
        || abs_float (r.max_y -. outer_r.max_y) < 1e-9
      in
      if i = 63 || i = 41 || i = 20 || on_boundary then begin
        print_ring r;
        if not (contains outer_r r) then
          Printf.printf "  WARNING: BB not contained in Outer BB!\n";
        if on_boundary then
          Printf.printf "  INFO: Hole touches Outer BBox boundary.\n"
      end)
    holes
