(* polygon_validation.ml - Geometry validation for polygons *)

open Geometry_types

type validation_error =
  | DuplicatePoint of int * (float * float)
  | DegenerateEdge of int * (float * float)
  | WrongWindingOrder of bool (* is_outer *)
  | SelfIntersection of (int * int) * (int * int)
  | HoleNotContained of int (* hole_index *)
  | HolesIntersect of int * int

let epsilon = 1e-9
let get_x (verts : float array) i = verts.(i * 2)
let get_y (verts : float array) i = verts.((i * 2) + 1)

let dist_sq x1 y1 x2 y2 =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  (dx *. dx) +. (dy *. dy)

(* --- Cheap Checks O(n) --- *)

let check_no_duplicates (verts : float array) start len =
  let errors = ref [] in
  for i = 0 to len - 1 do
    let i1 = start + i in
    let i2 = start + ((i + 1) mod len) in
    let x1, y1 = (get_x verts i1, get_y verts i1) in
    let x2, y2 = (get_x verts i2, get_y verts i2) in
    if dist_sq x1 y1 x2 y2 < epsilon *. epsilon then
      errors := DuplicatePoint (i, (x1, y1)) :: !errors
  done;
  !errors

let check_winding_order (verts : float array) start len is_outer =
  if len < 3 then []
  else
    let ref_x = get_x verts start in
    let ref_y = get_y verts start in
    let acc = ref 0.0 in
    for i = 0 to len - 1 do
      let i1 = start + i in
      let i2 = start + ((i + 1) mod len) in
      let p1x, p1y = (get_x verts i1 -. ref_x, get_y verts i1 -. ref_y) in
      let p2x, p2y = (get_x verts i2 -. ref_x, get_y verts i2 -. ref_y) in
      acc := !acc +. ((p1x *. p2y) -. (p2x *. p1y))
    done;
    let area = !acc *. 0.5 in
    if is_outer then if area < 0.0 then [ WrongWindingOrder true ] else []
    else if area > 0.0 then [ WrongWindingOrder false ]
    else []

(* --- Expensive Checks O(n^2) --- *)

let cross_product verts ia ib ic =
  let ax, ay = (get_x verts ia, get_y verts ia) in
  let bx, by = (get_x verts ib, get_y verts ib) in
  let cx, cy = (get_x verts ic, get_y verts ic) in
  ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax))

let segments_intersect verts i1 i2 i3 i4 =
  let cp1 = cross_product verts i1 i2 i3 in
  let cp2 = cross_product verts i1 i2 i4 in
  let cp3 = cross_product verts i3 i4 i1 in
  let cp4 = cross_product verts i3 i4 i2 in

  (* Standard intersection check *)
  if
    ((cp1 > epsilon && cp2 < -.epsilon) || (cp1 < -.epsilon && cp2 > epsilon))
    && ((cp3 > epsilon && cp4 < -.epsilon) || (cp3 < -.epsilon && cp4 > epsilon))
  then true
  else
    (* Special cases for endpoints on segments could be added here if needed,
       but for triangulation debugging, literal intersection is the main enemy. *)
    false

let check_self_intersection (verts : float array) start len =
  let errors = ref [] in
  for i = 0 to len - 1 do
    for j = i + 2 to len - 1 do
      (* Avoid adjacent edges which share an endpoint *)
      if i = 0 && j = len - 1 then ()
      else
        let i1 = start + i in
        let i2 = start + ((i + 1) mod len) in
        let j1 = start + j in
        let j2 = start + ((j + 1) mod len) in
        if segments_intersect verts i1 i2 j1 j2 then
          errors := SelfIntersection ((i, i2), (j, j2)) :: !errors
    done
  done;
  !errors

(* Point in ring test for containment checks.
   Returns true if point is strictly inside OR on the boundary. *)
let point_in_ring (verts : float array) start len px py =
  let inside = ref false in
  let on_edge = ref false in
  let j = ref (len - 1) in
  for i = 0 to len - 1 do
    let ix, iy = (get_x verts (start + i), get_y verts (start + i)) in
    let jx, jy = (get_x verts (start + !j), get_y verts (start + !j)) in

    (* Check if point is on edge i-j *)
    let dx1 = px -. ix in
    let dy1 = py -. iy in
    let dx2 = jx -. ix in
    let dy2 = jy -. iy in
    let cross = (dx1 *. dy2) -. (dx2 *. dy1) in
    (if abs_float cross < 1e-12 then
       let dot = (dx1 *. dx2) +. (dy1 *. dy2) in
       let seg_len_sq = (dx2 *. dx2) +. (dy2 *. dy2) in
       if dot >= -.epsilon && dot <= seg_len_sq +. epsilon then on_edge := true);

    if
      iy > py <> (jy > py)
      && px < ((jx -. ix) *. (py -. iy) /. (jy -. iy)) +. ix
    then inside := not !inside;
    j := i
  done;
  !on_edge || !inside

let check_hole_containment (verts : float array) outer_start outer_len holes =
  let errors = ref [] in
  Array.iteri
    (fun h_idx h ->
      (* Check if at least one point of the hole is inside the outer ring.
       Properly, ALL points should be, but one is a good start for speed. *)
      let px, py = (get_x verts h.start, get_y verts h.start) in
      if not (point_in_ring verts outer_start outer_len px py) then
        errors := HoleNotContained h_idx :: !errors)
    holes;
  !errors

(* Validation entry points *)

let validate_cheap (verts : float array) poly =
  let e1 = check_no_duplicates verts poly.outer.start poly.outer.len in
  let e2 = check_winding_order verts poly.outer.start poly.outer.len true in
  let e3 =
    Array.to_list
      (Array.mapi
         (fun i h ->
           check_no_duplicates verts h.start h.len
           @ check_winding_order verts h.start h.len false)
         poly.holes)
    |> List.flatten
  in
  e1 @ e2 @ e3

let validate_expensive (verts : float array) poly =
  let e1 = check_self_intersection verts poly.outer.start poly.outer.len in
  let e2 =
    check_hole_containment verts poly.outer.start poly.outer.len poly.holes
  in
  (* Could add hole-hole intersection check here *)
  e1 @ e2

let string_of_error = function
  | DuplicatePoint (i, (x, y)) ->
      Printf.sprintf "Duplicate point at index %d (%f, %f)" i x y
  | DegenerateEdge (i, (x, y)) ->
      Printf.sprintf "Degenerate edge at index %d (%f, %f)" i x y
  | WrongWindingOrder is_outer ->
      Printf.sprintf
        "Non-standard winding order (%s ring %s) - triangulator will normalize \
         this"
        (if is_outer then "Outer" else "Hole")
        (if is_outer then "is CW" else "is CCW")
  | SelfIntersection ((i1, i2), (j1, j2)) ->
      Printf.sprintf "Self-intersection between edge %d-%d and %d-%d" i1 i2 j1
        j2
  | HoleNotContained h_idx ->
      Printf.sprintf "Hole %d is not contained within outer ring" h_idx
  | HolesIntersect (h1, h2) -> Printf.sprintf "Holes %d and %d intersect" h1 h2
