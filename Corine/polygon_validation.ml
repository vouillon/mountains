(* polygon_validation.ml - Geometry validation for polygons *)

open Geometry_types

type error =
  | DuplicatePoint of int * (float * float)
  | SelfIntersection of (int * int) * (int * int)
  | HoleNotContained of int (* hole_index *)

let epsilon = 1e-12
let get_x (verts : float array) i = verts.(i * 2)
let get_y (verts : float array) i = verts.((i * 2) + 1)

let dist_sq x1 y1 x2 y2 =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  (dx *. dx) +. (dy *. dy)

let points_equal verts i1 i2 =
  let x1, y1 = (get_x verts i1, get_y verts i1) in
  let x2, y2 = (get_x verts i2, get_y verts i2) in
  dist_sq x1 y1 x2 y2 < epsilon *. epsilon

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

(* --- Expensive Checks O(n^2) --- *)

let cross_product verts ia ib ic =
  let ax, ay = (get_x verts ia, get_y verts ia) in
  let bx, by = (get_x verts ib, get_y verts ib) in
  let cx, cy = (get_x verts ic, get_y verts ic) in
  ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax))

let on_segment verts ia ib ip =
  let ax, ay = (get_x verts ia, get_y verts ia) in
  let bx, by = (get_x verts ib, get_y verts ib) in
  let px, py = (get_x verts ip, get_y verts ip) in
  abs_float (cross_product verts ia ib ip) < epsilon
  && px >= min ax bx -. epsilon
  && px <= max ax bx +. epsilon
  && py >= min ay by -. epsilon
  && py <= max ay by +. epsilon

let segments_intersect verts p1 p2 a b =
  let cp1 = cross_product verts p1 p2 a in
  let cp2 = cross_product verts p1 p2 b in
  let cp3 = cross_product verts a b p1 in
  let cp4 = cross_product verts a b p2 in

  (* Standard intersection check (strict) *)
  if
    ((cp1 > epsilon && cp2 < -.epsilon) || (cp1 < -.epsilon && cp2 > epsilon))
    && ((cp3 > epsilon && cp4 < -.epsilon) || (cp3 < -.epsilon && cp4 > epsilon))
  then true
  else
    (* Special cases: endpoint lies on other segment (excluding shared endpoints) *)
    let p1_on_ab =
      if not (points_equal verts p1 a || points_equal verts p1 b) then
        on_segment verts a b p1
      else false
    in
    let p2_on_ab =
      if not (points_equal verts p2 a || points_equal verts p2 b) then
        on_segment verts a b p2
      else false
    in
    let a_on_p1p2 =
      if not (points_equal verts a p1 || points_equal verts a p2) then
        on_segment verts p1 p2 a
      else false
    in
    let b_on_p1p2 =
      if not (points_equal verts b p1 || points_equal verts b p2) then
        on_segment verts p1 p2 b
      else false
    in
    p1_on_ab || p2_on_ab || a_on_p1p2 || b_on_p1p2

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

let check_self_intersection_indices (verts : float array) indices =
  let len = Array.length indices in
  let errors = ref [] in
  for i = 0 to len - 1 do
    for j = i + 2 to len - 1 do
      if i = 0 && j = len - 1 then ()
      else
        let i1 = indices.(i) in
        let i2 = indices.((i + 1) mod len) in
        let j1 = indices.(j) in
        let j2 = indices.((j + 1) mod len) in
        if segments_intersect verts i1 i2 j1 j2 then
          errors :=
            SelfIntersection
              ( (indices.(i), indices.((i + 1) mod len)),
                (indices.(j), indices.((j + 1) mod len)) )
            :: !errors
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
  let e2 =
    Array.to_list
      (Array.map (fun h -> check_no_duplicates verts h.start h.len) poly.holes)
    |> List.flatten
  in
  e1 @ e2

let validate_expensive (verts : float array) poly =
  let e1 = check_self_intersection verts poly.outer.start poly.outer.len in
  let e2 =
    check_hole_containment verts poly.outer.start poly.outer.len poly.holes
  in
  let e3 =
    Array.to_list
      (Array.map
         (fun h -> check_self_intersection verts h.start h.len)
         poly.holes)
    |> List.flatten
  in
  (* Could add hole-hole intersection check here *)
  e1 @ e2 @ e3

let string_of_error = function
  | DuplicatePoint (i, (x, y)) ->
      Printf.sprintf "Duplicate point at index %d (%f, %f)" i x y
  | SelfIntersection ((i1, i2), (j1, j2)) ->
      Printf.sprintf "Self-intersection between edge %d-%d and %d-%d" i1 i2 j1
        j2
  | HoleNotContained h_idx ->
      Printf.sprintf "Hole %d is not contained within outer ring" h_idx
