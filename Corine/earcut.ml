(* earcut.ml *)
[@@@warning "-32-27-69"]

type point = { x : float; y : float }

(* Linked list node for the polygon ring *)
type node = {
  mutable i : int; (* CHANGED: Made mutable to allow re-indexing *)
  p : point; (* Coordinate *)
  mutable prev : node;
  mutable next : node;
  mutable z : int; (* Z-order curve index *)
  mutable steiner : bool;
}

(* --- Helpers --- *)
let dummy_node =
  let rec n =
    {
      i = 0;
      p = { x = 0.; y = 0. };
      prev = n;
      next = n;
      z = 0;
      steiner = false;
    }
  in
  n

let create_node i p =
  let rec n = { i; p; prev = n; next = n; z = 0; steiner = false } in
  n

let insert_node i p last =
  let p_node = create_node i p in
  p_node.next <- last.next;
  p_node.prev <- last;
  last.next.prev <- p_node;
  last.next <- p_node;
  p_node

let remove_node p =
  p.next.prev <- p.prev;
  p.prev.next <- p.next;
  if p.prev == p then None else Some p.prev

(* Area of a triangle (a, b, c) *)
let area a b c = ((b.x -. a.x) *. (c.y -. a.y)) -. ((b.y -. a.y) *. (c.x -. a.x))

let point_in_triangle p a b c =
  let cp1 = ((c.x -. b.x) *. (p.y -. b.y)) -. ((c.y -. b.y) *. (p.x -. b.x)) in
  let cp2 = ((b.x -. a.x) *. (p.y -. a.y)) -. ((b.y -. a.y) *. (p.x -. a.x)) in
  let cp3 = ((a.x -. c.x) *. (p.y -. c.y)) -. ((a.y -. c.y) *. (p.x -. c.x)) in
  (cp1 >= 0. && cp2 >= 0. && cp3 >= 0.) || (cp1 <= 0. && cp2 <= 0. && cp3 <= 0.)

(* Check if a polygon vertex is an "Ear" (can be cut) *)
let is_ear ear =
  let a = ear.prev in
  let b = ear in
  let c = ear.next in

  if area a.p b.p c.p >= 0. then false (* Reflex, not convex *)
  else
    (* Check if any other points are inside this triangle *)
    let rec check p =
      if p == a || p == c then true (* Loop finished, no points found *)
      else if
        point_in_triangle p.p a.p b.p c.p
        && area a.p b.p p.p <= 0.
        && area b.p c.p p.p <= 0.
        && area c.p a.p p.p <= 0.
      then false (* Point inside, not an ear *)
      else check p.next
    in
    check c.next

(* Filter points: remove duplicate or collinear points *)
let filter_points start =
  let rec loop p =
    let again = ref false in
    (if p.p.x = p.next.p.x && p.p.y = p.next.p.y then
       (* Duplicate *)
       let _ = remove_node p in
       again := true
     else if area p.prev.p p.p p.next.p = 0. then
       (* Collinear *)
       let _ = remove_node p in
       again := true);

    if !again && p.next != p then loop p.next else p
  in
  if start.next == start then start else loop start

(* --- Main Ear Clipping Logic --- *)
let rec earcut_linked start triangles =
  if start.prev == start || start.prev == start.next then triangles
    (* < 3 vertices *)
  else
    let stop = start in
    let rec find_ear curr =
      if is_ear curr then
        (* Cut the ear *)
        let a = curr.prev in
        let c = curr.next in
        (* Add triangle indices: a, curr, c *)
        let new_tris = c.i :: curr.i :: a.i :: triangles in
        match remove_node curr with
        | None -> new_tris (* Should not happen if logic is correct *)
        | Some next_start -> earcut_linked next_start new_tris
      else if curr.next == stop then
        (* No ear found in whole loop? (Should be impossible for simple polygons) *)
        triangles
      else find_ear curr.next
    in
    find_ear start

(* --- Public API --- *)
let triangulate (rings : point list list) =
  match rings with
  | [] -> []
  | outer_pts :: holes_pts -> (
      if List.length outer_pts < 3 then []
      else
        let make_list pts offset =
          match pts with
          | [] -> None
          | hd :: tl ->
              let start = create_node offset hd in
              (* last is unused because the list structure is updated via side-effects in insert_node *)
              let _ =
                List.fold_left
                  (fun prev p -> insert_node (offset + 1) p prev)
                  start tl
              in

              (* Correct indices *)
              let rec fix_idx n i =
                n.i <- i;
                if n.next != start then fix_idx n.next (i + 1)
              in
              fix_idx start offset;
              Some start
        in

        match make_list outer_pts 0 with
        | None -> []
        | Some outer_node ->
            (* Note: Hole handling skipped for basic triangulation speed. 
             Only the outer ring is processed. *)
            earcut_linked (filter_points outer_node) [])
