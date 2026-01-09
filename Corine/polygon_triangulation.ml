(** Fast Polygon Triangulation with Holes in OCaml Algorithm: Ear Clipping with
    Spatial Hashing (and Brute-force fallback) *)

open Geometry_types

(* Types removed - using Geometry_types *)

module Geometry = struct
  let epsilon = 1e-12

  let get_x (verts : float array) i = Array.unsafe_get verts (i * 2)
  [@@inline always]

  let get_y (verts : float array) i = Array.unsafe_get verts ((i * 2) + 1)
  [@@inline always]

  let normalize_vertices (verts : float array) =
    let len = Array.length verts / 2 in
    if len = 0 then (verts, 1.0)
    else
      let min_x = ref infinity and min_y = ref infinity in
      let max_x = ref neg_infinity and max_y = ref neg_infinity in

      for i = 0 to len - 1 do
        let x = get_x verts i in
        let y = get_y verts i in
        if x < !min_x then min_x := x;
        if y < !min_y then min_y := y;
        if x > !max_x then max_x := x;
        if y > !max_y then max_y := y
      done;

      let cx = (!min_x +. !max_x) *. 0.5 in
      let cy = (!min_y +. !max_y) *. 0.5 in
      let width = !max_x -. !min_x in
      let height = !max_y -. !min_y in
      let scale = 2.0 /. max width height in
      (* Safe divide if width/height=0? max handles it unless both 0 *)
      let scale = if scale = infinity then 1.0 else scale in

      let norm_verts = Array.copy verts in
      for i = 0 to len - 1 do
        let x = get_x verts i in
        let y = get_y verts i in
        norm_verts.(i * 2) <- (x -. cx) *. scale;
        norm_verts.((i * 2) + 1) <- (y -. cy) *. scale
      done;
      (norm_verts, scale)

  let fmax (a : float) (b : float) = if a > b then a else b [@@inline always]
  let fmin (a : float) (b : float) = if a < b then a else b [@@inline always]

  let cross_product (verts : float array) ia ib ic =
    let ax, ay = (get_x verts ia, get_y verts ia) in
    let bx, by = (get_x verts ib, get_y verts ib) in
    let cx, cy = (get_x verts ic, get_y verts ic) in
    ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax))
  [@@inline always]

  let cross_product_2d ax ay bx by = (ax *. by) -. (ay *. bx) [@@inline always]

  let point_in_triangle (verts : float array) ip ia ib ic =
    let cp1 = cross_product verts ia ib ip in
    let cp2 = cross_product verts ib ic ip in
    let cp3 = cross_product verts ic ia ip in
    (* Inclusive of boundary to catch all possible intrusions *)
    (cp1 >= -.epsilon && cp2 >= -.epsilon && cp3 >= -.epsilon)
    || (cp1 <= epsilon && cp2 <= epsilon && cp3 <= epsilon)
  [@@inline always]

  let points_equal (verts : float array) i1 i2 =
    let x1, y1 = (get_x verts i1, get_y verts i1) in
    let x2, y2 = (get_x verts i2, get_y verts i2) in
    abs_float (x1 -. x2) < epsilon && abs_float (y1 -. y2) < epsilon
  [@@inline always]

  let dist_sq_point_segment x y ax ay bx by =
    let dx = bx -. ax in
    let dy = by -. ay in
    if dx = 0.0 && dy = 0.0 then
      let pdx = x -. ax in
      let pdy = y -. ay in
      (pdx *. pdx) +. (pdy *. pdy)
    else
      let t =
        (((x -. ax) *. dx) +. ((y -. ay) *. dy)) /. ((dx *. dx) +. (dy *. dy))
      in
      let t_clamped = fmax 0.0 (fmin 1.0 t) in
      let cx = ax +. (t_clamped *. dx) in
      let cy = ay +. (t_clamped *. dy) in
      let pdx = x -. cx in
      let pdy = y -. cy in
      (pdx *. pdx) +. (pdy *. pdy)
  [@@inline always]

  let dist_sq (verts : float array) ia ib =
    let ax, ay = (get_x verts ia, get_y verts ia) in
    let bx, by = (get_x verts ib, get_y verts ib) in
    let dx = ax -. bx in
    let dy = ay -. by in
    (dx *. dx) +. (dy *. dy)
  [@@inline always]

  let segments_overlap (verts : float array) p1 p2 a b =
    let cp1 = cross_product verts p1 p2 a in
    let cp2 = cross_product verts p1 p2 b in
    let cp3 = cross_product verts a b p1 in
    let cp4 = cross_product verts a b p2 in

    (* Standard intersection check (strict) *)
    if
      ((cp1 > epsilon && cp2 < -.epsilon) || (cp1 < -.epsilon && cp2 > epsilon))
      && ((cp3 > epsilon && cp4 < -.epsilon)
         || (cp3 < -.epsilon && cp4 > epsilon))
    then true
    else
      (* Special cases: endpoint lies on other segment (excluding shared endpoints) *)
      let p1_on_ab =
        if not (points_equal verts p1 a || points_equal verts p1 b) then
          let ax, ay = (get_x verts a, get_y verts a) in
          let bx, by = (get_x verts b, get_y verts b) in
          let px, py = (get_x verts p1, get_y verts p1) in
          abs_float (cross_product verts a b p1) < epsilon
          && px >= fmin ax bx -. epsilon
          && px <= fmax ax bx +. epsilon
          && py >= fmin ay by -. epsilon
          && py <= fmax ay by +. epsilon
        else false
      in
      let p2_on_ab =
        if not (points_equal verts p2 a || points_equal verts p2 b) then
          let ax, ay = (get_x verts a, get_y verts a) in
          let bx, by = (get_x verts b, get_y verts b) in
          let px, py = (get_x verts p2, get_y verts p2) in
          abs_float (cross_product verts a b p2) < epsilon
          && px >= fmin ax bx -. epsilon
          && px <= fmax ax bx +. epsilon
          && py >= fmin ay by -. epsilon
          && py <= fmax ay by +. epsilon
        else false
      in
      let a_on_p1p2 =
        if not (points_equal verts a p1 || points_equal verts a p2) then
          let ax, ay = (get_x verts p1, get_y verts p1) in
          let bx, by = (get_x verts p2, get_y verts p2) in
          let px, py = (get_x verts a, get_y verts a) in
          abs_float (cross_product verts p1 p2 a) < epsilon
          && px >= fmin ax bx -. epsilon
          && px <= fmax ax bx +. epsilon
          && py >= fmin ay by -. epsilon
          && py <= fmax ay by +. epsilon
        else false
      in
      let b_on_p1p2 =
        if not (points_equal verts b p1 || points_equal verts b p2) then
          let ax, ay = (get_x verts p1, get_y verts p1) in
          let bx, by = (get_x verts p2, get_y verts p2) in
          let px, py = (get_x verts b, get_y verts b) in
          abs_float (cross_product verts p1 p2 b) < epsilon
          && px >= fmin ax bx -. epsilon
          && px <= fmax ax bx +. epsilon
          && py >= fmin ay by -. epsilon
          && py <= fmax ay by +. epsilon
        else false
      in
      p1_on_ab || p2_on_ab || a_on_p1p2 || b_on_p1p2
  [@@inline always]

  let segments_cross (verts : float array) p1 p2 a b =
    let cp1 = cross_product verts p1 p2 a in
    let cp2 = cross_product verts p1 p2 b in
    let cp3 = cross_product verts a b p1 in
    let cp4 = cross_product verts a b p2 in

    (* Strict intersection check only *)
    ((cp1 > epsilon && cp2 < -.epsilon) || (cp1 < -.epsilon && cp2 > epsilon))
    && ((cp3 > epsilon && cp4 < -.epsilon) || (cp3 < -.epsilon && cp4 > epsilon))
  [@@inline always]

  let is_in_cone (verts : float array) ia ib ic ip =
    let cp_abc = cross_product verts ia ib ic in
    if cp_abc >= -.epsilon then
      (* Convex or flat: must be left of BOTH edges to be interior *)
      cross_product verts ia ib ip >= -.epsilon
      && cross_product verts ib ic ip >= -.epsilon
    else
      (* Reflex: must NOT be strictly in the exterior sector (Right of both rays) *)
      not
        (cross_product verts ia ib ip < -.epsilon
        && cross_product verts ib ic ip < -.epsilon)
  [@@inline always]

  let signed_area_range (verts : float array) start len =
    if len < 3 then 0.0
    else
      (* Robust area calculation using relative coordinates to avoid precision loss *)
      let ref_x = get_x verts start in
      let ref_y = get_y verts start in
      let acc = ref 0.0 in
      for i = 0 to len - 1 do
        let i1 = start + i in
        let i2 = if i = len - 1 then start else start + i + 1 in
        (* Shift to local coordinates relative to first vertex *)
        let p1x, p1y = (get_x verts i1 -. ref_x, get_y verts i1 -. ref_y) in
        let p2x, p2y = (get_x verts i2 -. ref_x, get_y verts i2 -. ref_y) in
        (* Standard Shoelace term: x1*y2 - x2*y1 *)
        acc := !acc +. ((p1x *. p2y) -. (p2x *. p1y))
      done;
      !acc *. 0.5

  let is_ccw_range (verts : float array) start len =
    (* Counter-Clockwise polygon has POSITIVE area with standard Shoelace *)
    signed_area_range verts start len > 0.0

  let find_rightmost_idx_range (verts : float array) start len =
    if len = 0 then start
    else
      let max_idx = ref start in
      let max_x = ref (get_x verts start) in
      for i = 1 to len - 1 do
        let idx = start + i in
        let x = get_x verts idx in
        if x > !max_x then (
          max_x := x;
          max_idx := idx)
      done;
      !max_idx
end

module PolygonList = struct
  type t = {
    next : int array;
    prev : int array;
    vert_idx : int array;
    mutable next_node : int;
  }

  let create total_verts num_holes =
    let capacity = total_verts + (num_holes * 2) + 4 in
    {
      next = Array.make capacity 0;
      prev = Array.make capacity 0;
      vert_idx = Array.make capacity 0;
      next_node = 0;
    }

  let init_ring t start_vert len is_ccw =
    let start_node = t.next_node in
    for i = 0 to len - 1 do
      let node = start_node + i in
      let v_idx =
        if is_ccw then start_vert + i else start_vert + (len - 1 - i)
      in
      t.vert_idx.(node) <- v_idx;

      let prev_node = if i = 0 then start_node + len - 1 else node - 1 in
      let next_node = if i = len - 1 then start_node else node + 1 in
      t.prev.(node) <- prev_node;
      t.next.(node) <- next_node
    done;
    t.next_node <- t.next_node + len;
    start_node

  let duplicate_node t ref_node =
    let node = t.next_node in
    t.vert_idx.(node) <- t.vert_idx.(ref_node);
    t.next_node <- t.next_node + 1;
    node

  (* Helper to count active nodes in circular list *)
  let count_nodes t start_node =
    let rec loop curr n =
      if n > 0 && curr = start_node then n else loop t.next.(curr) (n + 1)
    in
    (* Safety for degenerate lists *)
    if t.next.(start_node) = start_node then 1 else loop t.next.(start_node) 1

  (* Filter collinear and duplicate nodes *)
  let filter_points (verts : float array) t start_node =
    let curr = ref start_node in
    let end_node = ref start_node in
    let again = ref true in

    while !again || !curr != !end_node do
      again := false;
      (* Check if valid ring (>=3) before simplifying *)
      let i = !curr in
      let prev_i = t.prev.(i) in
      let next_i = t.next.(i) in

      if prev_i != i && next_i != i then
        (* At least 3 nodes *)
        let p1 = t.vert_idx.(prev_i) in
        let p2 = t.vert_idx.(i) in

        let is_dup = Geometry.points_equal verts p1 p2 in

        if is_dup then (
          t.prev.(next_i) <- prev_i;
          t.next.(prev_i) <- next_i;
          (* FIXED: Do NOT decrement t.count allocator cursor *)
          if !curr = !end_node then end_node := prev_i;
          curr := prev_i;
          again := true)
        else curr := next_i
      else again := false (* Stop if degenerate *)
    done;
    !curr
end

module SpatialIndex = struct
  type t = {
    grid_head : int array;
    next_in_bucket : int array;
    inv_cell_size : float;
    min_x : float;
    min_y : float;
    width : int;
    height : int;
  }

  let empty_index =
    {
      grid_head = [||];
      next_in_bucket = [||];
      inv_cell_size = 0.0;
      min_x = 0.0;
      min_y = 0.0;
      width = 0;
      height = 0;
    }

  let create nodes_to_index count n_total (verts : float array) vert_idx_map =
    if count = 0 then empty_index
    else begin
      let max_float = infinity in
      let min_float = neg_infinity in

      let min_x = ref max_float and min_y = ref max_float in
      let max_x = ref min_float and max_y = ref min_float in

      for i = 0 to count - 1 do
        let node = nodes_to_index.(i) in
        let v_idx = vert_idx_map.(node) in
        let x = Geometry.get_x verts v_idx in
        let y = Geometry.get_y verts v_idx in
        if x < !min_x then min_x := x;
        if y < !min_y then min_y := y;
        if x > !max_x then max_x := x;
        if y > !max_y then max_y := y
      done;

      let range_x = !max_x -. !min_x in
      let range_y = !max_y -. !min_y in
      let cell_size =
        Geometry.fmax range_x range_y /. sqrt (float_of_int n_total)
      in
      let cell_size = if cell_size < Geometry.epsilon then 1.0 else cell_size in
      let inv_cell_size = 1.0 /. cell_size in

      let width = int_of_float (range_x *. inv_cell_size) + 1 in
      let height = int_of_float (range_y *. inv_cell_size) + 1 in

      let grid_head = Array.make (width * height) (-1) in
      let next_in_bucket = Array.make n_total (-1) in

      for i = 0 to count - 1 do
        let item = nodes_to_index.(i) in
        let v_idx = vert_idx_map.(item) in
        let x = Geometry.get_x verts v_idx in
        let y = Geometry.get_y verts v_idx in

        let cx = int_of_float ((x -. !min_x) *. inv_cell_size) in
        let cy = int_of_float ((y -. !min_y) *. inv_cell_size) in

        let cx = if cx >= width then width - 1 else cx in
        let cy = if cy >= height then height - 1 else cy in

        let cell_idx = cx + (cy * width) in

        if cell_idx >= 0 && cell_idx < Array.length grid_head then begin
          next_in_bucket.(item) <- grid_head.(cell_idx);
          grid_head.(cell_idx) <- item
        end
      done;

      {
        grid_head;
        next_in_bucket;
        inv_cell_size;
        min_x = !min_x;
        min_y = !min_y;
        width;
        height;
      }
    end
end

module Triangulator = struct
  open Geometry

  let verbose = ref false

  (* Collects bridges during triangulation for debugging/visualization *)
  let collected_bridges : (int * int) list ref = ref []
  let clear_bridges () = collected_bridges := []
  let get_bridges () = !collected_bridges

  (* Collects the loop state (vertex indices) when triangulation stalls before forcing *)
  let stalled_loops : int list list ref = ref []
  let clear_stalled_loops () = stalled_loops := []
  let get_stalled_loops () = !stalled_loops

  (* Expose normalized coordinates for debugging tools *)
  let last_normalized_coords : float array ref = ref [||]
  let get_last_normalized_coords () = !last_normalized_coords

  (* Check if segment a->b is strictly internal to the polygon at vertex a.
     a_prev and a_next are neighbors of a.
     ccw indicates if the ring containing a is counter-clockwise. *)
  let locally_inside (verts : float array) a a_prev a_next b ccw =
    let ax, ay = (get_x verts a, get_y verts a) in
    let px, py = (get_x verts a_prev, get_y verts a_prev) in
    let nx, ny = (get_x verts a_next, get_y verts a_next) in
    let bx, by = (get_x verts b, get_y verts b) in

    let cp_prev =
      Geometry.cross_product_2d (ax -. px) (ay -. py) (nx -. ax) (ny -. ay)
    in
    let cp_b_prev =
      Geometry.cross_product_2d (ax -. px) (ay -. py) (bx -. ax) (by -. ay)
    in
    let cp_b_next =
      Geometry.cross_product_2d (nx -. ax) (ny -. ay) (bx -. ax) (by -. ay)
    in

    let is_reflex = if ccw then cp_prev < -.epsilon else cp_prev > epsilon in

    if is_reflex then
      (* Reflex vertex - inside if it's NOT in the exterior cone (right of both) *)
      cp_b_prev > -.epsilon || cp_b_next > -.epsilon
    else
      (* Convex vertex - inside if it's in the interior cone (left of both) *)
      cp_b_prev > -.epsilon && cp_b_next > -.epsilon

  (* Check if a segment p1-p2 intersects any edge of a ring in poly_list *)
  let segment_crosses_ring (verts : float array) p1_idx p2_idx ring_start
      poly_list =
    let curr = ref ring_start in
    let result = ref false in
    let loop = ref true in
    while !loop && not !result do
      let next_node = poly_list.PolygonList.next.(!curr) in
      let a = poly_list.PolygonList.vert_idx.(!curr) in
      let b = poly_list.PolygonList.vert_idx.(next_node) in
      if segments_overlap verts p1_idx p2_idx a b then result := true;
      if next_node = ring_start then loop := false else curr := next_node
    done;
    !result

  let is_visible verts m_node p_node poly_start_node poly_list =
    let open PolygonList in
    let vert_idx = poly_list.vert_idx in
    let next = poly_list.next in
    let vi_m = vert_idx.(m_node) in
    let vi_p = vert_idx.(p_node) in

    try
      let curr = ref poly_start_node in
      let loop = ref true in
      let loop_count = ref 0 in
      while !loop do
        incr loop_count;
        if !loop_count > 20000 then loop := false;
        let n = next.(!curr) in

        (* Skip edges incident to nodes we are connecting (M or P) *)
        (* Note: we DO NOT skip other nodes at the same vertex, which is correct *)
        if !curr = p_node || n = p_node || !curr = m_node || n = m_node then ()
        else
          let vi_a = vert_idx.(!curr) in
          let vi_b = vert_idx.(n) in
          (* Nuanced check: Block if Strict crossing OR vertex obstruction *)
          (* ALLOW overlapping if P is on the edge (grazing/collinear) *)
          let strict_cross =
            Geometry.segments_cross verts vi_m vi_p vi_a vi_b
          in
          let vertex_on_ray =
            (if vi_a <> vi_m && vi_a <> vi_p then
               Geometry.dist_sq_point_segment
                 (Geometry.get_x verts vi_a)
                 (Geometry.get_y verts vi_a)
                 (Geometry.get_x verts vi_m)
                 (Geometry.get_y verts vi_m)
                 (Geometry.get_x verts vi_p)
                 (Geometry.get_y verts vi_p)
               < Geometry.epsilon
             else false)
            ||
            if vi_b <> vi_m && vi_b <> vi_p then
              Geometry.dist_sq_point_segment
                (Geometry.get_x verts vi_b)
                (Geometry.get_y verts vi_b)
                (Geometry.get_x verts vi_m)
                (Geometry.get_y verts vi_m)
                (Geometry.get_x verts vi_p)
                (Geometry.get_y verts vi_p)
              < Geometry.epsilon
            else false
          in

          if strict_cross || vertex_on_ray then (
            if !verbose then
              Printf.printf
                "  Visibility: bridge %d->%d blocked by edge %d-%d (nodes %d-%d)\n\
                 %!"
                m_node p_node vert_idx.(!curr) vert_idx.(n) !curr n;
            raise Exit);

          if n = poly_start_node then loop := false else curr := n
      done;
      true
    with Exit -> false

  let cross_product_coords ax ay bx by = (ax *. by) -. (ay *. bx)

  let point_in_triangle_coords ax ay bx by cx cy px py =
    let cp1 =
      cross_product_coords (bx -. ax) (by -. ay) (px -. ax) (py -. ay)
    in
    let cp2 =
      cross_product_coords (cx -. bx) (cy -. by) (px -. bx) (py -. by)
    in
    let cp3 =
      cross_product_coords (ax -. cx) (ay -. cy) (px -. cx) (py -. cy)
    in
    (cp1 >= -.epsilon && cp2 >= -.epsilon && cp3 >= -.epsilon)
    || (cp1 <= epsilon && cp2 <= epsilon && cp3 <= epsilon)

  (* Check if segment p1-p2 intersects any pending holes *)
  let crosses_pending_holes (verts : float array) p1_idx p2_idx poly_list
      processed_holes start_idx =
    let num_holes = Array.length processed_holes in
    let result = ref false in
    let i = ref start_idx in
    while !i < num_holes && not !result do
      let hole_node, _, _ = processed_holes.(!i) in
      if hole_node <> -1 then
        if segment_crosses_ring verts p1_idx p2_idx hole_node poly_list then
          result := true;
      incr i
    done;
    !result

  (* Find if any vertex in the hole touches (has same coordinates as) any vertex
     in the current outer ring. If found, returns Some (hole_node, outer_node). *)
  let find_touching_point (verts : float array) hole_start_node outer_node
      poly_list =
    let open PolygonList in
    let vert_idx = poly_list.vert_idx in
    let next = poly_list.next in
    (* Iterate through all hole vertices *)
    let hole_curr = ref hole_start_node in
    let hole_loop = ref true in
    let result = ref None in
    while !hole_loop && !result = None do
      let h_vi = vert_idx.(!hole_curr) in
      let hx = get_x verts h_vi in
      let hy = get_y verts h_vi in
      (* Check against all outer ring vertices *)
      let outer_curr = ref outer_node in
      let outer_loop = ref true in
      while !outer_loop && !result = None do
        let o_vi = vert_idx.(!outer_curr) in
        let ox = get_x verts o_vi in
        let oy = get_y verts o_vi in
        if abs_float (hx -. ox) < epsilon && abs_float (hy -. oy) < epsilon then
          result := Some (!hole_curr, !outer_curr);
        outer_curr := next.(!outer_curr);
        if !outer_curr = outer_node then outer_loop := false
      done;
      hole_curr := next.(!hole_curr);
      if !hole_curr = hole_start_node then hole_loop := false
    done;
    !result

  (* Merge a touching hole directly at the shared vertex without creating bridges.
     The hole is spliced into the outer ring at the touching point. *)
  let merge_touching_hole (verts : float array) hole_touch_node outer_touch_node
      poly_list =
    let open PolygonList in
    let h_idx = poly_list.vert_idx.(hole_touch_node) in
    let o_idx = poly_list.vert_idx.(outer_touch_node) in
    if !verbose then
      Printf.printf
        "  Merging touching hole: hole vert %d touches outer vert %d\n%!" h_idx
        o_idx;
    (* The hole traversal order is: hole_touch -> ... -> hole_prev -> hole_touch
       We want to insert the hole between outer_touch and outer_next.
       After merge: outer_touch -> hole_next -> ... -> hole_prev -> outer_next *)
    let hole_prev = poly_list.prev.(hole_touch_node) in
    let hole_next = poly_list.next.(hole_touch_node) in
    let outer_next = poly_list.next.(outer_touch_node) in
    (* Connect outer_touch to hole_next (skipping hole_touch since it's the same point) *)
    poly_list.next.(outer_touch_node) <- hole_next;
    poly_list.prev.(hole_next) <- outer_touch_node;
    (* Connect hole_prev back to outer_next *)
    poly_list.next.(hole_prev) <- outer_next;
    poly_list.prev.(outer_next) <- hole_prev;
    (* Return the outer touch node as the new start *)
    outer_touch_node

  let find_bridge_point (verts : float array) hole_start_node outer_node
      poly_list processed_holes pending_start_idx =
    let open PolygonList in
    let vert_idx = poly_list.vert_idx in
    let next = poly_list.next in
    let prev = poly_list.prev in

    let m_idx = vert_idx.(hole_start_node) in
    let mx, my = (get_x verts m_idx, get_y verts m_idx) in

    (* Helper to validate a candidate bridge *)
    let is_valid_bridge p_node =
      let p_idx = vert_idx.(p_node) in
      is_visible verts hole_start_node p_node outer_node poly_list
      && not
           (crosses_pending_holes verts m_idx p_idx poly_list processed_holes
              pending_start_idx)
    in

    (* Step 2: Intersect ray M + t(1,0) with all edges of outer polygon *)
    (* Find closest intersection I on the ray *)
    let best_t = ref infinity in
    let best_edge_p = ref (-1) in
    let best_edge_n = ref (-1) in
    let best_vi_p = ref (-1) in
    let best_vi_n = ref (-1) in
    let intersection_x = ref 0.0 in
    let intersection_is_vertex = ref false in
    let intersection_vertex_node = ref (-1) in
    let curr = ref outer_node in
    let loop = ref true in
    while !loop do
      let n = next.(!curr) in
      let vi_curr = vert_idx.(!curr) in
      let vi_next = vert_idx.(n) in
      let vx, vy = (get_x verts vi_curr, get_y verts vi_curr) in
      let nx, ny = (get_x verts vi_next, get_y verts vi_next) in

      (* Eberly Step 2: Check ANY edge that straddles the ray y-level *)
      (* Removed "upward only" restriction to handle CCW holes/bridges robustly *)
      (* Eberly Step 2: Check edge that straddles the ray y-level *)
      let dy = ny -. vy in
      let horizontal =
        abs_float dy < epsilon && abs_float (vy -. my) < epsilon
      in
      (* Edge straddles if it crosses the ray Y-level in either direction *)
      (* Need strict inequality on one side to avoid double-counting at vertices *)
      let straddles = vy <= my && ny > my in

      if straddles || horizontal then begin
        (* Calculate intersection *)
        let x_int_opt =
          if horizontal then
            if mx <= nx || mx <= vx then Some (fmax mx vx) else None
          else
            (* Standard intersection *)
            let t_edge = (my -. vy) /. dy in
            Some (vx +. (t_edge *. (nx -. vx)))
        in

        match x_int_opt with
        | Some x_int ->
            (* Only consider intersections to the right of M *)
            if x_int >= mx then begin
              let t = x_int -. mx in
              if t < !best_t -. epsilon then (
                best_t := t;
                best_edge_p := !curr;
                best_edge_n := n;
                best_vi_p := vi_curr;
                best_vi_n := vi_next;
                intersection_x := x_int;
                let is_v_curr =
                  abs_float (x_int -. vx) < epsilon
                  && abs_float (my -. vy) < epsilon
                in
                let is_v_next =
                  abs_float (x_int -. nx) < epsilon
                  && abs_float (my -. ny) < epsilon
                in

                if is_v_curr then (
                  intersection_is_vertex := true;
                  intersection_vertex_node := !curr)
                else if is_v_next then (
                  intersection_is_vertex := true;
                  intersection_vertex_node := n)
                else (
                  intersection_is_vertex := false;
                  if !verbose then
                    Printf.eprintf
                      "Hit edge %d-%d at t=%g (x_int=%g). Not a vertex hit? \
                       vx=%g vy=%g nx=%g ny=%g my=%g\n\
                       %!"
                      !curr n t x_int vx vy nx ny my))
              else if abs_float (t -. !best_t) < epsilon then
                (* Same intersection distance *)
                let update_if_shorter node =
                  let nx, ny =
                    ( get_x verts vert_idx.(poly_list.next.(node)),
                      get_y verts vert_idx.(poly_list.next.(node)) )
                  in
                  let px, py =
                    (get_x verts vert_idx.(node), get_y verts vert_idx.(node))
                  in
                  let len = ((nx -. px) ** 2.0) +. ((ny -. py) ** 2.0) in

                  let best_nx, best_ny =
                    ( get_x verts
                        vert_idx.(poly_list.next.(!intersection_vertex_node)),
                      get_y verts
                        vert_idx.(poly_list.next.(!intersection_vertex_node)) )
                  in
                  let best_px, best_py =
                    ( get_x verts vert_idx.(!intersection_vertex_node),
                      get_y verts vert_idx.(!intersection_vertex_node) )
                  in
                  let best_len =
                    ((best_nx -. best_px) ** 2.0)
                    +. ((best_ny -. best_py) ** 2.0)
                  in
                  if !verbose then
                    Printf.eprintf
                      "VERTEX TIE at %g,%g (M=%g,%g). Candidate %d (len %g) vs \
                       Best %d (len %g)\n\
                       %!"
                      !intersection_x my mx my node len
                      !intersection_vertex_node best_len;

                  (* User hint: Connect to the node added with previous bridge (the bridge node).
                      The bridge edge is shorter than the original boundary edge.
                      So we want the SHORTER outgoing edge. *)
                  if len < best_len then (
                    Printf.eprintf "  -> Swapping to %d (Shorter)\n%!" node;
                    intersection_vertex_node := node;
                    best_edge_p := !curr;
                    best_edge_n := n;
                    best_vi_p := vi_curr;
                    best_vi_n := vi_next)
                in

                if
                  abs_float (x_int -. vx) < epsilon
                  && abs_float (my -. vy) < epsilon
                then
                  if !intersection_is_vertex then update_if_shorter !curr
                  else (
                    intersection_is_vertex := true;
                    intersection_vertex_node := !curr)
                else if
                  abs_float (x_int -. nx) < epsilon
                  && abs_float (my -. ny) < epsilon
                then
                  if !intersection_is_vertex then update_if_shorter n
                  else (
                    intersection_is_vertex := true;
                    intersection_vertex_node := n)
                else if not !intersection_is_vertex then
                  (* Edge hit (interior). If edges overlap, prefer the SHORTER one. *)
                  let curr_len_sq =
                    ((nx -. vx) ** 2.0) +. ((ny -. vy) ** 2.0)
                  in
                  let best_vx = get_x verts !best_vi_p in
                  let best_vy = get_y verts !best_vi_p in
                  let best_nx = get_x verts !best_vi_n in
                  let best_ny = get_y verts !best_vi_n in
                  let best_len_sq =
                    ((best_nx -. best_vx) ** 2.0)
                    +. ((best_ny -. best_vy) ** 2.0)
                  in
                  (* Prefer SHORTER edge *)
                  if curr_len_sq < best_len_sq then (
                    best_edge_p := !curr;
                    best_edge_n := n;
                    best_vi_p := vi_curr;
                    best_vi_n := vi_next)
            end
        | None -> ()
      end;
      if n = outer_node then loop := false else curr := n
    done;

    if !best_t = infinity then None (* No intersection found *)
    else if !intersection_is_vertex then begin
      (* Step 3: I is vertex of outer polygon. Terminates. *)
      (* Use the SPECIFIC node found by intersection logic *)
      let p_node = !intersection_vertex_node in
      if !verbose then
        Printf.printf "  Eberly Step 3: Hit vertex %d (node %d). Visible!\n%!"
          vert_idx.(p_node) p_node;
      Some p_node
    end
    else begin
      (* Step 4: I is an interior point. Select P = max X endpoint. *)
      let p_node =
        let vx, nx = (get_x verts !best_vi_p, get_x verts !best_vi_n) in
        (* Robust Vertical Edge Handling: Ambiguous Max X -> Force Start Node *)
        if abs_float (vx -. nx) < epsilon then !best_edge_p
        else if vx >= nx then !best_edge_p
        else !best_edge_n
      in
      let p_idx = vert_idx.(p_node) in
      let p_x = get_x verts p_idx in
      let p_y = get_y verts p_idx in

      if !verbose then
        Printf.printf "  Eberly Step 4: Hit edge, P is %d (node %d)\n%!" p_idx
          p_node;

      (* Step 5-6: Search reflex vertices in triangle M-I-P *)
      (* If all reflex vertices are outside the triangle, M and P are mutually visible *)
      (* Otherwise, find reflex R that minimizes angle to ray M-I *)
      let reflex_candidates = ref [] in
      let search_curr = ref outer_node in
      let search_loop = ref true in
      let search_count = ref 0 in
      while !search_loop do
        incr search_count;
        if !search_count > 20000 then (
          if !verbose then
            Printf.printf "Breaking infinite loop in Eberly reflex search\n%!";
          search_loop := false);
        let search_node = !search_curr in
        let v_idx = vert_idx.(search_node) in

        (* Skip P itself *)
        if search_node <> p_node then begin
          let prv = prev.(search_node) in
          let nxt = next.(search_node) in
          let vi_prv = vert_idx.(prv) in
          let vi_rn = vert_idx.(nxt) in

          (* STRICTLY OUTSIDE check: Skip nodes co-located with P or M *)
          (* A reflex vertex at P or M does not block visibility to P *)
          if
            (not (points_equal verts v_idx p_idx))
            && not (points_equal verts v_idx vert_idx.(hole_start_node))
          then
            let cp = cross_product verts vi_prv v_idx vi_rn in
            let is_reflex = cp < -.epsilon in

            if is_reflex then begin
              let rx, ry = (get_x verts v_idx, get_y verts v_idx) in
              if point_in_triangle_coords mx my !intersection_x my p_x p_y rx ry
              then
                let dx = rx -. mx in
                let dy = abs_float (ry -. my) in
                if dx > epsilon then
                  reflex_candidates :=
                    (dy /. dx, search_node) :: !reflex_candidates
            end
        end;

        let nxt = next.(!search_curr) in
        if nxt = outer_node then search_loop := false else search_curr := nxt
      done;
      if !verbose then
        Printf.printf "  Reflex candidates: %d\n%!"
          (List.length !reflex_candidates);

      (* Sort candidates by angle_tan (Step 6) *)
      let sorted_reflex =
        List.sort (fun (t1, _) (t2, _) -> compare t1 t2) !reflex_candidates
      in

      (* Try each reflex vertex in order *)
      let rec try_reflex = function
        | [] -> None
        | (_, ref_node) :: rest ->
            (* Trust the specific reflex node found *)
            if is_valid_bridge ref_node then Some ref_node else try_reflex rest
      in

      let final_bridge = try_reflex sorted_reflex in

      match final_bridge with
      | Some node ->
          if !verbose then
            Printf.printf
              "  Eberly: selected bridge vertex %d (reflex=true)\n%!"
              vert_idx.(node);
          Some node
      | None ->
          (* Fallback to ray-intersection vertex P *)
          if !verbose then
            Printf.printf
              "  Eberly: No visible reflex vertex, trying ray vertex %d\n%!"
              p_idx;
          Some p_node
    end

  let merge_hole_into_outer (verts : float array) hole_start_node outer_node
      poly_list processed_holes pending_start_idx =
    (* First check if the hole touches the outer ring at any vertex *)
    match find_touching_point verts hole_start_node outer_node poly_list with
    | Some (hole_touch, outer_touch) ->
        (* Hole touches outer ring - merge directly without bridge *)
        let new_outer =
          merge_touching_hole verts hole_touch outer_touch poly_list
        in
        Some new_outer
    | None -> (
        (* No touching point - use standard bridge-based merge *)
        match
          find_bridge_point verts hole_start_node outer_node poly_list
            processed_holes pending_start_idx
        with
        | Some target ->
            let open PolygonList in
            (* Record the bridge for visualization *)
            let h_idx = poly_list.vert_idx.(hole_start_node) in
            let outer_idx = poly_list.vert_idx.(target) in
            collected_bridges := (h_idx, outer_idx) :: !collected_bridges;
            if !verbose then
              Printf.printf
                "  DEBUG: Bridge created between Hole node %d (vert %d) and \
                 Outer node %d (vert %d)\n\
                 %!"
                hole_start_node h_idx target outer_idx;
            if !verbose then
              Printf.printf
                "  Hole merged with bridge: Hole vert %d -> Outer vert %d\n%!"
                h_idx outer_idx;

            let p_prime = PolygonList.duplicate_node poly_list target in
            let m_prime =
              PolygonList.duplicate_node poly_list hole_start_node
            in
            let m_prev_n = poly_list.prev.(hole_start_node) in
            let p_next_n = poly_list.next.(target) in
            poly_list.next.(target) <- hole_start_node;
            poly_list.prev.(hole_start_node) <- target;
            poly_list.next.(m_prev_n) <- m_prime;
            poly_list.prev.(m_prime) <- m_prev_n;
            poly_list.next.(m_prime) <- p_prime;
            poly_list.prev.(p_prime) <- m_prime;
            poly_list.next.(p_prime) <- p_next_n;
            poly_list.prev.(p_next_n) <- p_prime;
            Some target
        | None -> None)

  let triangulate_dll (verts : float array) start_node poly_list
      total_active_nodes out_buffer out_offset =
    let open PolygonList in
    let next = poly_list.next in
    let prev = poly_list.prev in
    let vert_idx = poly_list.vert_idx in
    let active = Array.make poly_list.next_node true in

    let out_idx = ref out_offset in
    let count = ref total_active_nodes in
    let curr = ref start_node in
    let iterations = ref 0 in
    let max_iter = total_active_nodes * total_active_nodes in
    let since_last_progress = ref 0 in

    let log_self_intersection label tri_indices =
      let c = ref !curr in
      let ring_indices = ref [] in
      for _ = 1 to !count do
        ring_indices := vert_idx.(!c) :: !ring_indices;
        c := next.(!c)
      done;
      let ring = Array.of_list (List.rev !ring_indices) in
      let self_int_errors =
        Polygon_validation.check_self_intersection_indices verts ring
      in
      if self_int_errors <> [] then (
        Printf.printf
          "  CRITICAL: %s introduced self-intersection! (count %d%s)\n" label
          !count
          (match tri_indices with
          | Some (p, i, n) -> Printf.sprintf ", tri: %d-%d-%d" p i n
          | None -> "");
        List.iter
          (fun e ->
            Printf.printf "    -> %s\n" (Polygon_validation.string_of_error e))
          self_int_errors;
        Printf.printf "%!")
    in

    if !verbose then (
      let c = ref !curr in
      let ring_indices = ref [] in
      for _ = 1 to !count do
        ring_indices := vert_idx.(!c) :: !ring_indices;
        c := next.(!c)
      done;
      let ring = Array.of_list (List.rev !ring_indices) in
      let self_int_errors =
        Polygon_validation.check_self_intersection_indices verts ring
      in
      if self_int_errors <> [] then (
        Printf.printf "  CRITICAL: DLL started with self-intersection!\n";
        List.iter
          (fun e ->
            Printf.printf "    -> %s\n" (Polygon_validation.string_of_error e))
          self_int_errors;
        Printf.printf "%!"));

    let is_ear i =
      let node_prev = prev.(i) in
      let node_next = next.(i) in
      let vi_prev = vert_idx.(node_prev) in
      let vi_curr = vert_idx.(i) in
      let vi_next = vert_idx.(node_next) in

      if cross_product verts vi_prev vi_curr vi_next < epsilon then false
      else
        try
          let start_node = next.(node_next) in
          let check_node = ref start_node in
          let loop = ref true in
          if start_node = node_prev then loop := false;

          while !loop do
            let r_node = !check_node in
            if active.(r_node) then (
              let vi_r = vert_idx.(r_node) in
              if r_node = node_prev || r_node = i || r_node = node_next then ()
              else
                (* Check if reflex vertex intrudes into the ear triangle *)
                let vi_rp = vert_idx.(prev.(r_node)) in
                let vi_rn = vert_idx.(next.(r_node)) in
                if cross_product verts vi_rp vi_r vi_rn < -.epsilon then
                  if point_in_triangle verts vi_r vi_prev vi_curr vi_next then
                    raise Exit;

                (* Check if edge from r_node crosses the ear's base diagonal *)
                let r_next = next.(r_node) in
                if active.(r_next) then
                  let vi_rnext = vert_idx.(r_next) in
                  if
                    (r_node = node_prev && r_next = i)
                    || (r_node = i && r_next = node_next)
                  then ()
                  else if
                    Geometry.segments_cross verts vi_prev vi_next vi_r vi_rnext
                  then raise Exit);
            check_node := next.(!check_node);
            if !check_node = start_node then loop := false
          done;
          true
        with Exit -> false
    in

    while !count > 2 && !iterations < max_iter do
      incr iterations;
      incr since_last_progress;

      let i = !curr in
      if active.(i) then (
        let p = prev.(i) in
        let n = next.(i) in
        let vi_p = vert_idx.(p) in
        let vi_i = vert_idx.(i) in
        let vi_n = vert_idx.(n) in
        let cp = cross_product verts vi_p vi_i vi_n in

        if !verbose then
          Printf.printf "  DEBUG: Count %d, curr %d (vert %d), CP %.6f\n%!"
            !count i vi_i cp;
        if cp >= epsilon && is_ear i then (
          if !verbose then
            Printf.printf "  DEBUG: Clipping ear %d: %d-%d-%d (count %d)\n%!" i
              vi_p vi_i vi_n !count;
          if !out_idx + 2 < Array.length out_buffer then (
            out_buffer.(!out_idx) <- vi_p;
            out_buffer.(!out_idx + 1) <- vi_i;
            out_buffer.(!out_idx + 2) <- vi_n;
            out_idx := !out_idx + 3);
          active.(i) <- false;
          next.(p) <- n;
          prev.(n) <- p;
          decr count;
          since_last_progress := 0;
          curr := p;
          if !verbose then
            log_self_intersection "Clip" (Some (vi_p, vi_i, vi_n)))
        else if abs_float cp < epsilon then (
          if !verbose then
            Printf.printf
              "  DEBUG: Removing collinear %d: %d-%d-%d (count %d)\n%!" i vi_p
              vi_i vi_n !count;
          active.(i) <- false;
          next.(p) <- n;
          prev.(n) <- p;
          decr count;
          since_last_progress := 0;
          curr := p;
          if !verbose then log_self_intersection "Collinear removal" None)
        else curr := n)
      else curr := next.(i);

      if !since_last_progress > !count && !count > 2 then (
        (* Progress stalled *)
        let stalled_idx_val =
          let c = ref !curr in
          let acc = ref [] in
          for _ = 1 to !count do
            acc := vert_idx.(!c) :: !acc;
            c := next.(!c)
          done;
          List.rev !acc
        in
        stalled_loops := stalled_idx_val :: !stalled_loops;

        (if !verbose then
           let ring = Array.of_list stalled_idx_val in
           let self_int_errors =
             Polygon_validation.check_self_intersection_indices verts ring
           in
           if self_int_errors <> [] then (
             Printf.printf "  STALL DIAGNOSTIC: Self-intersection detected!\n";
             List.iter
               (fun e ->
                 Printf.printf "    -> %s\n"
                   (Polygon_validation.string_of_error e))
               self_int_errors;
             Printf.printf "%!"));

        (* Find the best convex node to force *)
        let best_n = ref !curr in
        let best_cp = ref neg_infinity in
        let found_c = ref false in
        let scan = ref !curr in
        for _ = 1 to !count do
          let node = !scan in
          let cp =
            cross_product verts
              vert_idx.(prev.(node))
              vert_idx.(node)
              vert_idx.(next.(node))
          in
          if cp > epsilon then (
            found_c := true;
            if cp > !best_cp then (
              best_cp := cp;
              best_n := node));
          scan := next.(!scan)
        done;

        let i = if !found_c then !best_n else !curr in
        let p = prev.(i) in
        let n = next.(i) in
        let vi_p, vi_i, vi_n = (vert_idx.(p), vert_idx.(i), vert_idx.(n)) in

        if !verbose then (
          Printf.printf
            "FORCING progress at count %d node %d (iterations %d) \
             [found_convex=%b cp=%.6f]\n\
             %!"
            !count i !iterations !found_c !best_cp;

          let c_winding = ref i in
          let sum_winding = ref 0.0 in
          for _ = 1 to !count do
            let v1 = vert_idx.(!c_winding) in
            let v2 = vert_idx.(next.(!c_winding)) in
            sum_winding :=
              !sum_winding
              +. ((get_x verts v1 *. get_y verts v2)
                 -. (get_x verts v2 *. get_y verts v1));
            c_winding := next.(!c_winding)
          done;
          Printf.printf "  STALL Winding (signed area): %.10f (CCW if > 0)\n"
            (!sum_winding *. 0.5);

          if !count < 200 then (
            Printf.printf "EAR ANALYSIS (%d nodes):\n" !count;
            let c_diag = ref i in
            for _ = 1 to !count do
              let curr_node = !c_diag in
              let p = prev.(curr_node) in
              let n = next.(curr_node) in
              let vi_p = vert_idx.(p) in
              let vi_i = vert_idx.(curr_node) in
              let vi_n = vert_idx.(n) in
              let cp = cross_product verts vi_p vi_i vi_n in

              if cp <= epsilon then
                Printf.printf "  Node %d (vert %d): Reflex/Flat (cp=%.6f)\n"
                  curr_node vi_i cp
              else (
                Printf.printf
                  "  Node %d (vert %d) (%.6f, %.6f): Convex (cp=%.6f). \
                   Rejections:\n"
                  curr_node vi_i (get_x verts vi_i) (get_y verts vi_i) cp;
                (* Check for intrusions *)
                let check_int = ref next.(n) in
                while !check_int <> p do
                  let v = vert_idx.(!check_int) in
                  if
                    not
                      (points_equal verts v vi_p || points_equal verts v vi_i
                     || points_equal verts v vi_n)
                  then
                    if point_in_triangle verts v vi_p vi_i vi_n then
                      Printf.printf
                        "    -> INTRUSION: Node %d (vert %d) at (%g, %g)\n"
                        !check_int v (get_x verts v) (get_y verts v);
                  check_int := next.(!check_int)
                done;
                (* Check for segment crossings *)
                let scan_edge = ref next.(n) in
                while !scan_edge <> p do
                  let r1 = !scan_edge in
                  let r2 = next.(r1) in
                  if
                    r1 = p || r1 = curr_node || r1 = n || r2 = p
                    || r2 = curr_node || r2 = n
                  then ()
                  else if
                    Geometry.segments_overlap verts vi_p vi_n vert_idx.(r1)
                      vert_idx.(r2)
                  then
                    Printf.printf
                      "    -> CROSSING: Base %d-%d crosses edge %d-%d (verts \
                       %d-%d-%d-%d)\n"
                      p n r1 r2 vi_p vi_n vert_idx.(r1) vert_idx.(r2);
                  scan_edge := next.(!scan_edge)
                done);
              c_diag := next.(!c_diag)
            done;
            Printf.printf "--- END EAR ANALYSIS ---\n"));

        if cross_product verts vi_p vi_i vi_n > epsilon then
          if !out_idx + 2 < Array.length out_buffer then (
            out_buffer.(!out_idx) <- vi_p;
            out_buffer.(!out_idx + 1) <- vi_i;
            out_buffer.(!out_idx + 2) <- vi_n;
            out_idx := !out_idx + 3);
        active.(i) <- false;
        next.(p) <- n;
        prev.(n) <- p;
        decr count;
        since_last_progress := 0;
        curr := p;
        if !verbose then
          log_self_intersection "Forced clip" (Some (vi_p, vi_i, vi_n)))
    done;

    if !count > 2 then begin
      if !verbose then
        Printf.printf "EMERGENCY: %d vertices remaining after main loop\n%!"
          !count;
      let i = ref !curr in
      let stop = !curr in
      let loop = ref true in
      let skipped = ref 0 in
      while !loop do
        let n = next.(!i) in
        let p = prev.(!i) in
        (* Stop if 2 nodes remaining *)
        if n = !i || n = p then loop := false
        else
          let vi_p = vert_idx.(p) in
          let vi_i = vert_idx.(!i) in
          let vi_n = vert_idx.(n) in
          let cp = cross_product verts vi_p vi_i vi_n in
          (* Only emit triangle if it has correct orientation *)
          if cp > epsilon then begin
            if !out_idx + 2 < Array.length out_buffer then (
              (* Bounds check for debugging *)
              let max_vert = (Array.length verts / 2) - 1 in
              if vi_p > max_vert || vi_i > max_vert || vi_n > max_vert then
                Printf.printf
                  "ERROR: Invalid vertex indices in EMERGENCY: %d, %d, %d (max \
                   %d)\n\
                   %!"
                  vi_p vi_i vi_n max_vert;
              out_buffer.(!out_idx) <- vi_p;
              out_buffer.(!out_idx + 1) <- vi_i;
              out_buffer.(!out_idx + 2) <- vi_n;
              out_idx := !out_idx + 3)
          end
          else incr skipped;
          next.(p) <- n;
          prev.(n) <- p;
          i := n;
          if !i = stop then loop := false
      done;
      if !verbose && !skipped > 0 then
        Printf.printf "EMERGENCY: Skipped %d inverted triangles\n%!" !skipped
    end;

    !out_idx

  (* Revised triangulate_single that avoids dead nodes *)
  let triangulate_single (verts : float array) polygon out_buffer start_offset =
    let outer_len = polygon.outer.len in
    let num_holes = Array.length polygon.holes in

    let total_hole_len =
      Array.fold_left (fun acc h -> acc + h.len) 0 polygon.holes
    in
    let total_verts = outer_len + total_hole_len in

    if total_verts < 3 then start_offset
    else
      let poly_list = PolygonList.create total_verts num_holes in

      let outer_ccw =
        Geometry.is_ccw_range verts polygon.outer.start polygon.outer.len
      in
      let outer_start_node =
        PolygonList.init_ring poly_list polygon.outer.start polygon.outer.len
          outer_ccw
      in
      if !verbose then
        Printf.printf
          "  Triangulate: outer_len %d, total_verts %d, next_node after outer %d\n\
           %!"
          outer_len total_verts poly_list.next_node;

      let curr_outer_node =
        ref (PolygonList.filter_points verts poly_list outer_start_node)
      in

      if num_holes > 0 then begin
        let processed_holes = Array.make num_holes (-1, 0.0, 0.0) in
        for i = 0 to num_holes - 1 do
          let h = polygon.holes.(i) in
          if h.len > 0 then (
            let data_is_ccw = Geometry.is_ccw_range verts h.start h.len in
            let hole_start =
              PolygonList.init_ring poly_list h.start h.len (not data_is_ccw)
            in
            let filtered_start =
              PolygonList.filter_points verts poly_list hole_start
            in
            let max_x = ref (get_x verts poly_list.vert_idx.(filtered_start)) in
            let max_y = ref (get_y verts poly_list.vert_idx.(filtered_start)) in
            let max_node = ref filtered_start in
            let curr = ref poly_list.next.(filtered_start) in

            (* Loop until we hit start again *)
            while !curr <> filtered_start do
              let vx = get_x verts poly_list.vert_idx.(!curr) in
              let vy = get_y verts poly_list.vert_idx.(!curr) in
              (* Find rightmost, then topmost *)
              if
                vx > !max_x
                || (abs_float (vx -. !max_x) < epsilon && vy > !max_y)
              then (
                max_x := vx;
                max_y := vy;
                max_node := !curr);
              curr := poly_list.next.(!curr)
            done;
            if !verbose then
              Printf.printf
                "  Hole %d: bridge_node %d (vert %d), max_x %g, max_y %g\n%!" i
                !max_node
                poly_list.vert_idx.(!max_node)
                !max_x !max_y;
            processed_holes.(i) <- (!max_node, !max_x, !max_y))
          else processed_holes.(i) <- (-1, neg_infinity, neg_infinity)
        done;

        (* Sort holes by max X desc, then max Y desc (Step 1 of Eberly) *)
        Array.sort
          (fun (_, x1, y1) (_, x2, y2) ->
            let cx = compare x2 x1 in
            if cx <> 0 then cx else compare y2 y1)
          processed_holes;

        if !verbose then
          Printf.printf "Merging %d holes...\n%!" (Array.length processed_holes);
        Array.iteri
          (fun i (bridge_node, _, _) ->
            if bridge_node <> -1 then
              match
                merge_hole_into_outer verts bridge_node !curr_outer_node
                  poly_list processed_holes i
              with
              | Some new_node ->
                  curr_outer_node := new_node;
                  if !verbose then (
                    let sum = ref 0.0 in
                    let first = ref true in
                    let start = !curr_outer_node in
                    let curr_t = ref start in
                    let n_nodes = ref 0 in
                    while !first || !curr_t <> start do
                      first := false;
                      let n = poly_list.next.(!curr_t) in
                      let vi = poly_list.vert_idx.(!curr_t) in
                      let vn = poly_list.vert_idx.(n) in
                      let x1 = Geometry.get_x verts vi in
                      let y1 = Geometry.get_y verts vi in
                      let x2 = Geometry.get_x verts vn in
                      let y2 = Geometry.get_y verts vn in
                      sum := !sum +. ((x1 *. y2) -. (x2 *. y1));
                      curr_t := n;
                      incr n_nodes
                    done;
                    let area = abs_float (!sum *. 0.5) in
                    Printf.printf
                      "  Hole %d merged (bridge %d). New area: %.6f (%d nodes)\n\
                       %!"
                      i bridge_node area !n_nodes)
              | None ->
                  if !verbose then
                    Printf.printf "  Warning: could not merge hole %d\n%!" i)
          processed_holes;

        (* Final validation before triangulation *)
        if !verbose then (
          Printf.printf "  Running self-intersection check...\n%!";
          let c = ref !curr_outer_node in
          let start = !c in
          let ring_indices = ref [] in
          let loop = ref true in
          while !loop do
            ring_indices := poly_list.vert_idx.(!c) :: !ring_indices;
            c := poly_list.next.(!c);
            if !c = start then loop := false
          done;
          let ring = Array.of_list (List.rev !ring_indices) in
          Printf.printf "  Checking %d vertices for self-intersection...\n%!"
            (Array.length ring);
          let self_int_errors =
            Polygon_validation.check_self_intersection_indices verts ring
          in
          let self_int = self_int_errors <> [] in
          Printf.printf "  Self-intersection result: %b\n%!" self_int;
          if self_int then (
            Printf.printf
              "  CRITICAL WARNING: Polygon is self-intersecting AFTER hole \
               merging!\n";
            List.iter
              (fun e ->
                Printf.printf "    -> %s\n"
                  (Polygon_validation.string_of_error e))
              self_int_errors;
            Printf.printf "%!"))
      end;

      curr_outer_node :=
        PolygonList.filter_points verts poly_list !curr_outer_node;

      let active_count = PolygonList.count_nodes poly_list !curr_outer_node in

      (* 
         Skip degenerate (zero-area) polygons.
         After clipping and hole merging, we may end up with collinear points
         that form a line segment rather than a polygon. Ear-clipping cannot
         triangulate these.
      *)
      if active_count < 3 then start_offset
      else
        let area =
          (* Compute signed area by walking the linked list *)
          let sum = ref 0.0 in
          let start = !curr_outer_node in
          let curr = ref start in
          let first = ref true in
          while !first || !curr <> start do
            first := false;
            let n = poly_list.next.(!curr) in
            let vi = poly_list.vert_idx.(!curr) in
            let vn = poly_list.vert_idx.(n) in
            let x1 = Geometry.get_x verts vi in
            let y1 = Geometry.get_y verts vi in
            let x2 = Geometry.get_x verts vn in
            let y2 = Geometry.get_y verts vn in
            sum := !sum +. ((x1 *. y2) -. (x2 *. y1));
            curr := n
          done;
          abs_float (!sum *. 0.5)
        in
        if area < Geometry.epsilon then start_offset
        else
          triangulate_dll verts !curr_outer_node poly_list active_count
            out_buffer start_offset

  (* Use Geometry_types.validate_polygon *)

  (* Compute signed area of a triangle given 3 vertex indices *)
  let triangle_area verts i0 i1 i2 =
    let x0 = Geometry.get_x verts i0 in
    let y0 = Geometry.get_y verts i0 in
    let x1 = Geometry.get_x verts i1 in
    let y1 = Geometry.get_y verts i1 in
    let x2 = Geometry.get_x verts i2 in
    let y2 = Geometry.get_y verts i2 in
    abs_float (((x1 -. x0) *. (y2 -. y0)) -. ((x2 -. x0) *. (y1 -. y0))) *. 0.5

  (* Compute polygon area (outer - holes) *)
  let polygon_area verts poly =
    let outer_area =
      abs_float
        (Geometry.signed_area_range verts poly.outer.start poly.outer.len)
    in
    let holes_area =
      Array.fold_left
        (fun acc h ->
          acc +. abs_float (Geometry.signed_area_range verts h.start h.len))
        0.0 poly.holes
    in
    outer_area -. holes_area

  let triangulate_multi ?(tile = "") ?(feature_type = "") (verts : float array)
      polygons =
    Array.iter
      (fun poly ->
        validate_polygon verts poly;
        let cheap_errors = Polygon_validation.validate_cheap verts poly in
        if cheap_errors <> [] then (
          Printf.printf "Cheap validation errors for tile %s type %s:\n" tile
            feature_type;
          List.iter
            (fun e ->
              Printf.printf "  - %s\n" (Polygon_validation.string_of_error e))
            cheap_errors;
          Printf.printf "%!"))
      polygons;

    let total_triangles =
      Array.fold_left
        (fun acc poly ->
          let v_count =
            poly.outer.len
            + Array.fold_left (fun h_acc h -> h_acc + h.len) 0 poly.holes
          in
          let n_merged = v_count + (2 * Array.length poly.holes) in
          let n = n_merged - 2 in
          acc + if n > 0 then n else 0)
        0 polygons
    in

    let out_buffer = Array.make (total_triangles * 3) 0 in

    (* Normalize vertices for robustness *)
    let norm_verts, scale_factor = Geometry.normalize_vertices verts in
    last_normalized_coords := norm_verts;
    if !verbose then
      Printf.printf "Normalized vertices with scale factor: %f\n%!" scale_factor;

    let offset = ref 0 in

    Array.iter
      (fun poly ->
        let start_offset = !offset in
        (* Use normalized vertices for triangulation *)
        offset := triangulate_single norm_verts poly out_buffer !offset;
        let expected = polygon_area verts poly in

        (* Verify this specific polygon *)
        let poly_tris = (!offset - start_offset) / 3 in
        let actual = ref 0.0 in
        for i = 0 to poly_tris - 1 do
          let v_idx = start_offset + (i * 3) in
          let i1, i2, i3 =
            (out_buffer.(v_idx), out_buffer.(v_idx + 1), out_buffer.(v_idx + 2))
          in
          actual := !actual +. triangle_area verts i1 i2 i3
        done;
        if abs_float (expected -. !actual) >= 1e-11 then begin
          let ratio = !actual /. expected in

          if !verbose then
            Printf.printf "  Captured Actual Area: %g (ratio: %.4f)\n" !actual
              ratio;

          (* Quality Metrics *)
          let min_angle = ref 180.0 in
          let max_edge_sq = ref 0.0 in
          let theoretical_tris =
            poly.outer.len
            + Array.fold_left (fun acc h -> acc + h.len) 0 poly.holes
            + (2 * Array.length poly.holes)
            - 2
          in

          for i = 0 to poly_tris - 1 do
            let v_idx = start_offset + (i * 3) in
            let i1, i2, i3 =
              ( out_buffer.(v_idx),
                out_buffer.(v_idx + 1),
                out_buffer.(v_idx + 2) )
            in
            (*
            let x1, y1 = (Geometry.get_x verts i1, Geometry.get_y verts i1) in
            let x2, y2 = (Geometry.get_x verts i2, Geometry.get_y verts i2) in
            let x3, y3 = (Geometry.get_x verts i3, Geometry.get_y verts i3) in
*)

            let d12_sq = Geometry.dist_sq verts i1 i2 in
            let d23_sq = Geometry.dist_sq verts i2 i3 in
            let d31_sq = Geometry.dist_sq verts i3 i1 in

            max_edge_sq :=
              Geometry.fmax !max_edge_sq
                (Geometry.fmax d12_sq (Geometry.fmax d23_sq d31_sq));

            let d12 = sqrt d12_sq in
            let d23 = sqrt d23_sq in
            let d31 = sqrt d31_sq in

            (* Cosine rule: c^2 = a^2 + b^2 - 2ab cos(C) => cos(C) = (a^2 + b^2 - c^2) / 2ab *)
            let angle a b c =
              let cos_c =
                ((a *. a) +. (b *. b) -. (c *. c)) /. (2.0 *. a *. b)
              in
              let cos_c = Geometry.fmax (-1.0) (Geometry.fmin 1.0 cos_c) in
              acos cos_c *. 180.0 /. 3.14159265
            in
            if d12 > 1e-9 && d23 > 1e-9 && d31 > 1e-9 then begin
              min_angle := Geometry.fmin !min_angle (angle d12 d23 d31);
              min_angle := Geometry.fmin !min_angle (angle d23 d31 d12);
              min_angle := Geometry.fmin !min_angle (angle d31 d12 d23)
            end
          done;

          let quality_warnings = ref [] in
          if poly_tris <> theoretical_tris then
            quality_warnings :=
              Printf.sprintf "Triangle count mismatch: got %d, expected %d"
                poly_tris theoretical_tris
              :: !quality_warnings;
          if !min_angle < 1.0 then
            quality_warnings :=
              Printf.sprintf "Degenerate triangles: min angle %.2f deg"
                !min_angle
              :: !quality_warnings;

          if ratio < 0.995 || ratio > 1.005 then begin
            Printf.printf
              "AREA MISMATCH: expected=%g, actual=%g, ratio=%.4f (%d tris)\n%!"
              expected !actual ratio poly_tris;

            List.iter
              (fun w -> Printf.printf "  QUALITY WARNING: %s\n%!" w)
              !quality_warnings;

            (* Save failing polygon for debugging if tile is specified *)
            if tile <> "" then begin
              (* Run expensive validation diagnostics *)
              let cheap_errors = Polygon_validation.validate_cheap verts poly in
              let expensive_errors =
                Polygon_validation.validate_expensive verts poly
              in
              let all_errors = cheap_errors @ expensive_errors in
              let validation_errors =
                List.map Polygon_validation.string_of_error all_errors
                @ !quality_warnings
              in

              (* Extract outer ring coordinates *)
              let outer_arr =
                Array.init (poly.outer.len * 2) (fun i ->
                    verts.((poly.outer.start * 2) + i))
              in
              (* Extract hole coordinates *)
              let holes_arr =
                Array.map
                  (fun h ->
                    Array.init (h.len * 2) (fun i -> verts.((h.start * 2) + i)))
                  poly.holes
              in
              ignore
                (Polygon_test_utils.save_polygon_json ~tile ~feature_type
                   ~expected_area:expected ~actual_area:!actual ~outer:outer_arr
                   ~holes:holes_arr ~validation_errors ())
            end
          end
          else if !quality_warnings <> [] then begin
            (* Log quality warnings even if area is correct *)
            Printf.printf
              "Triangulation quality warning for %s %s (Area OK): %d tris\n%!"
              tile feature_type poly_tris;
            List.iter
              (fun w -> Printf.printf "  QUALITY WARNING: %s\n%!" w)
              !quality_warnings
          end
        end)
      polygons;

    if !offset < Array.length out_buffer then Array.sub out_buffer 0 !offset
    else out_buffer

  let triangulate (flat_verts : float array) hole_starts =
    let total_len = Array.length flat_verts / 2 in
    let num_holes = Array.length hole_starts in

    let outer_len = if num_holes > 0 then hole_starts.(0) else total_len in
    let outer_ring = { start = 0; len = outer_len } in

    let hole_rings =
      Array.init num_holes (fun i ->
          let start = hole_starts.(i) in
          let end_ =
            if i + 1 < num_holes then hole_starts.(i + 1) else total_len
          in
          { start; len = end_ - start })
    in

    triangulate_multi flat_verts
      [| { outer = outer_ring; holes = hole_rings } |]
end
