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

  let intersects (verts : float array) ia ib ic id =
    let cp1 = cross_product verts ia ib ic in
    let cp2 = cross_product verts ia ib id in
    let cp3 = cross_product verts ic id ia in
    let cp4 = cross_product verts ic id ib in
    ((cp1 > epsilon && cp2 < -.epsilon) || (cp1 < -.epsilon && cp2 > epsilon))
    && ((cp3 > epsilon && cp4 < -.epsilon) || (cp3 < -.epsilon && cp4 > epsilon))
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
        let p3 = t.vert_idx.(next_i) in

        let p1x, p1y = (Geometry.get_x verts p1, Geometry.get_y verts p1) in
        let p2x, p2y = (Geometry.get_x verts p2, Geometry.get_y verts p2) in
        (* p3 coords retrieved implicitly inside cross_product calls *)

        let is_dup = p1x = p2x && p1y = p2y in

        if is_dup then (
          t.prev.(next_i) <- prev_i;
          t.next.(prev_i) <- next_i;
          (* FIXED: Do NOT decrement t.count allocator cursor *)
          end_node := prev_i;
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

  let on_segment (verts : float array) ia ib ip =
    let ax, ay = (get_x verts ia, get_y verts ia) in
    let bx, by = (get_x verts ib, get_y verts ib) in
    let px, py = (get_x verts ip, get_y verts ip) in
    dist_sq_point_segment px py ax ay bx by < epsilon *. epsilon

  let segments_overlap verts p1 p2 a b =
    (* Use coordinate equality to handle duplicate vertices from bridge merging *)
    let p1_eq_a = points_equal verts p1 a in
    let p1_eq_b = points_equal verts p1 b in
    let p2_eq_a = points_equal verts p2 a in
    let p2_eq_b = points_equal verts p2 b in
    if p1_eq_a || p1_eq_b || p2_eq_a || p2_eq_b then
      (* Shared endpoint (by coordinates) - only overlap if the OTHER endpoint lies on the other segment *)
      if (not p1_eq_a) && (not p1_eq_b) && on_segment verts a b p1 then true
      else if (not p2_eq_a) && (not p2_eq_b) && on_segment verts a b p2 then
        true
      else if (not p1_eq_a) && (not p2_eq_a) && on_segment verts p1 p2 a then
        true
      else if (not p1_eq_b) && (not p2_eq_b) && on_segment verts p1 p2 b then
        true
      else false
    else
      (* No shared endpoints - standard intersection or any endpoint on segment *)
      intersects verts p1 p2 a b || on_segment verts a b p1
      || on_segment verts a b p2 || on_segment verts p1 p2 a
      || on_segment verts p1 p2 b

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

  let is_visible (verts : float array) p1_idx p2_idx poly_start_node poly_list =
    let curr_node = ref poly_start_node in
    let result = ref true in
    let loop = ref true in

    while !loop do
      let next_node = poly_list.PolygonList.next.(!curr_node) in
      let a = poly_list.PolygonList.vert_idx.(!curr_node) in
      let b = poly_list.PolygonList.vert_idx.(next_node) in
      let intersected = segments_overlap verts p1_idx p2_idx a b in

      if intersected then (
        if !verbose then Printf.printf "  Blocked by edge %d-%d\n%!" a b;
        result := false;
        loop := false)
      else if next_node = poly_start_node then loop := false
      else curr_node := next_node
    done;
    !result

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
      let hole_node, _ = processed_holes.(!i) in
      if hole_node <> -1 then
        if segment_crosses_ring verts p1_idx p2_idx hole_node poly_list then
          result := true;
      incr i
    done;
    !result

  let find_bridge_point (verts : float array) hole_start_node outer_node
      poly_list processed_holes pending_start_idx =
    let open PolygonList in
    let vert_idx = poly_list.vert_idx in
    let next = poly_list.next in
    let prev = poly_list.prev in

    let m_idx = vert_idx.(hole_start_node) in
    let mx, my = (get_x verts m_idx, get_y verts m_idx) in

    (* Helper to validate a candidate bridge *)
    let is_valid_bridge p_node p_idx =
      is_visible verts m_idx p_idx outer_node poly_list
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
    let loop_count = ref 0 in
    while !loop do
      incr loop_count;
      if !loop_count > 20000 then (
        if !verbose then
          Printf.printf "Breaking infinite loop in Eberly Phase 1\n%!";
        loop := false);
      let n = next.(!curr) in
      let vi_curr = vert_idx.(!curr) in
      let vi_next = vert_idx.(n) in
      let vx, vy = (get_x verts vi_curr, get_y verts vi_curr) in
      let nx, ny = (get_x verts vi_next, get_y verts vi_next) in

      (* Edge must have V_i <= my < V_{i+1} or V_{i+1} <= my < V_i *)
      (* (V_i is at or below ray, V_{i+1} is above ray, or vice versa) *)
      if (vy <= my && ny > my) || (ny <= my && vy > my) then begin
        (* Compute intersection x-coordinate *)
        let t_edge = (my -. vy) /. (ny -. vy) in
        let x_int = vx +. (t_edge *. (nx -. vx)) in
        (* Only consider intersections to the right of M *)
        if x_int >= mx then begin
          let t = x_int -. mx in
          (* distance along ray *)
          if t < !best_t then begin
            best_t := t;
            best_edge_p := !curr;
            best_edge_n := n;
            best_vi_p := vi_curr;
            best_vi_n := vi_next;
            intersection_x := x_int;
            (* Check if intersection is exactly at a vertex *)
            if
              abs_float (x_int -. vx) < epsilon
              && abs_float (my -. vy) < epsilon
            then begin
              intersection_is_vertex := true;
              intersection_vertex_node := !curr
            end
            else if
              abs_float (x_int -. nx) < epsilon
              && abs_float (my -. ny) < epsilon
            then begin
              intersection_is_vertex := true;
              intersection_vertex_node := n
            end
            else intersection_is_vertex := false
          end
        end
      end;
      if n = outer_node then loop := false else curr := n
    done;

    if !best_t = infinity then None (* No intersection found *)
    else begin
      (* Step 3: If I is a vertex, M and I are mutually visible *)
      if !intersection_is_vertex then begin
        if !verbose then
          Printf.printf "  Eberly: I is vertex %d, using directly\n%!"
            vert_idx.(!intersection_vertex_node);
        if
          is_valid_bridge !intersection_vertex_node
            vert_idx.(!intersection_vertex_node)
        then Some !intersection_vertex_node
        else None
      end
      else begin
        (* Step 4: I is interior to edge. Select P = endpoint with max x-value *)
        let px = get_x verts !best_vi_p in
        let nx = get_x verts !best_vi_n in
        let p_node, p_idx =
          if px >= nx then (!best_edge_p, !best_vi_p)
          else (!best_edge_n, !best_vi_n)
        in
        let p_x = get_x verts p_idx in
        let p_y = get_y verts p_idx in

        (* Step 5-6: Search reflex vertices in triangle M-I-P *)
        (* If all reflex vertices are outside the triangle, M and P are mutually visible *)
        (* Otherwise, find reflex R that minimizes angle to ray M-I *)
        let best_reflex_node = ref (-1) in
        let min_angle_tan = ref infinity in
        (* we'll use tan(angle) = dy/dx for comparison *)

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
            let vi_nxt = vert_idx.(nxt) in

            (* Check if this vertex is reflex (interior angle > 180 degrees) *)
            (* For CCW polygon, reflex if cross product < 0 *)
            let cp = cross_product verts vi_prv v_idx vi_nxt in
            let is_reflex = cp < -.epsilon in

            if is_reflex then begin
              let rx, ry = (get_x verts v_idx, get_y verts v_idx) in

              (* Check if R is inside triangle M-I-P *)
              (* M = (mx, my), I = (intersection_x, my), P = (p_x, p_y) *)
              let in_triangle =
                point_in_triangle_coords mx my !intersection_x my p_x p_y rx ry
              in

              if in_triangle then begin
                (* Compute angle between ray M-I (horizontal) and M-R *)
                (* tan(angle) = |ry - my| / |rx - mx| *)
                let dx = rx -. mx in
                let dy = abs_float (ry -. my) in
                if dx > epsilon then begin
                  let angle_tan = dy /. dx in
                  if angle_tan < !min_angle_tan then begin
                    min_angle_tan := angle_tan;
                    best_reflex_node := search_node
                  end
                end
              end
            end
          end;

          let nxt = next.(!search_curr) in
          if nxt = outer_node then search_loop := false else search_curr := nxt
        done;

        (* Select final bridge point *)
        let result_node =
          if !best_reflex_node <> -1 then !best_reflex_node else p_node
        in
        let result_idx = vert_idx.(result_node) in

        if !verbose then
          Printf.printf "  Eberly: selected bridge point %d (reflex=%b)\n%!"
            result_idx (!best_reflex_node <> -1);

        if is_valid_bridge result_node result_idx then Some result_node
        else begin
          (* Fallback: try P if reflex failed *)
          if !best_reflex_node <> -1 && is_valid_bridge p_node p_idx then
            Some p_node
          else None
        end
      end
    end

  let merge_hole_into_outer (verts : float array) hole_start_node outer_node
      poly_list processed_holes pending_start_idx =
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
            "  DEBUG: Bridge created between Hole node %d (vert %d) and Outer \
             node %d (vert %d)\n\
             %!"
            hole_start_node h_idx target outer_idx;
        Printf.printf
          "  DEBUG: Bridge created between Hole vert %d and Outer vert %d\n%!"
          h_idx outer_idx;
        if !verbose then
          Printf.printf
            "  Hole merged with bridge: Hole vert %d -> Outer vert %d\n%!" h_idx
            outer_idx;

        let p_prime = PolygonList.duplicate_node poly_list target in
        let m_prime = PolygonList.duplicate_node poly_list hole_start_node in
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
    | None -> None

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
          (* Check all active nodes for intrusions *)
          let check_node = ref next.(node_next) in
          while !check_node <> node_prev do
            let r_node = !check_node in
            (if active.(r_node) then
               let vi_r = vert_idx.(r_node) in
               if r_node = node_prev || r_node = i || r_node = node_next then ()
               else if
                 points_equal verts vi_r vi_prev
                 || points_equal verts vi_r vi_curr
                 || points_equal verts vi_r vi_next
               then ()
               else if point_in_triangle verts vi_r vi_prev vi_curr vi_next then
                 raise Exit);
            check_node := next.(!check_node)
          done;
          true
        with Exit -> false
    in

    while !count > 2 && !iterations < max_iter do
      incr iterations;
      incr since_last_progress;

      let i = !curr in
      if active.(i) then
        let p = prev.(i) in
        let n = next.(i) in
        let vi_p = vert_idx.(p) in
        let vi_i = vert_idx.(i) in
        let vi_n = vert_idx.(n) in
        let cp = cross_product verts vi_p vi_i vi_n in

        if cp >= epsilon && is_ear i then (
          if !out_idx + 2 < Array.length out_buffer then (
            (* Bounds check for debugging *)
            let max_vert = (Array.length verts / 2) - 1 in
            if vi_p > max_vert || vi_i > max_vert || vi_n > max_vert then
              Printf.printf
                "ERROR: Invalid vertex indices in ear clip: %d, %d, %d (max %d)\n\
                 %!"
                vi_p vi_i vi_n max_vert;
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
              Printf.printf
                "  CRITICAL: Clip introduced self-intersection! (count %d, \
                 tri: %d-%d-%d)\n"
                !count vi_p vi_i vi_n;
              List.iter
                (fun e ->
                  Printf.printf "    -> %s\n"
                    (Polygon_validation.string_of_error e))
                self_int_errors;
              Printf.printf "%!")))
        else if abs_float cp < epsilon then (
          (* Collinear - remove it *)
          active.(i) <- false;
          next.(p) <- n;
          prev.(n) <- p;
          decr count;
          since_last_progress := 0;
          curr := p;
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
              Printf.printf
                "  CRITICAL: Collinear removal introduced self-intersection! \
                 (count %d)\n"
                !count;
              List.iter
                (fun e ->
                  Printf.printf "    -> %s\n"
                    (Polygon_validation.string_of_error e))
                self_int_errors;
              Printf.printf "%!")))
        else
          (* Concave or convex-but-not-ear - skip *)
          curr := n
      else curr := next.(i);

      if !since_last_progress > !count && !count > 2 then (
        (* Progress stalled - capture vertex indices for visualization *)
        let c = ref !curr in
        let stalled_idx = ref [] in
        for _ = 1 to !count do
          stalled_idx := vert_idx.(!c) :: !stalled_idx;
          c := next.(!c)
        done;
        stalled_loops := List.rev !stalled_idx :: !stalled_loops;

        (if !verbose then
           let ring = Array.of_list (List.rev !stalled_idx) in
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
        let best_node = ref !curr in
        let best_cp = ref neg_infinity in
        let found_convex = ref false in

        let scan_c = ref !curr in
        for _ = 1 to !count do
          let node = !scan_c in
          let p = prev.(node) in
          let n = next.(node) in
          let cp =
            cross_product verts vert_idx.(p) vert_idx.(node) vert_idx.(n)
          in
          if cp > epsilon then (
            found_convex := true;
            if cp > !best_cp then (
              best_cp := cp;
              best_node := node));
          scan_c := next.(!scan_c)
        done;

        let i = if !found_convex then !best_node else !curr in

        if !verbose then (
          Printf.printf
            "FORCING progress at count %d node %d (iterations %d) \
             [found_convex=%b cp=%.6f]\n\
             %!"
            !count i !iterations !found_convex !best_cp;

          if !iterations > 1000 then begin
            (* Save stalled state for analysis *)
            let c = ref i in
            let nodes = ref [] in
            for _ = 1 to !count do
              nodes := !c :: !nodes;
              c := next.(!c)
            done;
            let nodes = List.rev !nodes in
            let coords =
              List.map
                (fun n -> (get_x verts vert_idx.(n), get_y verts vert_idx.(n)))
                nodes
            in
            Printf.printf "STALLED POLYGON (%d nodes): " !count;
            List.iter (fun (x, y) -> Printf.printf "(%g,%g) " x y) coords;
            Printf.printf "\n%!"
          end;

          if !count < 200 then (
            let sum_area = ref 0.0 in
            let c_area = ref i in
            for _ = 1 to !count do
              let v1 = vert_idx.(!c_area) in
              let v2 = vert_idx.(next.(!c_area)) in
              sum_area :=
                !sum_area
                +. ((get_x verts v1 *. get_y verts v2)
                   -. (get_x verts v2 *. get_y verts v1));
              c_area := next.(!c_area)
            done;
            let loop_area = !sum_area *. 0.5 in
            Printf.printf "EAR ANALYSIS (%d nodes, signed area %.6g):\n" !count
              loop_area;
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
                Printf.printf "  Node %d: Reflex/Flat (cp=%.6f)\n" curr_node cp
              else begin
                Printf.printf
                  "  Node %d: Convex (cp=%.6f). Checking intrusions...\n"
                  curr_node cp;
                let check = ref next.(n) in
                let stop = p in
                let found = ref false in
                while !check <> stop && not !found do
                  let v = vert_idx.(!check) in
                  if
                    not
                      (points_equal verts v vi_p || points_equal verts v vi_i
                     || points_equal verts v vi_n)
                  then
                    if point_in_triangle verts v vi_p vi_i vi_n then (
                      let vx, vy = (get_x verts v, get_y verts v) in
                      let px, py = (get_x verts vi_p, get_y verts vi_p) in
                      let ix, iy = (get_x verts vi_i, get_y verts vi_i) in
                      let nx, ny = (get_x verts vi_n, get_y verts vi_n) in
                      Printf.printf
                        "    -> FAILED: Contains node %d (vert %d) (%.6f, \
                         %.6f) in tri [(%.6f,%.6f), (%.6f,%.6f), (%.6f,%.6f)]\n"
                        !check v vx vy px py ix iy nx ny;
                      found := true);
                  check := next.(!check)
                done;
                if not !found then
                  Printf.printf "    -> PASSED: Should be an ear!\n"
              end;
              c_diag := next.(!c_diag)
            done;
            Printf.printf "--- END EAR ANALYSIS ---\n"));

        let p = prev.(i) in
        let n = next.(i) in
        let vi_p = vert_idx.(p) in
        let vi_i = vert_idx.(i) in
        let vi_n = vert_idx.(n) in
        let cp = cross_product verts vi_p vi_i vi_n in

        (* Only emit triangle during FORCING if:
           1. Positive cross product (valid orientation)
           2. Triangle edges aren't excessively long (prevents massive triangles) *)
        let max_reasonable_edge = 5.0 in
        (* In normalized space, max distance is ~2.82. 5.0 is safe. *)
        let edge1 = dist_sq verts vi_p vi_i in
        let edge2 = dist_sq verts vi_i vi_n in
        let edge3 = dist_sq verts vi_n vi_p in
        let max_edge = sqrt (Geometry.fmax edge1 (Geometry.fmax edge2 edge3)) in

        if cp > epsilon && max_edge < max_reasonable_edge then
          if !out_idx + 2 < Array.length out_buffer then (
            out_buffer.(!out_idx) <- vi_p;
            out_buffer.(!out_idx + 1) <- vi_i;
            out_buffer.(!out_idx + 2) <- vi_n;
            out_idx := !out_idx + 3);

        (* Always remove the vertex to prevent infinite loops *)
        active.(i) <- false;
        next.(p) <- n;
        prev.(n) <- p;
        decr count;
        since_last_progress := 0;
        curr := p)
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
        let processed_holes = Array.make num_holes (0, 0.0) in
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
            let max_node = ref filtered_start in
            let curr = ref poly_list.next.(filtered_start) in

            (* Loop until we hit start again *)
            while !curr <> filtered_start do
              let vx = get_x verts poly_list.vert_idx.(!curr) in
              if vx > !max_x then (
                max_x := vx;
                max_node := !curr);
              curr := poly_list.next.(!curr)
            done;
            if !verbose then
              Printf.printf
                "  Hole %d: start_node %d (vert %d), filtered_node %d (vert %d)\n\
                 %!"
                i hole_start
                poly_list.vert_idx.(hole_start)
                filtered_start
                poly_list.vert_idx.(filtered_start);
            processed_holes.(i) <- (!max_node, !max_x))
          else processed_holes.(i) <- (-1, neg_infinity)
        done;

        (* Sort holes by max X desc *)
        Array.sort (fun (_, x1) (_, x2) -> compare x2 x1) processed_holes;

        if !verbose then
          Printf.printf "Merging %d holes...\n%!" (Array.length processed_holes);
        Array.iteri
          (fun i (bridge_node, _) ->
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
