(** Fast Polygon Triangulation with Holes in OCaml Algorithm: Ear Clipping with
    Spatial Hashing (and Brute-force fallback) *)

open Geometry_types

(* Types removed - using Geometry_types *)

module Geometry = struct
  let epsilon = 1e-9

  let get_x (verts : float array) i = Array.unsafe_get verts (i * 2)
  [@@inline always]

  let get_y (verts : float array) i = Array.unsafe_get verts ((i * 2) + 1)
  [@@inline always]

  let fmax (a : float) (b : float) = if a > b then a else b [@@inline always]
  let fmin (a : float) (b : float) = if a < b then a else b [@@inline always]

  let cross_product (verts : float array) ia ib ic =
    let ax, ay = (get_x verts ia, get_y verts ia) in
    let bx, by = (get_x verts ib, get_y verts ib) in
    let cx, cy = (get_x verts ic, get_y verts ic) in
    ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax))
  [@@inline always]

  let point_in_triangle (verts : float array) ip ia ib ic =
    let cp1 = cross_product verts ia ib ip in
    let cp2 = cross_product verts ib ic ip in
    let cp3 = cross_product verts ic ia ip in
    (cp1 >= -.epsilon && cp2 >= -.epsilon && cp3 >= -.epsilon)
    || (cp1 <= epsilon && cp2 <= epsilon && cp3 <= epsilon)
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
    mutable count : int;
  }

  let create total_verts num_holes =
    let capacity = total_verts + (num_holes * 2) + 2 in
    {
      next = Array.make capacity 0;
      prev = Array.make capacity 0;
      vert_idx = Array.make capacity 0;
      count = 0;
    }

  let init_ring t start_vert len is_ccw =
    let start_node = t.count in
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
    t.count <- t.count + len;
    start_node

  let duplicate_node t ref_node =
    let new_node = t.count in
    t.vert_idx.(new_node) <- t.vert_idx.(ref_node);
    t.count <- t.count + 1;
    new_node

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
        let is_collinear =
          if is_dup then true
          else
            abs_float (Geometry.cross_product verts p1 p2 p3) < Geometry.epsilon
        in

        if is_collinear then (
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

  let on_segment (verts : float array) ia ib ip =
    let ax, ay = (get_x verts ia, get_y verts ia) in
    let bx, by = (get_x verts ib, get_y verts ib) in
    let px, py = (get_x verts ip, get_y verts ip) in
    abs_float (cross_product verts ia ib ip) < epsilon
    && px >= fmin ax bx -. epsilon
    && px <= fmax ax bx +. epsilon
    && py >= fmin ay by -. epsilon
    && py <= fmax ay by +. epsilon

  let is_visible (verts : float array) p1_idx p2_idx poly_start_node poly_list =
    let curr_node = ref poly_start_node in
    let result = ref true in
    let loop = ref true in

    while !loop do
      let next_node = poly_list.PolygonList.next.(!curr_node) in
      let a = poly_list.vert_idx.(!curr_node) in
      let b = poly_list.vert_idx.(next_node) in

      let intersected =
        if a = p1_idx || a = p2_idx || b = p1_idx || b = p2_idx then false
        else
          intersects verts p1_idx p2_idx a b
          || on_segment verts p1_idx p2_idx a
          || on_segment verts p1_idx p2_idx b
      in

      if intersected then (
        if !verbose then Printf.printf "  Blocked by edge %d-%d\n%!" a b;
        result := false;
        loop := false)
      else if next_node = poly_start_node then loop := false
      else curr_node := next_node
    done;
    !result

  let find_bridge_point (verts : float array) hole_max_x_node outer_node
      poly_list =
    let h_idx = poly_list.PolygonList.vert_idx.(hole_max_x_node) in
    let hx = get_x verts h_idx in

    let best_candidate = ref (-1) in
    let min_dist = ref infinity in

    let curr_node = ref outer_node in
    let loop = ref true in

    while !loop do
      let v_idx = poly_list.PolygonList.vert_idx.(!curr_node) in
      let vx = get_x verts v_idx in

      (if vx >= hx then
         let d = dist_sq verts h_idx v_idx in
         if d < !min_dist then
           if is_visible verts h_idx v_idx outer_node poly_list then (
             min_dist := d;
             best_candidate := !curr_node));

      let next = poly_list.next.(!curr_node) in
      if next = outer_node then loop := false else curr_node := next
    done;

    if !best_candidate = -1 then outer_node else !best_candidate

  let merge_hole_into_outer (verts : float array) hole_start_node outer_node
      poly_list =
    let m_node = hole_start_node in
    let p_node = find_bridge_point verts m_node outer_node poly_list in
    let p_prime = PolygonList.duplicate_node poly_list p_node in
    let m_prime = PolygonList.duplicate_node poly_list m_node in

    let m_prev = poly_list.prev.(m_node) in
    let p_next = poly_list.next.(p_node) in

    (* P -> M *)
    poly_list.next.(p_node) <- m_node;
    poly_list.prev.(m_node) <- p_node;

    (* M_prev -> M' *)
    poly_list.next.(m_prev) <- m_prime;
    poly_list.prev.(m_prime) <- m_prev;

    (* M' -> P' *)
    poly_list.next.(m_prime) <- p_prime;
    poly_list.prev.(p_prime) <- m_prime;

    (* P' -> P_next *)
    poly_list.next.(p_prime) <- p_next;
    poly_list.prev.(p_next) <- p_prime;
    p_node

  let triangulate_dll (verts : float array) start_node poly_list
      total_active_nodes out_buffer out_offset =
    let open PolygonList in
    let next = poly_list.next in
    let prev = poly_list.prev in
    let vert_idx = poly_list.vert_idx in

    let reflex_nodes = Array.make total_active_nodes 0 in
    let reflex_count = ref 0 in
    let curr = ref start_node in

    for _ = 0 to total_active_nodes - 1 do
      let i = !curr in
      let vi_prev = vert_idx.(prev.(i)) in
      let vi_curr = vert_idx.(i) in
      let vi_next = vert_idx.(next.(i)) in
      if cross_product verts vi_prev vi_curr vi_next < 0.0 then (
        reflex_nodes.(!reflex_count) <- i;
        incr reflex_count);
      curr := next.(i)
    done;
    if !verbose then Printf.printf "Total reflex nodes: %d\n%!" !reflex_count;

    let spatial_idx =
      if total_active_nodes > 96 then
        SpatialIndex.create reflex_nodes !reflex_count poly_list.count verts
          vert_idx
      else SpatialIndex.empty_index
    in

    let active = Array.make poly_list.count true in

    let is_ear i =
      let node_prev = prev.(i) in
      let node_next = next.(i) in
      let vi_prev = vert_idx.(node_prev) in
      let vi_curr = vert_idx.(i) in
      let vi_next = vert_idx.(node_next) in

      if cross_product verts vi_prev vi_curr vi_next <= 0.0 then false
      else
        let ax, ay = (get_x verts vi_prev, get_y verts vi_prev) in
        let bx, by = (get_x verts vi_curr, get_y verts vi_curr) in
        let cx, cy = (get_x verts vi_next, get_y verts vi_next) in

        if spatial_idx.width = 0 then
          try
            for k = 0 to !reflex_count - 1 do
              let r_node = reflex_nodes.(k) in
              if
                r_node != node_prev && r_node != i && r_node != node_next
                && active.(r_node)
              then
                let vi_r = vert_idx.(r_node) in
                let rx, ry = (get_x verts vi_r, get_y verts vi_r) in
                if
                  (rx = ax && ry = ay)
                  || (rx = bx && ry = by)
                  || (rx = cx && ry = cy)
                then (
                  if !verbose then
                    Printf.printf "  Reflex node %d is a vertex of ear(?)\n%!"
                      vi_r)
                else if point_in_triangle verts vi_r vi_prev vi_curr vi_next
                then (
                  if !verbose then
                    Printf.printf "  Rejected: Reflex node %d inside\n%!" vi_r;
                  raise Exit)
            done;
            if !verbose then Printf.printf "  Accepted Ear!\n%!";
            true
          with Exit -> false
        else
          let min_tx = Geometry.fmin ax (Geometry.fmin bx cx) in
          let min_ty = Geometry.fmin ay (Geometry.fmin by cy) in
          let max_tx = Geometry.fmax ax (Geometry.fmax bx cx) in
          let max_ty = Geometry.fmax ay (Geometry.fmax by cy) in

          let start_x =
            int_of_float
              ((min_tx -. spatial_idx.min_x) *. spatial_idx.inv_cell_size)
          in
          let start_y =
            int_of_float
              ((min_ty -. spatial_idx.min_y) *. spatial_idx.inv_cell_size)
          in
          let end_x =
            int_of_float
              ((max_tx -. spatial_idx.min_x) *. spatial_idx.inv_cell_size)
          in
          let end_y =
            int_of_float
              ((max_ty -. spatial_idx.min_y) *. spatial_idx.inv_cell_size)
          in

          try
            let sy = if start_y > 0 then start_y else 0 in
            let ey =
              if spatial_idx.height - 1 < end_y then spatial_idx.height - 1
              else end_y
            in
            let sx = if start_x > 0 then start_x else 0 in
            let ex =
              if spatial_idx.width - 1 < end_x then spatial_idx.width - 1
              else end_x
            in

            for y = sy to ey do
              for x = sx to ex do
                let cell_idx = x + (y * spatial_idx.width) in
                let curr_node_ref = ref spatial_idx.grid_head.(cell_idx) in
                while !curr_node_ref != -1 do
                  let r_node = !curr_node_ref in
                  let is_valid =
                    if r_node = node_prev || r_node = i || r_node = node_next
                    then false
                    else if not active.(r_node) then false
                    else
                      let vi_r = vert_idx.(r_node) in
                      let rx, ry = (get_x verts vi_r, get_y verts vi_r) in
                      if
                        (rx = ax && ry = ay)
                        || (rx = bx && ry = by)
                        || (rx = cx && ry = cy)
                      then false
                      else point_in_triangle verts vi_r vi_prev vi_curr vi_next
                  in
                  if is_valid then raise Exit;
                  curr_node_ref := spatial_idx.next_in_bucket.(r_node)
                done
              done
            done;
            true
          with Exit -> false
    in

    let out_idx = ref out_offset in
    let count = ref total_active_nodes in
    curr := start_node;
    let iterations = ref 0 in
    (* Linear max_iter with stuck detection - much faster than O(N²) *)
    let max_iter = 10 * total_active_nodes in
    let since_last_progress = ref 0 in

    while !count > 2 && !iterations < max_iter do
      incr iterations;
      incr since_last_progress;
      (* If we've gone around 3 times without progress, forcibly remove a vertex *)
      if !since_last_progress > !count * 3 && !count > 2 then begin
        let i = !curr in
        let p = prev.(i) in
        let n = next.(i) in
        active.(i) <- false;
        next.(p) <- n;
        prev.(n) <- p;
        decr count;
        since_last_progress := 0;
        curr := p (* Advance to previous vertex *)
      end
      else begin
        if !verbose then
          Printf.printf "Loop iter %d count %d curr %d\n%!" !iterations !count
            !curr;
        let i = !curr in
        if active.(i) then begin
          let p = prev.(i) in
          let n = next.(i) in
          let vi_p = vert_idx.(p) in
          let vi_i = vert_idx.(i) in
          let vi_n = vert_idx.(n) in
          let cp = cross_product verts vi_p vi_i vi_n in

          if cp > epsilon then begin
            (* Convex vertex - check if it's a valid ear *)
            if is_ear i then begin
              if !out_idx + 2 >= Array.length out_buffer then
                Printf.printf
                  "ERROR: out_buffer overflow! out_idx=%d len=%d\n%!" !out_idx
                  (Array.length out_buffer);
              out_buffer.(!out_idx) <- vi_p;
              out_buffer.(!out_idx + 1) <- vi_i;
              out_buffer.(!out_idx + 2) <- vi_n;
              out_idx := !out_idx + 3;

              active.(i) <- false;
              next.(p) <- n;
              prev.(n) <- p;
              decr count;
              since_last_progress := 0;
              curr := p
            end
            else curr := n
          end
          else if abs_float cp < 1e-14 then begin
            (* 
               Truly collinear vertex (extremely small cross product).
               We use a much stricter threshold than epsilon to preserve thin
               triangles while still removing degenerate collinear points.
            *)
            active.(i) <- false;
            next.(p) <- n;
            prev.(n) <- p;
            decr count;
            since_last_progress := 0;
            curr := p
          end
          else
            (* Reflex vertex - skip *)
            curr := n
        end
        else curr := next.(i)
      end
    done;

    if !count > 2 then (
      Format.eprintf "Failure: %d remaining vertices after %d iterations@."
        !count max_iter;
      (* Diagnostic: Print the remaining polygon *)
      Format.eprintf "Remaining polygon vertices:@.";
      let start = !curr in
      let n = ref start in
      let first = ref true in
      while !first || !n <> start do
        first := false;
        let vi = vert_idx.(!n) in
        Format.eprintf "  [%d] node=%d vert=%d (%.10f, %.10f)@."
          (if !n = start then 0 else 1)
          !n vi (get_x verts vi) (get_y verts vi);
        n := next.(!n)
      done;
      let i = ref !curr in
      let stop = !curr in
      let loop = ref true in
      while !loop do
        let n = next.(!i) in
        let p = prev.(!i) in
        (* FIXED: Stop if 2 nodes remaining *)
        if n = !i || n = p then loop := false
        else (
          if !out_idx + 2 >= Array.length out_buffer then
            Printf.printf
              "ERROR: out_buffer overflow (2)! out_idx=%d len=%d\n%!" !out_idx
              (Array.length out_buffer);
          out_buffer.(!out_idx) <- vert_idx.(p);
          out_buffer.(!out_idx + 1) <- vert_idx.(!i);
          out_buffer.(!out_idx + 2) <- vert_idx.(n);
          out_idx := !out_idx + 3;
          next.(p) <- n;
          prev.(n) <- p;
          i := n;
          if !i = stop then loop := false)
      done);

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

      let curr_outer_node =
        ref (PolygonList.filter_points verts poly_list outer_start_node)
      in

      if num_holes > 0 then begin
        let processed_holes = Array.make num_holes (0, 0.0) in
        (* Correct Logic: Filter hole first, then find max_x node on active list *)
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

            (* Find max X on filtered ring *)
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
            processed_holes.(i) <- (!max_node, !max_x))
          else processed_holes.(i) <- (-1, neg_infinity)
        done;

        (* Sort holes by max X desc *)
        Array.sort (fun (_, x1) (_, x2) -> compare x2 x1) processed_holes;

        Array.iter
          (fun (bridge_node, _) ->
            if bridge_node <> -1 then
              curr_outer_node :=
                merge_hole_into_outer verts bridge_node !curr_outer_node
                  poly_list)
          processed_holes
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

  let triangulate_multi (verts : float array) polygons =
    Array.iter (validate_polygon verts) polygons;

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
    let offset = ref 0 in

    Array.iter
      (fun poly -> offset := triangulate_single verts poly out_buffer !offset)
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
