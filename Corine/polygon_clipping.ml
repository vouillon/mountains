(** Polygon Clipping to Rectangle in OCaml Algorithm: Sutherland-Hodgman

    Optimizations:
    - Zero-Allocation Intersection (No heap tuples)
    - Branch Prediction Optimization via Loop Unswitching
    - Unsafe Array Access
    - `[@@inline always]` to prevent float boxing
    - Manual Pre-allocation (No checks in add)
    - Unified Logic (clip_polygon calls clip_multipolygon)
    - Bulk Copying with Array.blit
    - Direct array processing in clip passes (No initial copy)
    - Shared Clipping Logic (Explicit Loop Unswitching) *)

open Geometry_types

module FloatBuffer = struct
  type t = { mutable data : float array; mutable count : int }

  let create capacity = { data = Array.make (capacity * 2) 0.0; count = 0 }

  (* Ensure the buffer can hold [required_verts] more vertices. 
     Resizes if necessary using a geometric growth strategy (doubling). *)
  let ensure_capacity t required_verts =
    let required_len = required_verts * 2 in
    let current_len = Array.length t.data in
    if current_len < required_len then begin
      let doubled = current_len * 2 in
      let new_capacity =
        if required_len > doubled then required_len else doubled
      in
      let new_data = Array.make new_capacity 0.0 in
      if t.count > 0 then Array.blit t.data 0 new_data 0 (t.count * 2);
      t.data <- new_data
    end

  (* Ultra-fast path: Unsafe write, No bounds check.
     SAFETY: Caller MUST ensure capacity via [ensure_capacity] before calling. 
     [@@inline always] required to pass floats in registers. *)
  let add t x y =
    let idx = t.count * 2 in
    Array.unsafe_set t.data idx x;
    Array.unsafe_set t.data (idx + 1) y;
    t.count <- t.count + 1
  [@@inline always]

  (* Bulk copy from a source float array. *)
  let blit_from t src src_start_vert num_verts =
    ensure_capacity t (t.count + num_verts);
    Array.blit src (src_start_vert * 2) t.data (t.count * 2) (num_verts * 2);
    t.count <- t.count + num_verts

  let clear t = t.count <- 0
  let to_array t = Array.sub t.data 0 (t.count * 2)
end

module Clipper = struct
  type edge = Left | Right | Bottom | Top

  let get_x (arr : float array) i = Array.unsafe_get arr (i * 2)
  [@@inline always]

  let get_y (arr : float array) i = Array.unsafe_get arr ((i * 2) + 1)
  [@@inline always]

  (* Generic clip pass with explicit loop unswitching.
     This manually duplicates the loop 4 times to ensure the 'edge' check
     and intersection math are fully constant-folded by the compiler. *)
  let clip_pass input_data input_start input_len output_buf region edge =
    (* Clear first so a resize in ensure_capacity does not copy stale data.
       Each input vertex emits at most 2 points (intersection + vertex), so
       [input_len * 2] is the exact worst case. *)
    FloatBuffer.clear output_buf;
    FloatBuffer.ensure_capacity output_buf (input_len * 2);

    if input_len > 0 then begin
      let prev_idx = input_start + input_len - 1 in
      let prev_x = ref (get_x input_data prev_idx) in
      let prev_y = ref (get_y input_data prev_idx) in

      match edge with
      | Left ->
          let min_x = region.min_x in
          let prev_in = ref (!prev_x >= min_x) in
          for idx = input_start to input_start + input_len - 1 do
            let cx = get_x input_data idx in
            let cy = get_y input_data idx in
            let cin = cx >= min_x in
            (if !prev_in <> cin then
               let t = (min_x -. !prev_x) /. (cx -. !prev_x) in
               FloatBuffer.add output_buf min_x
                 (!prev_y +. (t *. (cy -. !prev_y))));
            if cin then FloatBuffer.add output_buf cx cy;
            prev_x := cx;
            prev_y := cy;
            prev_in := cin
          done
      | Right ->
          let max_x = region.max_x in
          let prev_in = ref (!prev_x <= max_x) in
          for idx = input_start to input_start + input_len - 1 do
            let cx = get_x input_data idx in
            let cy = get_y input_data idx in
            let cin = cx <= max_x in
            (if !prev_in <> cin then
               let t = (max_x -. !prev_x) /. (cx -. !prev_x) in
               FloatBuffer.add output_buf max_x
                 (!prev_y +. (t *. (cy -. !prev_y))));
            if cin then FloatBuffer.add output_buf cx cy;
            prev_x := cx;
            prev_y := cy;
            prev_in := cin
          done
      | Bottom ->
          let min_y = region.min_y in
          let prev_in = ref (!prev_y >= min_y) in
          for idx = input_start to input_start + input_len - 1 do
            let cx = get_x input_data idx in
            let cy = get_y input_data idx in
            let cin = cy >= min_y in
            (if !prev_in <> cin then
               let t = (min_y -. !prev_y) /. (cy -. !prev_y) in
               FloatBuffer.add output_buf
                 (!prev_x +. (t *. (cx -. !prev_x)))
                 min_y);
            if cin then FloatBuffer.add output_buf cx cy;
            prev_x := cx;
            prev_y := cy;
            prev_in := cin
          done
      | Top ->
          let max_y = region.max_y in
          let prev_in = ref (!prev_y <= max_y) in
          for idx = input_start to input_start + input_len - 1 do
            let cx = get_x input_data idx in
            let cy = get_y input_data idx in
            let cin = cy <= max_y in
            (if !prev_in <> cin then
               let t = (max_y -. !prev_y) /. (cy -. !prev_y) in
               FloatBuffer.add output_buf
                 (!prev_x +. (t *. (cx -. !prev_x)))
                 max_y);
            if cin then FloatBuffer.add output_buf cx cy;
            prev_x := cx;
            prev_y := cy;
            prev_in := cin
          done
    end

  let clip_ring (verts : float array) ring region buf1 buf2 =
    (* Pipeline: Source -> Buf1 -> Buf2 -> Buf1 -> Buf2 *)
    clip_pass verts ring.start ring.len buf1 region Left;
    clip_pass buf1.data 0 buf1.count buf2 region Right;
    clip_pass buf2.data 0 buf2.count buf1 region Bottom;
    clip_pass buf1.data 0 buf1.count buf2 region Top;

    (* Note: sanitize removed - triangulation handles duplicates/collinear vertices *)
    buf2

  (* Internal core: clips one polygon and appends valid rings to final_buf *)
  let clip_polygon_to_buffer verts poly region buf_p1 buf_p2 final_buf =
    let res_outer = clip_ring verts poly.outer region buf_p1 buf_p2 in

    if res_outer.FloatBuffer.count < 3 then None
    else begin
      let outer_start = final_buf.FloatBuffer.count in
      let outer_len = res_outer.count in

      (* Bulk copy clipped outer ring to final buffer *)
      FloatBuffer.blit_from final_buf res_outer.data 0 outer_len;

      let valid_holes = ref [] in

      Array.iter
        (fun hole ->
          let res_hole = clip_ring verts hole region buf_p1 buf_p2 in
          if res_hole.count >= 3 then begin
            let h_start = final_buf.count in
            let h_len = res_hole.count in

            (* Bulk copy clipped hole *)
            FloatBuffer.blit_from final_buf res_hole.data 0 h_len;

            valid_holes := { start = h_start; len = h_len } :: !valid_holes
          end)
        poly.holes;

      Some
        {
          outer = { start = outer_start; len = outer_len };
          holes = Array.of_list (List.rev !valid_holes);
        }
    end

  (* Multi Polygon: Returns (new_vertices, clipped_polygons_array) *)
  let clip_multipolygon (verts : float array) polygons region =
    (* 1. Calculate max single ring size for work buffers *)
    let max_ring =
      Array.fold_left
        (fun acc p ->
          let h_max =
            Array.fold_left (fun hacc h -> max hacc h.len) 0 p.holes
          in
          max acc (max p.outer.len h_max))
        0 polygons
    in

    (* Work buffers: initial size is only an estimate to limit reallocation;
       ensure_capacity in each clip pass grows them as needed *)
    let work_cap = (max_ring * 2) + 32 in
    let buf_p1 = FloatBuffer.create work_cap in
    let buf_p2 = FloatBuffer.create work_cap in

    (* 2. Estimate final buffer size (Sum of all inputs) *)
    let total_input =
      Array.fold_left
        (fun acc p ->
          let h_sum = Array.fold_left (fun hacc h -> hacc + h.len) 0 p.holes in
          acc + p.outer.len + h_sum)
        0 polygons
    in

    let final_buf = FloatBuffer.create total_input in
    let results = ref [] in

    Array.iter
      (fun poly ->
        validate_polygon verts poly;
        match
          clip_polygon_to_buffer verts poly region buf_p1 buf_p2 final_buf
        with
        | None -> ()
        | Some p -> results := p :: !results)
      polygons;

    let new_verts = FloatBuffer.to_array final_buf in
    (new_verts, Array.of_list (List.rev !results))

  (* Single Polygon Wrapper: Returns (new_vertices, clipped_polygon) option *)
  let clip_polygon (verts : float array) poly region =
    let new_verts, result_polys = clip_multipolygon verts [| poly |] region in
    if Array.length result_polys = 0 then None
    else Some (new_verts, result_polys.(0))
end
