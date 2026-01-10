type t = {
  mbb_array : float array;
  point_indices : int array;
  level_offsets : int array;
  m : int;
  n : int;
  height : int;
  verts : float array;
  vert_idx : int array;
}

(** {2 Construction} *)

let get_level_info n m =
  let rec calc_counts count acc =
    if count <= 1 then List.rev (count :: acc)
    else
      let next_count = (count + m - 1) / m in
      calc_counts next_count (count :: acc)
  in
  let leaf_mbb_count = (n + m - 1) / m in
  let counts = calc_counts leaf_mbb_count [] in
  let offsets = Array.make (List.length counts) 0 in
  let current_offset = ref 0 in
  List.iteri
    (fun i count ->
      offsets.(i) <- !current_offset;
      current_offset := !current_offset + (count * 4))
    counts;
  (counts, offsets)

let get_x (verts : float array) i = Array.unsafe_get verts (i * 2)
let get_y (verts : float array) i = Array.unsafe_get verts ((i * 2) + 1)

let build ~verts ~vert_idx ~items ~min_x ~min_y ~max_x ~max_y =
  let m = 8 in
  let n = Array.length items in
  let scale_x =
    if max_x > min_x then float ((1 lsl 31) - 1) /. (max_x -. min_x) else 0.
  in
  let scale_y =
    if max_y > min_y then float ((1 lsl 31) - 1) /. (max_y -. min_y) else 0.
  in
  let sorted_indices = Array.copy items in
  let hilbert_keys =
    Array.init n (fun i ->
        let idx = sorted_indices.(i) in
        let v_idx = vert_idx.(idx) in
        let px = get_x verts v_idx in
        let py = get_y verts v_idx in
        let ix = int_of_float ((px -. min_x) *. scale_x) in
        let iy = int_of_float ((py -. min_y) *. scale_y) in
        Hilbert.transform ix iy)
  in

  let keyed_items =
    Array.init n (fun i -> (hilbert_keys.(i), sorted_indices.(i)))
  in
  Array.sort (fun (k1, _) (k2, _) -> compare k1 k2) keyed_items;
  let sorted_indices = Array.map snd keyed_items in

  let counts, offsets = get_level_info n m in
  let height = List.length counts in
  let total_mbbs = List.fold_left ( + ) 0 counts in
  let mbb_array = Array.make (total_mbbs * 4) 0. in
  let n0 = List.hd counts in
  for i = 0 to n0 - 1 do
    let start_point = i * m in
    let end_point = min (start_point + m) n in
    let xmin = ref infinity and ymin = ref infinity in
    let xmax = ref neg_infinity and ymax = ref neg_infinity in
    for j = start_point to end_point - 1 do
      let idx = sorted_indices.(j) in
      let v_idx = vert_idx.(idx) in
      let px = get_x verts v_idx in
      let py = get_y verts v_idx in
      if px < !xmin then xmin := px;
      if py < !ymin then ymin := py;
      if px > !xmax then xmax := px;
      if py > !ymax then ymax := py
    done;
    let base = offsets.(0) + (i * 4) in
    mbb_array.(base) <- !xmin;
    mbb_array.(base + 1) <- !ymin;
    mbb_array.(base + 2) <- !xmax;
    mbb_array.(base + 3) <- !ymax
  done;
  for h = 1 to height - 1 do
    let curr_count = List.nth counts h in
    let prev_count = List.nth counts (h - 1) in
    for i = 0 to curr_count - 1 do
      let start_child = i * m in
      let end_child = min (start_child + m) prev_count in
      let xmin = ref infinity and ymin = ref infinity in
      let xmax = ref neg_infinity and ymax = ref neg_infinity in
      for j = start_child to end_child - 1 do
        let base_prev = offsets.(h - 1) + (j * 4) in
        let cxmin = mbb_array.(base_prev) in
        let cymin = mbb_array.(base_prev + 1) in
        let cxmax = mbb_array.(base_prev + 2) in
        let cymax = mbb_array.(base_prev + 3) in
        if cxmin < !xmin then xmin := cxmin;
        if cymin < !ymin then ymin := cymin;
        if cxmax > !xmax then xmax := cxmax;
        if cymax > !ymax then ymax := cymax
      done;
      let base = offsets.(h) + (i * 4) in
      mbb_array.(base) <- !xmin;
      mbb_array.(base + 1) <- !ymin;
      mbb_array.(base + 2) <- !xmax;
      mbb_array.(base + 3) <- !ymax
    done
  done;
  {
    mbb_array;
    point_indices = sorted_indices;
    level_offsets = offsets;
    m;
    n;
    height;
    verts;
    vert_idx;
  }

(** {2 Search} *)

let intersects (q_xmin : float) (q_ymin : float) (q_xmax : float)
    (q_ymax : float) n_xmin n_ymin n_xmax n_ymax =
  not (q_xmin > n_xmax || q_xmax < n_xmin || q_ymin > n_ymax || q_ymax < n_ymin)

let lookup t q_xmin q_ymin q_xmax q_ymax callback =
  let rec search level index =
    let base = t.level_offsets.(level) + (index * 4) in
    let n_xmin = t.mbb_array.(base) in
    let n_ymin = t.mbb_array.(base + 1) in
    let n_xmax = t.mbb_array.(base + 2) in
    let n_ymax = t.mbb_array.(base + 3) in
    if intersects q_xmin q_ymin q_xmax q_ymax n_xmin n_ymin n_xmax n_ymax then
      if level = 0 then
        let start_point = index * t.m in
        let end_point = min (start_point + t.m) t.n in
        for j = start_point to end_point - 1 do
          let idx = t.point_indices.(j) in
          let v_idx = t.vert_idx.(idx) in
          let px = get_x t.verts v_idx in
          let py = get_y t.verts v_idx in
          if px >= q_xmin && px <= q_xmax && py >= q_ymin && py <= q_ymax then
            callback idx
        done
      else
        let prev_level_count =
          (t.level_offsets.(level) - t.level_offsets.(level - 1)) / 4
        in
        let start_child = index * t.m in
        let end_child = min (start_child + t.m) prev_level_count in
        for j = start_child to end_child - 1 do
          search (level - 1) j
        done
  in
  if t.n > 0 then search (t.height - 1) 0

(** {2 Inline Tests} *)

let%test_unit "static_r_tree_range_query" =
  let n = 1000 in
  let verts = Array.make (n * 2) 0.0 in
  let vert_idx = Array.init n (fun i -> i) in
  (* 1:1 mapping *)
  let items = Array.init n (fun i -> i) in
  for i = 0 to n - 1 do
    verts.(i * 2) <- Random.float 1000.;
    verts.((i * 2) + 1) <- Random.float 1000.
  done;
  let t =
    build ~verts ~vert_idx ~items ~min_x:0. ~min_y:0. ~max_x:1000. ~max_y:1000.
  in
  let q_xmin, q_ymin, q_xmax, q_ymax = (100., 100., 200., 200.) in
  let results = ref [] in
  lookup t q_xmin q_ymin q_xmax q_ymax (fun idx -> results := idx :: !results);
  let brute_force = ref [] in
  for i = 0 to n - 1 do
    let px = verts.(i * 2) in
    let py = verts.((i * 2) + 1) in
    if px >= q_xmin && px <= q_xmax && py >= q_ymin && py <= q_ymax then
      brute_force := i :: !brute_force
  done;
  let sort l = List.sort compare l in
  assert (sort !results = sort !brute_force)

let%test_unit "hilbert_closeness" =
  let test_size = 32 in
  let h_points = ref [] in
  for x = 0 to test_size - 1 do
    for y = 0 to test_size - 1 do
      h_points := (Hilbert.transform x y, x, y) :: !h_points
    done
  done;
  let sorted =
    List.sort (fun (h1, _, _) (h2, _, _) -> compare h1 h2) !h_points
  in
  let rec check = function
    | (_, x1, y1) :: ((_, x2, y2) :: _ as rest) ->
        let dist = abs (x1 - x2) + abs (y1 - y2) in
        if dist <> 1 then
          failwith
            (Printf.sprintf
               "Hilbert adjacency fail: p1=(%d,%d), p2=(%d,%d), dist=%d" x1 y1
               x2 y2 dist);
        check rest
    | _ -> ()
  in
  check sorted
