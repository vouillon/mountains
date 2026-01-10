(* check_cache_overlaps.ml *)
(* Compile: ocamlopt bigarray.cmxa check_cache_overlaps.ml -o check_cache_overlaps *)

type mark = Visiting | Visited

(* Custom exception to exit the recursion immediately when a cycle is found *)
exception Cycle_detected of string list

let find_cycle_or_order (pairs : (string * string * _) list) =
  (* 1. Build the Graph *)
  let graph = Hashtbl.create 16 in
  let add_edge u v =
    let neighbors = try Hashtbl.find graph u with Not_found -> [] in
    Hashtbl.replace graph u (v :: neighbors);
    (* Ensure destination nodes exist in the table even if they have no outgoing edges *)
    if not (Hashtbl.mem graph v) then Hashtbl.add graph v []
  in
  List.iter (fun (u, v, _) -> add_edge u v) pairs;

  (* 2. Prepare for DFS *)
  let marks = Hashtbl.create 16 in

  (* Helper: Extract the cycle from the current path stack *)
  let extract_cycle node path =
    let rec loop acc = function
      | [] -> acc (* Should not happen if logic is correct *)
      | h :: t ->
          if h = node then node :: h :: acc (* Close the loop *)
          else loop (h :: acc) t
    in
    loop [] path
  in

  let rec visit node path =
    match Hashtbl.find_opt marks node with
    | Some Visited -> ()
    | Some Visiting ->
        (* Cycle found! 'path' contains the history in reverse order (stack) *)
        let cycle = extract_cycle node path in
        raise (Cycle_detected cycle)
    | None ->
        Hashtbl.replace marks node Visiting;
        let neighbors = try Hashtbl.find graph node with Not_found -> [] in
        List.iter (fun neighbor -> visit neighbor (node :: path)) neighbors;
        Hashtbl.replace marks node Visited
  in

  (* 3. Iterate all nodes (handles disconnected components) *)
  let all_nodes = Hashtbl.fold (fun k _ acc -> k :: acc) graph [] in

  try List.iter (fun n -> visit n []) all_nodes
  with Cycle_detected cycle -> List.iter print_endline cycle

open Printf
open Bigarray

type point = { x : float; y : float }
type triangle = { p1 : point; p2 : point; p3 : point }

type tri_bbox = {
  t_min_x : float;
  t_max_x : float;
  t_min_y : float;
  t_max_y : float;
}

type feature = {
  id : int;
  z : float;
  r : float;
  g : float;
  b : float;
  min_x : float;
  max_x : float;
  min_y : float;
  max_y : float;
  area : float;
  geometry : (triangle * tri_bbox) list;
}

type overlap_detail = {
  small_id : int;
  small_r : float;
  small_g : float;
  small_b : float;
  small_area : float;
  inter_area : float;
  pct_small : float;
}

(* --- 1. Geometry Math --- *)

let cp a b c = ((b.x -. a.x) *. (c.y -. a.y)) -. ((b.y -. a.y) *. (c.x -. a.x))
let inside p a b = cp a b p >= 0.

let intersect_point a b c d =
  let a1 = b.y -. a.y in
  let b1 = a.x -. b.x in
  let c1 = (a1 *. a.x) +. (b1 *. a.y) in
  let a2 = d.y -. c.y in
  let b2 = c.x -. d.x in
  let c2 = (a2 *. c.x) +. (b2 *. c.y) in
  let det = (a1 *. b2) -. (a2 *. b1) in
  if abs_float det < 1e-9 then a
  else
    {
      x = ((b2 *. c1) -. (b1 *. c2)) /. det;
      y = ((a1 *. c2) -. (a2 *. c1)) /. det;
    }

let clip_polygon subject c1 c2 =
  let input = subject in
  if input = [] then []
  else
    let len = List.length input in
    let prev_idx i = if i = 0 then len - 1 else i - 1 in
    let input_arr = Array.of_list input in
    let output = ref [] in
    for i = 0 to len - 1 do
      let curr = input_arr.(i) in
      let prev = input_arr.(prev_idx i) in
      let curr_in = inside curr c1 c2 in
      let prev_in = inside prev c1 c2 in
      if curr_in then (
        if not prev_in then output := intersect_point prev curr c1 c2 :: !output;
        output := curr :: !output)
      else if prev_in then output := intersect_point prev curr c1 c2 :: !output
    done;
    List.rev !output

let get_triangle_intersection_area t1 t2 =
  let poly = [ t1.p1; t1.p2; t1.p3 ] in
  let poly = clip_polygon poly t2.p1 t2.p2 in
  let poly = clip_polygon poly t2.p2 t2.p3 in
  let poly = clip_polygon poly t2.p3 t2.p1 in
  match poly with
  | [] -> 0.
  | _ ->
      let arr = Array.of_list poly in
      let n = Array.length arr in
      let sum = ref 0. in
      for i = 0 to n - 1 do
        let j = (i + 1) mod n in
        sum := !sum +. ((arr.(i).x *. arr.(j).y) -. (arr.(j).x *. arr.(i).y))
      done;
      0.5 *. abs_float !sum

(* --- 2. Cache Reading --- *)
let read_cache filename =
  if not (Sys.file_exists filename) then (
    printf "Error: %s not found.\n" filename;
    exit 1);
  let ic = open_in_bin filename in
  let _ = (input_value ic : float) in
  let _ = (input_value ic : float) in
  let _ = (input_value ic : float) in
  let _ = (input_value ic : float) in
  let v_count : int = input_value ic in
  let data : (float, float32_elt, c_layout) Array1.t = input_value ic in
  close_in ic;
  (v_count, data)

(* --- 3. Feature Extraction --- *)
let get_tri_bbox t =
  {
    t_min_x = min t.p1.x (min t.p2.x t.p3.x);
    t_max_x = max t.p1.x (max t.p2.x t.p3.x);
    t_min_y = min t.p1.y (min t.p2.y t.p3.y);
    t_max_y = max t.p1.y (max t.p2.y t.p3.y);
  }

let extract_features v_count data =
  printf "Reconstructing geometry...\n%!";
  let features = ref [] in
  let cur_tris = ref [] in
  let cur_z = ref nan in
  let cur_min_x, cur_max_x, cur_min_y, cur_max_y =
    (ref infinity, ref neg_infinity, ref infinity, ref neg_infinity)
  in
  let cur_r, cur_g, cur_b = (ref 0., ref 0., ref 0.) in
  let pt_buf = ref [] in

  let flush_feature () =
    if (not (classify_float !cur_z = FP_nan)) && !cur_tris <> [] then
      features :=
        {
          id = List.length !features;
          z = !cur_z;
          r = !cur_r;
          g = !cur_g;
          b = !cur_b;
          min_x = !cur_min_x;
          max_x = !cur_max_x;
          min_y = !cur_min_y;
          max_y = !cur_max_y;
          area = 0.0;
          (* Placeholder *)
          geometry = !cur_tris;
        }
        :: !features
  in

  for i = 0 to v_count - 1 do
    let base = i * 6 in
    let x, y, z =
      ( Array1.get data (base + 0),
        Array1.get data (base + 1),
        Array1.get data (base + 2) )
    in

    if z <> !cur_z then (
      flush_feature ();
      cur_z := z;
      cur_tris := [];
      pt_buf := [];
      cur_min_x := x;
      cur_max_x := x;
      cur_min_y := y;
      cur_max_y := y;
      cur_r := Array1.get data (base + 3);
      cur_g := Array1.get data (base + 4);
      cur_b := Array1.get data (base + 5));
    if x < !cur_min_x then cur_min_x := x;
    if x > !cur_max_x then cur_max_x := x;
    if y < !cur_min_y then cur_min_y := y;
    if y > !cur_max_y then cur_max_y := y;

    pt_buf := { x; y } :: !pt_buf;
    match !pt_buf with
    | [ p3; p2; p1 ] ->
        let t = { p1; p2; p3 } in
        cur_tris := (t, get_tri_bbox t) :: !cur_tris;
        pt_buf := []
    | _ -> ()
  done;
  flush_feature ();
  List.rev !features

(* --- 4. Logic --- *)
let calculate_feature_area f =
  List.fold_left
    (fun acc (t, _) ->
      acc
      +. 0.5
         *. abs_float
              (((t.p2.x -. t.p1.x) *. (t.p3.y -. t.p1.y))
              -. ((t.p3.x -. t.p1.x) *. (t.p2.y -. t.p1.y))))
    0. f.geometry

let bboxes_intersect a b =
  not
    (a.max_x <= b.min_x || a.min_x >= b.max_x || a.max_y <= b.min_y
   || a.min_y >= b.max_y)

let calculate_overlap_area f1 f2 =
  let t2_arr =
    Array.of_list
      (List.sort
         (fun (_, b1) (_, b2) -> compare b1.t_min_x b2.t_min_x)
         f2.geometry)
  in
  let n2 = Array.length t2_arr in
  let total_overlap = ref 0. in

  List.iter
    (fun (t1, b1) ->
      let rec check_t2 i =
        if i >= n2 then ()
        else
          let t2, b2 = t2_arr.(i) in
          if b2.t_min_x >= b1.t_max_x then ()
          else (
            if
              b2.t_max_x > b1.t_min_x && b2.t_max_y > b1.t_min_y
              && b2.t_min_y < b1.t_max_y
            then
              total_overlap :=
                !total_overlap +. get_triangle_intersection_area t1 t2;
            check_t2 (i + 1))
      in
      check_t2 0)
    f1.geometry;
  !total_overlap

(* --- Main --- *)
let () =
  let count, data = read_cache "clc.cache" in
  let features = extract_features count data in
  if features = [] then exit 0;

  printf "Calculating TRUE geometric areas...\n%!";
  (* Correct: We calculate area first, BEFORE identifying background *)
  let features_with_area =
    List.map (fun f -> { f with area = calculate_feature_area f }) features
  in

  (* Correct: Sort by True Area Descending *)
  let sorted_area =
    List.sort (fun a b -> compare b.area a.area) features_with_area
  in
  let bg = List.hd sorted_area in
  printf "Skipping Background Layer ID %d (Z=%.2f) Area=%.2e\n" bg.id bg.z
    bg.area;

  let active = List.filter (fun f -> f.id <> bg.id) features_with_area in

  (* Sort by X for Sweep-Line *)
  let sorted = List.sort (fun a b -> compare a.min_x b.min_x) active in
  printf "Checking %d features for SIGNIFICANT overlaps...\n%!"
    (List.length sorted);

  let results = Hashtbl.create 100 in

  let rec check lst =
    match lst with
    | [] -> ()
    | head :: tail ->
        let rec walk candidates =
          match candidates with
          | [] -> ()
          | other :: rest ->
              if other.min_x >= head.max_x then ()
              else (
                (if bboxes_intersect head other then
                   let overlap = calculate_overlap_area head other in
                   let pct_h = overlap /. head.area *. 100. in
                   let pct_o = overlap /. other.area *. 100. in

                   if overlap > 1.0 && (pct_h >= 1.0 || pct_o >= 1.0) then
                     let large, small, _, pct_s =
                       if head.area >= other.area then
                         (head, other, pct_h, pct_o)
                       else (other, head, pct_o, pct_h)
                     in

                     let detail =
                       {
                         small_id = small.id;
                         small_r = small.r;
                         small_g = small.g;
                         small_b = small.b;
                         small_area = small.area;
                         inter_area = overlap;
                         pct_small = pct_s;
                       }
                     in

                     let cur_list =
                       match Hashtbl.find_opt results large.id with
                       | Some (_, l) -> l
                       | None -> []
                     in
                     Hashtbl.replace results large.id (large, detail :: cur_list));
                walk rest)
        in
        walk tail;
        check tail
  in

  let t0 = Sys.time () in
  check sorted;
  let duration = Sys.time () -. t0 in

  printf "\n====== OVERLAP REPORT (Grouped by Large Polygon) ======\n";
  printf "Check Time: %.4fs\n\n" duration;

  let problem_features = Hashtbl.fold (fun _ v acc -> v :: acc) results [] in
  let sorted_problems =
    List.sort
      (fun (_, l1) (_, l2) -> compare (List.length l2) (List.length l1))
      problem_features
  in

  if sorted_problems = [] then
    printf "SUCCESS: No significant overlaps found.\n"
  else (
    List.iter
      (fun (large, overlaps) ->
        let n = List.length overlaps in
        printf "FEATURE ID %d (Area: %.0f, Color: %.2f,%.2f,%.2f)\n" large.id
          large.area large.r large.g large.b;
        printf "  -> Overlapped by %d smaller features.\n" n;

        let total_overlap_area =
          List.fold_left (fun acc d -> acc +. d.inter_area) 0. overlaps
        in
        printf "  -> Total Overlap Area: %.2f (%.2f%% of self)\n"
          total_overlap_area
          (total_overlap_area /. large.area *. 100.);

        let worst =
          List.sort (fun a b -> compare b.pct_small a.pct_small) overlaps
        in
        List.iteri
          (fun i d ->
            if i < 5 then
              printf
                "     %d. ID %d (Area: %.0f, Color: %.2f,%.2f,%.2f) - Overlap: \
                 %.2f (%.1f%% of small)\n"
                (i + 1) d.small_id d.small_area d.small_r d.small_g d.small_b
                d.inter_area d.pct_small)
          worst;
        if n > 5 then printf "     ... and %d more.\n" (n - 5);
        printf "\n")
      sorted_problems;

    printf "\n====== COLOR HIERARCHY ANALYSIS ======\n";
    let color_str r g b = Printf.sprintf "(%.2f, %.2f, %.2f)" r g b in
    let relations = Hashtbl.create 20 in

    List.iter
      (fun (large, overlaps) ->
        let large_c = color_str large.r large.g large.b in
        List.iter
          (fun d ->
            let small_c = color_str d.small_r d.small_g d.small_b in
            if large_c <> small_c then
              let key = (large_c, small_c) in
              let count =
                match Hashtbl.find_opt relations key with
                | Some c -> c
                | None -> 0
              in
              Hashtbl.replace relations key (count + 1))
          overlaps)
      sorted_problems;

    let rel_list =
      Hashtbl.fold (fun (l, s) c acc -> (l, s, c) :: acc) relations []
    in
    let sorted_rels =
      List.sort (fun (_, _, c1) (_, _, c2) -> compare c2 c1) rel_list
    in

    find_cycle_or_order sorted_rels;

    if sorted_rels = [] then printf "No inter-color containment found.\n"
    else (
      printf "Observed Containment (Large Color -> Small Color):\n";
      List.iter
        (fun (l, s, c) -> printf "  %s contains %s : %d times\n" l s c)
        sorted_rels))
