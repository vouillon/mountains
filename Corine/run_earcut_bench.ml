(* run_earcut_bench.ml - Benchmark earcut tiles fixture on OCaml triangulators *)

open Geometry_types

let tiles_bin_path = "/home/jerome/sources/earcut/bench/tiles-fixture.bin"

type decoded_poly = {
  z : int;
  verts : float array;
  poly : polygon;
  num_vertices : int;
  earcut_pts : Earcut.point list list;
}

let read_varint bytes pos =
  let res = ref 0 in
  let shift = ref 0 in
  let p = ref !pos in
  let continue = ref true in
  while !continue do
    let b = Char.code (Bytes.get bytes !p) in
    incr p;
    res := !res lor ((b land 0x7f) lsl !shift);
    shift := !shift + 7;
    if b land 0x80 = 0 then continue := false
  done;
  pos := !p;
  !res

let zig_zag_decode n = (n lsr 1) lxor -(n land 1)

let ring_area (ring : (float * float) list) =
  let pts = Array.of_list ring in
  let n = Array.length pts in
  let sum = ref 0.0 in
  let j = ref (n - 1) in
  for i = 0 to n - 1 do
    let xi, yi = pts.(i) in
    let xj, yj = pts.(!j) in
    sum := !sum +. ((xj -. xi) *. (yi +. yj));
    j := i
  done;
  !sum /. 2.0

let decode_mvt_rings geom =
  let rings = ref [] in
  let x = ref 0 in
  let y = ref 0 in
  let curr_ring = ref [] in
  let i = ref 0 in
  let len = Array.length geom in

  while !i < len do
    let cmd_int = geom.(!i) in
    incr i;
    let cmd = cmd_int land 0x7 in
    let count = cmd_int lsr 3 in

    if cmd = 1 then
      for _ = 1 to count do
        x := !x + zig_zag_decode geom.(!i);
        y := !y + zig_zag_decode geom.(!i + 1);
        i := !i + 2;
        if !curr_ring <> [] then rings := List.rev !curr_ring :: !rings;
        curr_ring := [ (float !x, float !y) ]
      done
    else if cmd = 2 then
      for _ = 1 to count do
        x := !x + zig_zag_decode geom.(!i);
        y := !y + zig_zag_decode geom.(!i + 1);
        i := !i + 2;
        curr_ring := (float !x, float !y) :: !curr_ring
      done
    else if cmd = 7 then (
      if !curr_ring <> [] then rings := List.rev !curr_ring :: !rings;
      curr_ring := [])
  done;

  if !curr_ring <> [] then rings := List.rev !curr_ring :: !rings;
  List.rev !rings

let build_poly_data z (rings : (float * float) list list) =
  let valid_rings =
    List.filter_map
      (fun r ->
        if List.length r < 3 then None
        else
          let a = ring_area r in
          if a = 0.0 then None else Some (r, a))
      rings
  in
  let raw_polys = ref [] in
  let curr_outer = ref None in
  let curr_holes = ref [] in

  let push_curr () =
    match !curr_outer with
    | None -> ()
    | Some outer ->
        raw_polys := (outer, List.rev !curr_holes) :: !raw_polys;
        curr_outer := None;
        curr_holes := []
  in

  List.iter
    (fun (r, a) ->
      if a > 0.0 then (
        push_curr ();
        curr_outer := Some r)
      else if !curr_outer <> None then curr_holes := r :: !curr_holes)
    valid_rings;
  push_curr ();

  List.map
    (fun (outer_pts, holes_pts) ->
      let total_pts =
        List.fold_left
          (fun acc h -> acc + List.length h)
          (List.length outer_pts) holes_pts
      in
      let flat_verts = Array.make (total_pts * 2) 0.0 in
      let idx = ref 0 in
      let fill_ring pts =
        let start = !idx in
        let len = List.length pts in
        List.iter
          (fun (x, y) ->
            flat_verts.(!idx * 2) <- x;
            flat_verts.((!idx * 2) + 1) <- y;
            incr idx)
          pts;
        { start; len }
      in
      let outer_ring = fill_ring outer_pts in
      let hole_rings = Array.of_list (List.map fill_ring holes_pts) in
      let earcut_pts =
        outer_pts :: holes_pts
        |> List.map (List.map (fun (x, y) -> { Earcut.x; y }))
      in
      {
        z;
        verts = flat_verts;
        poly = { outer = outer_ring; holes = hole_rings };
        num_vertices = total_pts;
        earcut_pts;
      })
    (List.rev !raw_polys)

let read_tiles_fixture path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let bytes = Bytes.create len in
  really_input ic bytes 0 len;
  close_in ic;

  let pos = ref 0 in
  let polys = ref [] in

  while !pos < len do
    let z = read_varint bytes pos in
    let features = read_varint bytes pos in
    for _ = 1 to features do
      let count = read_varint bytes pos in
      let geom = Array.make count 0 in
      for k = 0 to count - 1 do
        geom.(k) <- read_varint bytes pos
      done;
      let rings = decode_mvt_rings geom in
      let poly_datas = build_poly_data z rings in
      polys := List.rev_append poly_datas !polys
    done
  done;
  List.rev !polys

let run_our_triangulation polys =
  let total_tris = ref 0 in
  List.iter
    (fun d ->
      let tris =
        Polygon_triangulation.Triangulator.triangulate_multi d.verts
          [| d.poly |]
      in
      total_tris := !total_tris + Array.length tris)
    polys;
  !total_tris

let checksum_our_triangulation polys =
  let sum = ref 0l in
  List.iter
    (fun d ->
      let tris =
        Polygon_triangulation.Triangulator.triangulate_multi d.verts
          [| d.poly |]
      in
      for i = 0 to Array.length tris - 1 do
        let idx = Int32.of_int tris.(i) in
        let pos = Int32.of_int (i + 1) in
        let prod = Int32.mul idx pos in
        sum := Int32.add !sum prod
      done)
    polys;
  !sum

let run_ocaml_earcut polys =
  let total_tris = ref 0 in
  List.iter
    (fun d ->
      let tris = Earcut.triangulate d.earcut_pts in
      total_tris := !total_tris + List.length tris)
    polys;
  !total_tris

let time_execution fn num_runs =
  let times = ref [] in
  for _ = 1 to num_runs do
    let t0 = Unix.gettimeofday () in
    let _ = fn () in
    let t1 = Unix.gettimeofday () in
    times := ((t1 -. t0) *. 1000.0) :: !times
  done;
  let sorted = List.sort Float.compare !times in
  let median = List.nth sorted (num_runs / 2) in
  let lo = List.hd sorted in
  let hi = List.nth sorted (num_runs - 1) in
  (median, lo, hi)

let () =
  Polygon_triangulation.Triangulator.verbose := false;
  Printf.printf
    "===============================================================\n";
  Printf.printf "      EARCUT MVT TILES BENCHMARK - OCAML VS JS REFERENCE\n";
  Printf.printf
    "===============================================================\n\n";

  Printf.printf "Loading fixture from %s...\n%!" tiles_bin_path;
  let t0 = Unix.gettimeofday () in
  let polys = read_tiles_fixture tiles_bin_path in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Loaded %d polygons in %.2f s.\n\n" (List.length polys)
    (t1 -. t0);

  (* Stats *)
  let vert_counts =
    List.map (fun p -> p.num_vertices) polys |> List.sort compare
  in
  let arr_vc = Array.of_list vert_counts in
  let n_polys = Array.length arr_vc in
  let total_verts = Array.fold_left ( + ) 0 arr_vc in
  let with_holes =
    List.filter (fun p -> Array.length p.poly.holes > 0) polys |> List.length
  in
  let q pct =
    arr_vc.(min (n_polys - 1) (int_of_float (pct *. float n_polys)))
  in

  Printf.printf "FIXTURE STATS:\n";
  Printf.printf "  Polygons:   %d\n" n_polys;
  Printf.printf "  Vertices:   %d\n" total_verts;
  Printf.printf "  With holes: %.1f%%\n"
    (float with_holes *. 100.0 /. float n_polys);
  Printf.printf "  Verts/poly: median %d, p90 %d, p99 %d, max %d\n\n" (q 0.5)
    (q 0.9) (q 0.99)
    arr_vc.(n_polys - 1);

  Printf.printf "Running warm-up passes...\n%!";
  let _ = run_our_triangulation polys in
  let _ = run_ocaml_earcut polys in

  Printf.printf "Computing checksums...\n%!";
  let our_chk = checksum_our_triangulation polys in
  let total_indices = run_our_triangulation polys in
  let num_triangles = total_indices / 3 in

  Printf.printf "Benchmarking Polygon_triangulation (5 runs)...\n%!";
  let our_med, our_lo, our_hi =
    time_execution (fun () -> run_our_triangulation polys) 5
  in

  Printf.printf "Benchmarking OCaml Earcut (5 runs)...\n%!";
  let earcut_med, earcut_lo, earcut_hi =
    time_execution (fun () -> run_ocaml_earcut polys) 5
  in

  Printf.printf "\n%s\n" (String.make 75 '=');
  Printf.printf "BENCHMARK RESULTS:\n";
  Printf.printf "%s\n" (String.make 75 '=');

  Printf.printf
    "Engine:                      Polygon_triangulation     OCaml \
     Earcut           JS Earcut (ref)\n";
  Printf.printf "Triangles:                   %-25d %-22d 1,747,434\n"
    num_triangles
    (run_ocaml_earcut polys / 3);
  Printf.printf "Checksum:                    %-25ld %-22s 2816740945\n" our_chk
    "N/A (no holes)";
  Printf.printf "Median Time:                 %-22.1f ms %-20.1f ms ~503 ms\n"
    our_med earcut_med;
  Printf.printf
    "Range (min - max):           %.1f - %.1f ms           %.1f - %.1f \
     ms         501 - 513 ms\n"
    our_lo our_hi earcut_lo earcut_hi;
  Printf.printf "Throughput (polygons/sec):   %-25.0f %-22.0f ~238,141\n"
    (float n_polys /. (our_med /. 1000.0))
    (float n_polys /. (earcut_med /. 1000.0));
  Printf.printf "Throughput (M verts/sec):    %-25.2f %-22.2f ~3.8 M\n"
    (float total_verts /. (our_med /. 1000.0) /. 1e6)
    (float total_verts /. (earcut_med /. 1000.0) /. 1e6);

  Printf.printf "\n%s\n" (String.make 75 '-');
  Printf.printf "PER-ZOOM BREAKDOWN (Polygon_triangulation):\n";
  Printf.printf
    "  z   polygons      verts   med.v   time     %%time   polys/s\n";
  Printf.printf "%s\n" (String.make 75 '-');

  let by_zoom = Hashtbl.create 16 in
  List.iter
    (fun p ->
      let list = try Hashtbl.find by_zoom p.z with Not_found -> [] in
      Hashtbl.replace by_zoom p.z (p :: list))
    polys;

  let zooms =
    Hashtbl.fold (fun z _ acc -> z :: acc) by_zoom [] |> List.sort compare
  in

  List.iter
    (fun z ->
      let set = Hashtbl.find by_zoom z in
      let set_arr = Array.of_list set in
      let set_vc = Array.map (fun p -> p.num_vertices) set_arr in
      Array.sort compare set_vc;
      let set_total_verts = Array.fold_left ( + ) 0 set_vc in
      let set_med_v = set_vc.(Array.length set_vc / 2) in
      let z_med, _, _ =
        time_execution (fun () -> run_our_triangulation set) 3
      in
      let pct = z_med *. 100.0 /. our_med in
      let speed = float (List.length set) /. (z_med /. 1000.0) in
      Printf.printf " %2d  %8d  %9d  %5d  %5.0f ms  %5.1f%%  %8.0f\n" z
        (List.length set) set_total_verts set_med_v z_med pct speed)
    zooms;

  Printf.printf "%s\n" (String.make 75 '=')
