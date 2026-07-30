(* osm_fetch.ml - Fetch water polygons from OpenStreetMap via Overpass API *)

(* 
   Query water polygons using Overpass API, parse GeoJSON response.
   Maps OSM water=* tags to CLC codes for integration with CLC tile system.
   
   Usage:
     let water_polys = Osm_fetch.fetch_water_polygons ~min_lat ~min_lon ~max_lat ~max_lon in
   
   CLC Code Mapping:
     water=lake, reservoir, pond -> 512 (Water bodies)
     water=river, canal          -> 511 (Water courses)
     water=lagoon                -> 521 (Coastal lagoons)
     (default)                   -> 512 (Water bodies)
*)

(* --- Types --- *)

type point = { x : float; y : float }
type ring = point list
type polygon = ring list (* outer ring + holes *)
type water_feature = { id : int; clc_code : int; polygons : polygon list }

(* --- CLC Code Mapping --- *)

let water_tag_to_clc_code water_tag =
  match water_tag with
  | Some "lake" | Some "reservoir" | Some "pond" | Some "oxbow" -> 512
  | Some "river" | Some "canal" | Some "stream" -> 511
  | Some "lagoon" -> 521
  | Some "sea" | Some "ocean" -> 523
  | _ -> 512 (* Default: Water bodies *)

(* --- Overpass Query --- *)

(* The explicit server-side timeout stays below curl's --max-time (120s), so
   a slow query fails fast with a server "remark" (retried below) instead of
   being repeatedly killed by curl at the default 180s server budget *)
let build_overpass_query ~min_lat ~min_lon ~max_lat ~max_lon =
  Printf.sprintf
    {|[out:json][timeout:110][bbox:%f,%f,%f,%f];(wr[natural=water];wr[water];);(._;>>;);out body;|}
    min_lat min_lon max_lat max_lon

(* --- HTTP Fetch via curl with retry --- *)

let fetch_overpass_data_once query =
  (* Use curl to POST to Overpass API with 'data=' prefix *)
  let url = "https://overpass-api.de/api/interpreter" in
  (* Replace newlines with spaces for cleaner query *)
  let clean_query = String.concat " " (String.split_on_char '\n' query) in
  let cmd =
    Printf.sprintf
      "curl -f -s --connect-timeout 30 --max-time 120 -X POST -d 'data=%s' '%s'"
      clean_query url
  in

  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 65536 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Some (Buffer.contents buf)
  | Unix.WEXITED code ->
      Printf.eprintf "curl failed with exit code %d\n%!" code;
      None
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      Printf.eprintf "curl was killed by signal\n%!";
      None

(* Overpass reports server-side failures (query timeout, memory exhaustion)
   as HTTP 200 JSON with a top-level "remark" field and truncated or empty
   output; such responses must be retried, not parsed as complete data *)
let response_error response =
  if String.length response < 1 || response.[0] <> '{' then
    Some "response is not JSON"
  else
    match Yojson.Safe.from_string response with
    | exception _ -> Some "malformed JSON response"
    | json -> (
        match Yojson.Safe.Util.member "remark" json with
        | `String remark -> Some ("server remark: " ^ remark)
        | _ -> None)

let fetch_overpass_data ?(max_retries = 10) query =
  let rec retry attempt =
    Printf.printf "Overpass API request (attempt %d/%d)...\n%!" attempt
      max_retries;
    let result =
      match fetch_overpass_data_once query with
      | None -> Error "curl failed"
      | Some response -> (
          match response_error response with
          | None -> Ok response
          | Some err -> Error err)
    in
    match result with
    | Ok response -> Some response
    | Error err when attempt < max_retries ->
        let delay = 2.0 *. (1.5 ** float (attempt - 1)) in
        Printf.printf "Request failed (%s), retrying in %.0fs...\n%!" err delay;
        Unix.sleepf delay;
        retry (attempt + 1)
    | Error err ->
        Printf.eprintf "Overpass request failed after %d attempts: %s\n%!"
          max_retries err;
        None
  in
  retry 1

(* --- Parsing with Node ID-based Chaining --- *)

(* Types for intermediate representation *)
type way_info = { node_ids : int array; tags : Yojson.Safe.t }
type relation_member = { member_type : string; member_ref : int; role : string }

type relation_info = {
  rel_id : int;
  members : relation_member list;
  tags : Yojson.Safe.t;
}

(* Parse Overpass JSON response with recursive query format *)
let parse_overpass_elements json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let elements = json |> member "elements" |> to_list in

  (* Build hash tables for nodes, ways, relations *)
  let nodes : (int, float * float) Hashtbl.t = Hashtbl.create 100000 in
  let ways : (int, way_info) Hashtbl.t = Hashtbl.create 10000 in
  let relations : (int, relation_info) Hashtbl.t = Hashtbl.create 1000 in

  (* First pass: populate hash tables *)
  List.iter
    (fun elem ->
      let elem_type = elem |> member "type" |> to_string in
      let elem_id = elem |> member "id" |> to_int in
      (* Member ways/relations pulled in by the recursion may be untagged;
         default to an empty object so tag lookups return None instead of
         raising on `Null *)
      let elem_tags () =
        match elem |> member "tags" with `Null -> `Assoc [] | t -> t
      in
      match elem_type with
      | "node" ->
          let lat = elem |> member "lat" |> to_number in
          let lon = elem |> member "lon" |> to_number in
          Hashtbl.add nodes elem_id (lon, lat)
      | "way" ->
          let node_ids =
            elem |> member "nodes" |> to_list |> List.map to_int
            |> Array.of_list
          in
          let tags = elem_tags () in
          Hashtbl.add ways elem_id { node_ids; tags }
      | "relation" ->
          let members =
            elem |> member "members" |> to_list
            |> List.map (fun m ->
                {
                  member_type = m |> member "type" |> to_string;
                  member_ref = m |> member "ref" |> to_int;
                  role = m |> member "role" |> to_string;
                })
          in
          let tags = elem_tags () in
          Hashtbl.add relations elem_id { rel_id = elem_id; members; tags }
      | _ -> ())
    elements;

  (* Helper: get way endpoints (first and last node IDs) *)
  let way_endpoints way =
    let n = Array.length way.node_ids in
    if n < 2 then None else Some (way.node_ids.(0), way.node_ids.(n - 1))
  in

  (* Helper: convert node IDs to a coordinate list. If any node is missing
     from the response, drop the whole ring with a warning (consistent with
     the missing-ways handling below): silently deleting vertices would
     replace part of the shoreline with a chord across the polygon. *)
  let nodes_to_points owner_id node_ids =
    let missing = ref 0 in
    let pts =
      Array.fold_right
        (fun nid acc ->
          match Hashtbl.find_opt nodes nid with
          | Some (lon, lat) -> { x = lon; y = lat } :: acc
          | None ->
              incr missing;
              acc)
        node_ids []
    in
    if !missing = 0 then Some pts
    else (
      Printf.printf
        "Warning: Dropping ring in %d: %d nodes not in dataset (outside bbox?)\n\
         %!"
        owner_id !missing;
      None)
  in

  (* Chain ways by node ID into closed rings *)
  let chain_ways_by_id rel_id way_ids =
    if way_ids = [] then []
    else
      (* Check how many ways are actually available *)
      let missing_ways = ref [] in
      let available_ways = ref [] in
      List.iter
        (fun wid ->
          if Hashtbl.mem ways wid then available_ways := wid :: !available_ways
          else missing_ways := wid :: !missing_ways)
        way_ids;

      (* If we're missing ways, the chain likely can't close - skip with warning *)
      if !missing_ways <> [] then (
        Printf.printf
          "Warning: Relation %d references %d ways not in dataset (outside \
           bbox?): [%s]\n\
           %!"
          rel_id
          (List.length !missing_ways)
          (String.concat ", " (List.map string_of_int (List.rev !missing_ways)));
        [])
      else if List.length !available_ways < 1 then []
      else
        (* Build endpoint lookup: node_id -> list of (way_id, is_start) *)
        let endpoint_map : (int, (int * bool) list) Hashtbl.t =
          Hashtbl.create 256
        in
        List.iter
          (fun wid ->
            match Hashtbl.find_opt ways wid with
            | None -> ()
            | Some w -> (
                match way_endpoints w with
                | None -> ()
                | Some (first, last) ->
                    let add_endpoint nid is_start =
                      let existing =
                        Option.value ~default:[]
                          (Hashtbl.find_opt endpoint_map nid)
                      in
                      Hashtbl.replace endpoint_map nid
                        ((wid, is_start) :: existing)
                    in
                    add_endpoint first true;
                    if first <> last then add_endpoint last false))
          way_ids;

        let used = Hashtbl.create 256 in
        let rings = ref [] in

        (* Start a chain from each unused way *)
        List.iter
          (fun start_wid ->
            if not (Hashtbl.mem used start_wid) then
              match Hashtbl.find_opt ways start_wid with
              | None -> ()
              | Some start_way ->
                  Hashtbl.add used start_wid true;
                  let chain = ref [ start_way.node_ids ] in
                  let chain_way_ids = ref [ start_wid ] in

                  (* Current endpoint we're trying to extend from *)
                  let curr_end =
                    ref (snd (Option.get (way_endpoints start_way)))
                  in
                  let chain_start =
                    ref (fst (Option.get (way_endpoints start_way)))
                  in

                  let changed = ref true in
                  while !changed && !curr_end <> !chain_start do
                    changed := false;

                    (* Try to extend forward from curr_end *)
                    (match Hashtbl.find_opt endpoint_map !curr_end with
                    | None -> ()
                    | Some candidates ->
                        List.iter
                          (fun (wid, is_start) ->
                            if (not !changed) && not (Hashtbl.mem used wid) then
                              match Hashtbl.find_opt ways wid with
                              | None -> ()
                              | Some w ->
                                  let first, last =
                                    Option.get (way_endpoints w)
                                  in
                                  if is_start && first = !curr_end then (
                                    (* Append way as-is (skip first node to avoid dup) *)
                                    chain :=
                                      !chain
                                      @ [
                                          Array.sub w.node_ids 1
                                            (Array.length w.node_ids - 1);
                                        ];
                                    chain_way_ids := !chain_way_ids @ [ wid ];
                                    Hashtbl.add used wid true;
                                    curr_end := last;
                                    changed := true)
                                  else if (not is_start) && last = !curr_end
                                  then (
                                    (* Append reversed way *)
                                    let rev =
                                      Array.of_list
                                        (List.rev (Array.to_list w.node_ids))
                                    in
                                    chain :=
                                      !chain
                                      @ [
                                          Array.sub rev 1 (Array.length rev - 1);
                                        ];
                                    chain_way_ids := !chain_way_ids @ [ wid ];
                                    Hashtbl.add used wid true;
                                    curr_end := first;
                                    changed := true))
                          candidates);

                    (* Try to extend backward from chain_start *)
                    if not !changed then
                      match Hashtbl.find_opt endpoint_map !chain_start with
                      | None -> ()
                      | Some candidates ->
                          List.iter
                            (fun (wid, is_start) ->
                              if (not !changed) && not (Hashtbl.mem used wid)
                              then
                                match Hashtbl.find_opt ways wid with
                                | None -> ()
                                | Some w ->
                                    let first, last =
                                      Option.get (way_endpoints w)
                                    in
                                    if (not is_start) && last = !chain_start
                                    then (
                                      (* Prepend way as-is (skip last node to avoid dup) *)
                                      let trimmed =
                                        Array.sub w.node_ids 0
                                          (Array.length w.node_ids - 1)
                                      in
                                      chain := [ trimmed ] @ !chain;
                                      chain_way_ids := wid :: !chain_way_ids;
                                      Hashtbl.add used wid true;
                                      chain_start := first;
                                      changed := true)
                                    else if is_start && first = !chain_start
                                    then (
                                      (* Prepend reversed way (skip last node which is now start) *)
                                      let rev =
                                        Array.of_list
                                          (List.rev (Array.to_list w.node_ids))
                                      in
                                      let trimmed =
                                        Array.sub rev 0 (Array.length rev - 1)
                                      in
                                      chain := [ trimmed ] @ !chain;
                                      chain_way_ids := wid :: !chain_way_ids;
                                      Hashtbl.add used wid true;
                                      chain_start := last;
                                      changed := true))
                            candidates
                  done;

                  (* Check if chain is closed *)
                  if !curr_end = !chain_start then
                    (* Combine all node arrays into one ring *)
                    let all_nodes =
                      Array.concat (List.map (fun arr -> arr) !chain)
                    in
                    rings := all_nodes :: !rings
                  else
                    Printf.printf
                      "Warning: Unclosed chain in relation %d, ways: [%s]\n%!"
                      rel_id
                      (String.concat ", "
                         (List.map string_of_int !chain_way_ids)))
          way_ids;

        !rings
  in

  (* Point-in-polygon test *)
  let point_in_ring pt ring =
    let n = List.length ring in
    if n < 3 then false
    else
      let ring_arr = Array.of_list ring in
      let inside = ref false in
      let j = ref (n - 1) in
      for i = 0 to n - 1 do
        let pi = ring_arr.(i) in
        let pj = ring_arr.(!j) in
        if
          pi.y > pt.y <> (pj.y > pt.y)
          && pt.x < ((pj.x -. pi.x) *. (pt.y -. pi.y) /. (pj.y -. pi.y)) +. pi.x
        then inside := not !inside;
        j := i
      done;
      !inside
  in

  (* Process each relation *)
  let features =
    Hashtbl.fold
      (fun _rel_id rel acc ->
        let water_tag = rel.tags |> member "water" |> to_option to_string in
        let natural_tag = rel.tags |> member "natural" |> to_option to_string in
        (* Only process water features *)
        if natural_tag <> Some "water" && water_tag = None then acc
        else
          let clc_code = water_tag_to_clc_code water_tag in
          let outer_way_ids =
            List.filter_map
              (fun m ->
                if m.member_type = "way" && m.role = "outer" then
                  Some m.member_ref
                else None)
              rel.members
          in
          let inner_way_ids =
            List.filter_map
              (fun m ->
                if m.member_type = "way" && m.role = "inner" then
                  Some m.member_ref
                else None)
              rel.members
          in

          let outer_rings = chain_ways_by_id rel.rel_id outer_way_ids in
          let inner_rings = chain_ways_by_id rel.rel_id inner_way_ids in

          (* Merge touching inner rings (OSM specific extension to Simple Features) *)
          let inner_rings =
            if inner_rings = [] then []
            else
              let rec merge_all rings =
                let n = List.length rings in
                let rings_arr = Array.of_list rings in
                let merged = ref false in
                let result = ref [] in
                let used = Array.make n false in

                let is_ccw_nodes node_ids =
                  let n = Array.length node_ids in
                  if n < 3 then true
                  else
                    let acc = ref 0.0 in
                    match Hashtbl.find_opt nodes node_ids.(0) with
                    | None -> true
                    | Some (ref_x, ref_y) ->
                        for i = 0 to n - 1 do
                          let n1 = node_ids.(i) in
                          let n2 =
                            if i = n - 1 then node_ids.(0) else node_ids.(i + 1)
                          in
                          match
                            ( Hashtbl.find_opt nodes n1,
                              Hashtbl.find_opt nodes n2 )
                          with
                          | Some (x1, y1), Some (x2, y2) ->
                              let p1x, p1y = (x1 -. ref_x, y1 -. ref_y) in
                              let p2x, p2y = (x2 -. ref_x, y2 -. ref_y) in
                              acc := !acc +. ((p1x *. p2y) -. (p2x *. p1y))
                          | _ -> ()
                        done;
                        !acc > 0.0
                in

                let rotate_to_nid nid arr =
                  let n = Array.length arr in
                  if n < 2 then arr
                  else
                    let rec find k =
                      if k >= n - 1 then None
                      else if arr.(k) = nid then Some k
                      else find (k + 1)
                    in
                    match find 0 with
                    | None -> arr
                    | Some idx ->
                        let res = Array.make n 0 in
                        for i = 0 to n - 2 do
                          res.(i) <- arr.((idx + i) mod (n - 1))
                        done;
                        res.(n - 1) <- res.(0);
                        res
                in

                for i = 0 to n - 1 do
                  if not used.(i) then (
                    let current = ref (Array.to_list rings_arr.(i)) in
                    used.(i) <- true;
                    let found_match = ref true in
                    while !found_match do
                      found_match := false;
                      for j = 0 to n - 1 do
                        if (not !found_match) && not used.(j) then (
                          let other = rings_arr.(j) in
                          let nodes_in_current = Hashtbl.create 512 in
                          List.iter
                            (fun nid ->
                              Hashtbl.replace nodes_in_current nid true)
                            !current;

                          let r1_arr = Array.of_list !current in
                          let r1_ccw = is_ccw_nodes r1_arr in
                          let r2_ccw = is_ccw_nodes other in
                          let other_norm =
                            if r1_ccw <> r2_ccw then
                              Array.of_list (List.rev (Array.to_list other))
                            else other
                          in

                          let best_nid = ref None in
                          let best_common = ref 0 in
                          let best_is_dup = ref false in

                          for k = 0 to Array.length other_norm - 1 do
                            let nid = other_norm.(k) in
                            if Hashtbl.mem nodes_in_current nid then
                              let r1_tmp = rotate_to_nid nid r1_arr in
                              let r2_tmp = rotate_to_nid nid other_norm in

                              (* 1. Check for Duplicate Ring (Forward match) *)
                              let is_duplicate =
                                Array.length r1_tmp = Array.length r2_tmp
                                &&
                                let same = ref true in
                                for i = 0 to Array.length r1_tmp - 1 do
                                  if r1_tmp.(i) <> r2_tmp.(i) then same := false
                                done;
                                !same
                              in

                              if is_duplicate then (
                                best_common := Array.length r1_tmp;
                                best_nid := Some nid;
                                best_is_dup := true)
                              else if not !best_is_dup then (
                                (* 2. Check for Shared Edges (Reverse match) *)
                                let r1_rev_tmp =
                                  Array.of_list
                                    (List.rev (Array.to_list r1_tmp))
                                in
                                let common = ref 0 in
                                let limit =
                                  min (Array.length r1_tmp)
                                    (Array.length r2_tmp)
                                  - 1
                                in
                                while
                                  !common < limit
                                  && r1_rev_tmp.(!common) = r2_tmp.(!common)
                                do
                                  incr common
                                done;
                                if !common > !best_common then (
                                  best_common := !common;
                                  best_nid := Some nid))
                          done;

                          match !best_nid with
                          | Some shared_nid ->
                              if !best_is_dup then (
                                used.(j) <- true;
                                merged := true;
                                found_match := true)
                              else if !best_common >= 2 then
                                (* Merge along the shared segment (2+ shared
                                   consecutive nodes = 1+ shared edges).
                                   Rotate both rings so index 0 is the node
                                   where the match was found: r2 walks the
                                   shared segment forward at indices 0..c-1
                                   while r1 walks it backward, i.e.
                                   r2.(k) = r1.((len1 - k) mod len1). *)
                                let c = !best_common in
                                let r1 = rotate_to_nid shared_nid r1_arr in
                                let r2 = rotate_to_nid shared_nid other_norm in
                                let len1 = Array.length r1 - 1 in
                                let len2 = Array.length r2 - 1 in
                                if c >= len2 then (
                                  (* All of r2's distinct nodes lie on the
                                     shared segment - absorb it *)
                                  used.(j) <- true;
                                  merged := true;
                                  found_match := true)
                                else if c < len1 then (
                                  (* Splice: r1's non-shared path from the
                                     shared node to the far end of the shared
                                     segment (indices 0..len1-c+1), then r2's
                                     non-shared path (indices c..len2-1). The
                                     shared interior nodes appear in neither
                                     part; the two segment endpoints appear
                                     exactly once each. *)
                                  let part1 =
                                    Array.to_list
                                      (Array.sub r1 0 (len1 - c + 2))
                                  in
                                  let part2 =
                                    Array.to_list (Array.sub r2 c (len2 - c))
                                  in
                                  let final = part1 @ part2 in
                                  current := final @ [ List.hd final ];
                                  used.(j) <- true;
                                  merged := true;
                                  found_match := true)
                          | None -> ())
                      done
                    done;
                    result := Array.of_list !current :: !result)
                done;
                if !merged then merge_all (List.rev !result)
                else List.rev !result
              in
              merge_all inner_rings
          in

          if outer_rings = [] then (
            if inner_rings <> [] then
              Printf.printf
                "Warning: Dropping %d inner rings with no outer ring in \
                 relation %d\n\
                 %!"
                (List.length inner_rings) rel.rel_id;
            acc)
          else
            (* Convert node ID arrays to point lists *)
            let outer_point_rings =
              List.filter_map (nodes_to_points rel.rel_id) outer_rings
            in
            let inner_point_rings =
              List.filter_map (nodes_to_points rel.rel_id) inner_rings
            in

            (* Assign holes to outer rings *)
            let polygons =
              List.map
                (fun outer ->
                  let holes =
                    List.filter
                      (fun inner ->
                        match inner with
                        | pt :: _ -> point_in_ring pt outer
                        | [] -> false)
                      inner_point_rings
                  in
                  outer :: holes)
                outer_point_rings
            in
            { id = rel.rel_id; clc_code; polygons } :: acc)
      relations []
  in

  (* Also process standalone ways (not part of relations) *)
  let relation_way_ids =
    Hashtbl.fold
      (fun _ rel acc ->
        List.fold_left
          (fun s m -> if m.member_type = "way" then m.member_ref :: s else s)
          acc rel.members)
      relations []
  in
  let relation_way_set = Hashtbl.create 10000 in
  List.iter (fun wid -> Hashtbl.add relation_way_set wid true) relation_way_ids;

  let standalone_features =
    Hashtbl.fold
      (fun wid (w : way_info) acc ->
        if Hashtbl.mem relation_way_set wid then acc
        else
          let water_tag = w.tags |> member "water" |> to_option to_string in
          let natural_tag = w.tags |> member "natural" |> to_option to_string in
          if natural_tag <> Some "water" && water_tag = None then acc
          else
            let clc_code = water_tag_to_clc_code water_tag in
            let n_nodes = Array.length w.node_ids in
            if n_nodes > 0 && w.node_ids.(0) = w.node_ids.(n_nodes - 1) then
              match nodes_to_points wid w.node_ids with
              (* A valid polygon must have at least 4 points (A-B-C-A) *)
              | Some points when List.length points >= 4 ->
                  { id = wid; clc_code; polygons = [ [ points ] ] } :: acc
              | _ -> acc
            else acc)
      ways []
  in

  features @ standalone_features

(* --- Main API --- *)

let fetch_water_polygons ~min_lat ~min_lon ~max_lat ~max_lon =
  Printf.printf
    "Fetching OSM water polygons for bbox [%.4f,%.4f,%.4f,%.4f]...\n%!" min_lat
    min_lon max_lat max_lon;

  let query = build_overpass_query ~min_lat ~min_lon ~max_lat ~max_lon in
  match fetch_overpass_data query with
  | None -> failwith "Overpass API request failed after all retries"
  | Some response ->
      let features = parse_overpass_elements response in
      Printf.printf "Fetched %d water features from OSM\n%!"
        (List.length features);
      features

(* Convert to flat vertex array format used by polygon_clipping/triangulation *)
let feature_to_flat_arrays (feature : water_feature) =
  List.map
    (fun polygon ->
      let outer = List.hd polygon in
      let holes = List.tl polygon in

      (* Flatten outer ring *)
      let outer_flat =
        Array.of_list (List.concat_map (fun pt -> [ pt.x; pt.y ]) outer)
      in

      (* Flatten hole rings *)
      let holes_flat =
        Array.of_list
          (List.map
             (fun ring ->
               Array.of_list (List.concat_map (fun pt -> [ pt.x; pt.y ]) ring))
             holes)
      in

      (feature.clc_code, outer_flat, holes_flat))
    feature.polygons
