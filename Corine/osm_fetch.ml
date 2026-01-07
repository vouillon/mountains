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

let build_overpass_query ~min_lat ~min_lon ~max_lat ~max_lon =
  Printf.sprintf
    {|[out:json][bbox:%f,%f,%f,%f];(wr[natural=water];wr[water];);(._;>>;);out body;|}
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

let fetch_overpass_data ?(max_retries = 10) query =
  let rec retry attempt =
    Printf.printf "Overpass API request (attempt %d/%d)...\n%!" attempt
      max_retries;
    match fetch_overpass_data_once query with
    | Some response
      when String.length response >= 1 && String.sub response 0 1 = "{" ->
        Some response
    | (Some _ | None) when attempt < max_retries ->
        let delay = 2.0 *. (1.5 ** float (attempt - 1)) in
        Printf.printf "Request failed (rate limited?), retrying in %.0fs...\n%!"
          delay;
        Unix.sleepf delay;
        retry (attempt + 1)
    | resp -> resp (* Return last response or None *)
  in
  retry 1

(* --- Parsing with Node ID-based Chaining --- *)

(* Types for intermediate representation *)
type way_info = { way_id : int; node_ids : int array; tags : Yojson.Safe.t }
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
      match elem_type with
      | "node" ->
          let lat = elem |> member "lat" |> to_float in
          let lon = elem |> member "lon" |> to_float in
          Hashtbl.add nodes elem_id (lon, lat)
      | "way" ->
          let node_ids =
            elem |> member "nodes" |> to_list |> List.map to_int
            |> Array.of_list
          in
          let tags = elem |> member "tags" in
          Hashtbl.add ways elem_id { way_id = elem_id; node_ids; tags }
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
          let tags = elem |> member "tags" in
          Hashtbl.add relations elem_id { rel_id = elem_id; members; tags }
      | _ -> ())
    elements;

  (* Helper: get way endpoints (first and last node IDs) *)
  let way_endpoints way =
    let n = Array.length way.node_ids in
    if n < 2 then None else Some (way.node_ids.(0), way.node_ids.(n - 1))
  in

  (* Helper: convert node IDs to coordinate list *)
  let nodes_to_points node_ids =
    Array.fold_right
      (fun nid acc ->
        match Hashtbl.find_opt nodes nid with
        | Some (lon, lat) -> { x = lon; y = lat } :: acc
        | None -> acc)
      node_ids []
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
                  while !changed do
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
              List.map (fun node_arr -> nodes_to_points node_arr) outer_rings
            in
            let inner_point_rings =
              List.map (fun node_arr -> nodes_to_points node_arr) inner_rings
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
            let points = nodes_to_points w.node_ids in
            if List.length points >= 3 then
              { id = wid; clc_code; polygons = [ [ points ] ] } :: acc
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
