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
type water_feature = { clc_code : int; polygons : polygon list }

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
    {|[out:json][bbox:%f,%f,%f,%f];
(
  way["natural"="water"];
  relation["natural"="water"];
);
out body geom;|}
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

(* --- GeoJSON Parsing --- *)

let parse_coordinates json =
  let open Yojson.Safe.Util in
  json |> to_list
  |> List.map (fun coord ->
      match to_list coord with
      | [ lon_j; lat_j ] | [ lon_j; lat_j; _ ] ->
          { x = to_float lon_j; y = to_float lat_j }
      | _ -> failwith "Invalid coordinate")

let parse_linear_ring json = parse_coordinates json

let parse_polygon_coords json =
  let open Yojson.Safe.Util in
  json |> to_list |> List.map parse_linear_ring

let parse_multipolygon_coords json =
  let open Yojson.Safe.Util in
  json |> to_list |> List.map parse_polygon_coords

(* Parse Overpass JSON response (not standard GeoJSON, but similar) *)
let parse_overpass_elements json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let elements = json |> member "elements" |> to_list in

  List.filter_map
    (fun elem ->
      let elem_type = elem |> member "type" |> to_string in
      let tags = elem |> member "tags" in
      let water_tag = tags |> member "water" |> to_option to_string in
      let clc_code = water_tag_to_clc_code water_tag in

      match elem_type with
      | "way" -> (
          (* Way geometry is in "geometry" array - can be null *)
          match elem |> member "geometry" |> to_option to_list with
          | None -> None
          | Some geom when List.length geom < 3 -> None
          | Some geom ->
              let ring =
                List.map
                  (fun pt ->
                    {
                      x = pt |> member "lon" |> to_float;
                      y = pt |> member "lat" |> to_float;
                    })
                  geom
              in
              Some { clc_code; polygons = [ [ ring ] ] })
      | "relation" -> (
          (* Relations have "members" with roles - ways need to be chained together *)
          match elem |> member "members" |> to_option to_list with
          | None -> None
          | Some members ->
              (* Extract all way segments with their role *)
              let extract_segment role m =
                let m_role = m |> member "role" |> to_string in
                if m_role = role then
                  match m |> member "geometry" |> to_option to_list with
                  | None -> None
                  | Some geom when List.length geom < 2 -> None
                  | Some geom ->
                      Some
                        (List.map
                           (fun pt ->
                             {
                               x = pt |> member "lon" |> to_float;
                               y = pt |> member "lat" |> to_float;
                             })
                           geom)
                else None
              in

              let outer_segments =
                List.filter_map (extract_segment "outer") members
              in
              let inner_segments =
                List.filter_map (extract_segment "inner") members
              in

              (* Chain segments together to form closed rings *)
              let chain_segments segments =
                if segments = [] then []
                else
                  let epsilon = 1e-7 in
                  let points_equal p1 p2 =
                    abs_float (p1.x -. p2.x) < epsilon
                    && abs_float (p1.y -. p2.y) < epsilon
                  in

                  (* Try to chain all segments into rings *)
                  let remaining = ref segments in
                  let rings = ref [] in

                  while !remaining <> [] do
                    (* Start a new ring with the first remaining segment *)
                    let seg = List.hd !remaining in
                    remaining := List.tl !remaining;
                    let current_ring = ref seg in

                    (* Keep trying to extend the ring *)
                    let changed = ref true in
                    while !changed do
                      changed := false;
                      let still_remaining = ref [] in
                      List.iter
                        (fun s ->
                          if not !changed then begin
                            let ring_start = List.hd !current_ring in
                            let ring_end = List.hd (List.rev !current_ring) in
                            let seg_start = List.hd s in
                            let seg_end = List.hd (List.rev s) in

                            if points_equal ring_end seg_start then begin
                              (* Append segment (skip first point to avoid duplicate) *)
                              current_ring := !current_ring @ List.tl s;
                              changed := true
                            end
                            else if points_equal ring_end seg_end then begin
                              (* Append reversed segment *)
                              current_ring :=
                                !current_ring @ List.tl (List.rev s);
                              changed := true
                            end
                            else if points_equal ring_start seg_end then begin
                              (* Prepend segment *)
                              current_ring :=
                                List.rev (List.tl (List.rev s)) @ !current_ring;
                              changed := true
                            end
                            else if points_equal ring_start seg_start then begin
                              (* Prepend reversed segment *)
                              current_ring :=
                                List.tl (List.rev s) @ !current_ring;
                              changed := true
                            end
                            else still_remaining := s :: !still_remaining
                          end
                          else still_remaining := s :: !still_remaining)
                        !remaining;
                      remaining := !still_remaining
                    done;

                    (* Only add if it forms a valid ring (at least 3 points) *)
                    if List.length !current_ring >= 3 then
                      rings := !current_ring :: !rings
                  done;
                  !rings
              in

              let outer_rings = chain_segments outer_segments in
              let inner_rings = chain_segments inner_segments in

              if List.length outer_rings = 0 then None
              else
                (* Each outer ring becomes a polygon, with inner rings as potential holes *)
                let polygons =
                  List.map (fun outer -> outer :: inner_rings) outer_rings
                in
                Some { clc_code; polygons })
      | _ -> None)
    elements

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
