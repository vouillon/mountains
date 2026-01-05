(* poi_fetch.ml - Fetch POI (peaks, saddles) from OpenStreetMap via Overpass API *)

(* 
   Query POI nodes using Overpass API, parse JSON response.
   
   Usage:
     let pois = Poi_fetch.fetch_pois ~min_lat ~min_lon ~max_lat ~max_lon in
   
   POI Types:
     - Peak (natural=peak)
     - Saddle (natural=saddle)
*)

(* --- Types --- *)

type poi_type = Peak | Saddle

type poi = {
  name : string;
  lat : float;
  lon : float;
  elevation : int option;
  poi_type : poi_type;
}

(* --- Overpass Query --- *)

let build_overpass_query ~min_lat ~min_lon ~max_lat ~max_lon =
  Printf.sprintf
    {|[out:json][bbox:%f,%f,%f,%f];
(
  node["natural"="peak"];
  node["natural"="saddle"];
);
out;|}
    min_lat min_lon max_lat max_lon

(* --- HTTP Fetch via curl with retry --- *)

let fetch_overpass_data_once query =
  let url = "https://overpass-api.de/api/interpreter" in
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

let fetch_overpass_data ?(max_retries = 5) query =
  let rec retry attempt =
    Printf.printf "POI Overpass API request (attempt %d/%d)...\n%!" attempt
      max_retries;
    match fetch_overpass_data_once query with
    | Some response
      when String.length response >= 1 && String.sub response 0 1 = "{" ->
        Some response
    | (Some _ | None) when attempt < max_retries ->
        let delay = 2.0 *. (3.0 ** float (attempt - 1)) in
        Printf.printf "Request failed (rate limited?), retrying in %.0fs...\n%!"
          delay;
        Unix.sleepf delay;
        retry (attempt + 1)
    | resp -> resp
  in
  retry 1

(* --- JSON Parsing --- *)

let parse_overpass_elements json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let elements = json |> member "elements" |> to_list in

  List.filter_map
    (fun elem ->
      let tags = elem |> member "tags" in

      (* Get name - prefer French name *)
      let name =
        match
          ( tags |> member "name:fr" |> to_option to_string,
            tags |> member "name" |> to_option to_string )
        with
        | Some n, _ -> Some n
        | None, Some n -> Some n
        | None, None -> None
      in

      match name with
      | None -> None (* Skip POIs without names *)
      | Some name ->
          let lat = elem |> member "lat" |> to_float in
          let lon = elem |> member "lon" |> to_float in

          (* Parse elevation *)
          let elevation =
            tags |> member "ele" |> to_option to_string
            |> Option.map (fun s ->
                try truncate (float_of_string s +. 0.5) with Failure _ -> 0)
          in

          (* Determine POI type *)
          let natural = tags |> member "natural" |> to_string in
          let poi_type =
            match natural with
            | "peak" -> Peak
            | "saddle" -> Saddle
            | _ -> Peak (* Default to peak *)
          in

          Some { name; lat; lon; elevation; poi_type })
    elements

(* --- Main API --- *)

let fetch_pois ~min_lat ~min_lon ~max_lat ~max_lon =
  Printf.printf "Fetching POIs for bbox [%.4f,%.4f,%.4f,%.4f]...\n%!" min_lat
    min_lon max_lat max_lon;

  let query = build_overpass_query ~min_lat ~min_lon ~max_lat ~max_lon in
  match fetch_overpass_data query with
  | None -> failwith "POI Overpass API request failed after all retries"
  | Some response ->
      let pois = parse_overpass_elements response in
      Printf.printf "Fetched %d POIs from OSM\n%!" (List.length pois);
      pois
