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

(* The explicit server-side timeout stays below curl's --max-time (120s), so
   a slow query fails fast with a server "remark" (retried below) instead of
   being repeatedly killed by curl at the default 180s server budget *)
let build_overpass_query ~min_lat ~min_lon ~max_lat ~max_lon =
  Printf.sprintf
    {|[out:json][timeout:110][bbox:%f,%f,%f,%f];
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
    Printf.printf "POI Overpass API request (attempt %d/%d)...\n%!" attempt
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
        Printf.eprintf "POI Overpass request failed after %d attempts: %s\n%!"
          max_retries err;
        None
  in
  retry 1

(* --- JSON Parsing --- *)

let parse_overpass_elements json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let elements = json |> member "elements" |> to_list in

  List.filter_map
    (fun elem ->
      let tags =
        match elem |> member "tags" with `Null -> `Assoc [] | t -> t
      in

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
          let lat = elem |> member "lat" |> to_number in
          let lon = elem |> member "lon" |> to_number in

          (* Parse elevation; an unparseable "ele" tag (e.g. "ca. 3000",
             "1234;1236") means the elevation is unknown, not 0 *)
          let elevation =
            match tags |> member "ele" |> to_option to_string with
            | None -> None
            | Some s -> (
                match float_of_string_opt s with
                | Some e -> Some (truncate (Float.round e))
                | None -> None)
          in

          (* Determine POI type *)
          let natural =
            tags |> member "natural" |> to_option to_string
            |> Option.value ~default:""
          in
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
