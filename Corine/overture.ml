(* overture.ml - Water polygons and POIs from Overture Maps

   Overture's base theme is OSM's data, read by bounding box, a cell at a
   time, where the Overpass API was one more server to depend on. Its land
   layer has the peaks and the saddles, with their names and elevations.

   Its water layer is OSM's water, the sea included: the coastline, which OSM
   has as lines, comes assembled into polygons (subtype "ocean", cut into
   pieces), where the coastline polygons of osmdata.openstreetmap.de are one
   file for the whole world.

   Mapping to CLC codes, as Osm_fetch:
     ocean                -> 523 (Sea and ocean)
     lagoon               -> 521 (Coastal lagoons)
     river, canal, stream -> 511 (Water courses)
     (anything else)      -> 512 (Water bodies)
*)

open Osm_fetch

(* Not water to draw: swimming pools and the like, which are most of the
   polygons of a town, and what the "physical" subtype names over the water
   that is there already (bays, straits). *)
let clc_code ~subtype ~cls =
  match (subtype, cls) with
  | ("human_made" | "physical"), _ | _, ("wastewater" | "swimming_pool") -> None
  | "ocean", _ -> Some 523
  | _, "lagoon" -> Some 521
  | ("river" | "canal" | "stream"), _ -> Some 511
  | _ -> Some 512

(* A GeoJSON ring repeats its first point at the end *)
let ring_of_json json =
  let open Yojson.Safe.Util in
  let points =
    List.map
      (fun p ->
        match to_list p with
        | x :: y :: _ -> { x = to_number x; y = to_number y }
        | _ -> failwith "Overture: malformed position")
      (to_list json)
  in
  match (points, List.rev points) with
  | first :: _, last :: rest when first = last -> List.rev rest
  | _ -> points

(* The outer ring, then the holes *)
let polygon_of_json json =
  match List.map ring_of_json (Yojson.Safe.Util.to_list json) with
  | outer :: holes when List.length outer >= 3 ->
      outer :: List.filter (fun ring -> List.length ring >= 3) holes
  | _ -> []

(* A line of a GeoJSON sequence: RFC 8142 puts a record separator before
   each feature *)
let json_of_line line =
  let line = String.trim line in
  let line =
    if line <> "" && line.[0] = '\x1e' then
      String.sub line 1 (String.length line - 1)
    else line
  in
  if line = "" then None else Some (Yojson.Safe.from_string line)

(* A field of an object, [`Null] without one: a feature without a name has no
   [names] at all, which [Yojson.Safe.Util.member] takes for an error *)
let field name = function
  | `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null
  | _ -> `Null

let property json name =
  match Yojson.Safe.Util.(member name (member "properties" json)) with
  | `String s -> s
  | _ -> ""

let water_of_line id line =
  let open Yojson.Safe.Util in
  match json_of_line line with
  | None -> None
  | Some json -> (
      match
        clc_code ~subtype:(property json "subtype") ~cls:(property json "class")
      with
      | None -> None
      | Some clc_code -> (
          let geometry = member "geometry" json in
          let coordinates = member "coordinates" geometry in
          let polygons =
            match member "type" geometry with
            | `String "Polygon" -> [ polygon_of_json coordinates ]
            | `String "MultiPolygon" ->
                List.map polygon_of_json (to_list coordinates)
            | _ -> []
          in
          match List.filter (fun polygon -> polygon <> []) polygons with
          | [] -> None
          | polygons -> Some { id; clc_code; polygons }))

let water_of_lines lines =
  List.filter_map Fun.id (List.mapi water_of_line lines)

let lines file = In_channel.with_open_bin file In_channel.input_lines

let load_water file =
  let features = water_of_lines (lines file) in
  Printf.printf "Read %d water features from %s\n%!" (List.length features) file;
  features

(* The named peaks and saddles, as Poi_fetch: the French name where there is
   one ([names.common] is a list of language and name pairs), the elevation
   where OSM's parsed as a number. *)
let poi_of_line line =
  let open Yojson.Safe.Util in
  match json_of_line line with
  | None -> None
  | Some json -> (
      let poi_type =
        match property json "class" with
        | "peak" -> Some Poi_fetch.Peak
        | "saddle" -> Some Poi_fetch.Saddle
        | _ -> None
      in
      let geometry = member "geometry" json in
      let names = member "names" (member "properties" json) in
      let name =
        let french =
          match field "common" names with
          | `List pairs ->
              List.find_map
                (function
                  | `List [ `String "fr"; `String name ] -> Some name
                  | _ -> None)
                pairs
          | `Assoc pairs -> (
              match List.assoc_opt "fr" pairs with
              | Some (`String name) -> Some name
              | _ -> None)
          | _ -> None
        in
        match (french, field "primary" names) with
        | Some name, _ | None, `String name -> Some name
        | None, _ -> None
      in
      match (poi_type, name, member "type" geometry) with
      | Some poi_type, Some name, `String "Point" -> (
          match to_list (member "coordinates" geometry) with
          | lon :: lat :: _ ->
              let elevation =
                match member "elevation" (member "properties" json) with
                | (`Int _ | `Float _) as e ->
                    Some (truncate (Float.round (to_number e)))
                | _ -> None
              in
              Some
                {
                  Poi_fetch.name;
                  lat = to_number lat;
                  lon = to_number lon;
                  elevation;
                  poi_type;
                }
          | _ -> None)
      | _ -> None)

let pois_of_lines lines = List.filter_map poi_of_line lines

let load_pois file =
  let pois = pois_of_lines (lines file) in
  Printf.printf "Read %d POIs from %s\n%!" (List.length pois) file;
  pois
