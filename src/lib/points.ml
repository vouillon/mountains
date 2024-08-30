(*
curl https://overpass-api.de/api/interpreter -d '[out:json]
[bbox:44,6,45,7];
(node[natural=peak];
 node[natural=saddle];);
out;' | wc -c

[out:csv(name,"name:fr",natural,::lat,::lon,ele;false)]
*)

type coord = { lat : float; lon : float }
type t = { name : string; coord : coord; elevation : int option }

let load () =
  let d = Yojson.Safe.from_file "data/points.geojson" in
  let open Yojson.Safe.Util in
  d |> member "features" |> to_list
  |> List.map (fun d ->
         match
           ( d |> member "properties" |> member "name:fr" |> to_option to_string,
             d |> member "properties" |> member "name" |> to_option to_string )
         with
         | None, None -> []
         | Some name, _ | None, Some name ->
             let coord =
               match
                 d |> member "geometry" |> member "coordinates" |> to_list
                 |> List.map to_number
               with
               | [ lon; lat ] -> { lat; lon }
               | _ -> assert false
             in
             let elevation =
               d |> member "properties" |> member "ele" |> to_option to_string
               |> Option.map (fun s -> truncate (float_of_string s +. 0.5))
             in
             [ { name; elevation; coord } ])
  |> List.flatten

let find { lat; lon } { lat = lat'; lon = lon' } =
  let l = load () in
  List.filter
    (fun { coord = { lat = lat''; lon = lon'' }; _ } ->
      lat < lat'' && lat'' < lat' && lon < lon'' && lon'' < lon')
    l
