type coord = { lat : float; lon : float }

let load () =
  let d = Yojson.Safe.from_file "points.geojson" in
  let open Yojson.Safe.Util in
  d |> member "features" |> to_list
  |> List.map (fun d ->
         match
           d |> member "properties" |> member "name" |> to_option to_string
         with
         | None -> []
         | Some name ->
             let coord =
               match
                 d |> member "geometry" |> member "coordinates" |> to_list
                 |> List.map to_number
               with
               | [ lon; lat ] -> { lat; lon }
               | _ -> assert false
             in
             [ (name, coord) ])
  |> List.flatten

let find { lat; lon } { lat = lat'; lon = lon' } =
  let l = load () in
  List.filter
    (fun (_, { lat = lat''; lon = lon'' }) ->
      lat < lat'' && lat'' < lat' && lon < lon'' && lon'' < lon')
    l
