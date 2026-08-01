type trackpoint = { lat : float; lon : float; ele : float option }

type waypoint = {
  name : string;
  lat : float;
  lon : float;
  ele : float option;
  desc : string option;
}

type track = { name : string; points : trackpoint list }
type gpx_data = { tracks : track list; waypoints : waypoint list }

let parse_float_opt str_opt =
  match str_opt with
  | None -> None
  | Some s -> ( try Some (float_of_string (String.trim s)) with _ -> None)

let extract_attr name str =
  let name_len = String.length name in
  let str_len = String.length str in
  let rec find_name pos =
    if pos + name_len > str_len then None
    else
      let candidate = String.lowercase_ascii (String.sub str pos name_len) in
      if candidate = String.lowercase_ascii name then
        let rec skip_ws p =
          if p >= str_len then None
          else if
            str.[p] = ' ' || str.[p] = '\t' || str.[p] = '\r' || str.[p] = '\n'
          then skip_ws (p + 1)
          else if str.[p] = '=' then skip_eq (p + 1)
          else None
        and skip_eq p =
          if p >= str_len then None
          else if
            str.[p] = ' ' || str.[p] = '\t' || str.[p] = '\r' || str.[p] = '\n'
          then skip_eq (p + 1)
          else if str.[p] = '"' || str.[p] = '\'' then
            let quote = str.[p] in
            let start_v = p + 1 in
            match String.index_from_opt str start_v quote with
            | Some end_v -> Some (String.sub str start_v (end_v - start_v))
            | None -> None
          else None
        in
        skip_ws (pos + name_len)
      else find_name (pos + 1)
  in
  find_name 0

let extract_tag tag str =
  let open_tag = "<" ^ String.lowercase_ascii tag ^ ">" in
  let close_tag = "</" ^ String.lowercase_ascii tag ^ ">" in
  let str_lower = String.lowercase_ascii str in
  let len_o = String.length open_tag in
  let len_c = String.length close_tag in
  let len_str = String.length str_lower in
  let rec search pos =
    if pos + len_o > len_str then None
    else if String.sub str_lower pos len_o = open_tag then
      let start_val = pos + len_o in
      match String.index_from_opt str_lower start_val '<' with
      | Some cpos ->
          if
            cpos + len_c <= len_str
            && String.sub str_lower cpos len_c = close_tag
          then Some (String.sub str start_val (cpos - start_val))
          else None
      | None -> None
    else search (pos + 1)
  in
  search 0

let find_all_elements tag xml_str =
  let open_prefix = "<" ^ tag in
  let close_tag = "</" ^ tag ^ ">" in
  let prefix_len = String.length open_prefix in
  let close_len = String.length close_tag in
  let str_len = String.length xml_str in
  let str_lower = String.lowercase_ascii xml_str in
  let prefix_lower = String.lowercase_ascii open_prefix in
  let close_lower = String.lowercase_ascii close_tag in
  let acc = ref [] in
  let is_tag_boundary = function
    | ' ' | '\t' | '\r' | '\n' | '>' | '/' -> true
    | _ -> false
  in
  let rec search pos =
    if pos + prefix_len > str_len then ()
    else if
      String.sub str_lower pos prefix_len = prefix_lower
      && (pos + prefix_len = str_len
         || is_tag_boundary str_lower.[pos + prefix_len])
    then
      let start_val = pos in
      match String.index_from_opt str_lower (start_val + prefix_len) '>' with
      | None -> ()
      | Some open_end ->
          if open_end > start_val && str_lower.[open_end - 1] = '/' then (
            let end_val = open_end + 1 in
            acc := String.sub xml_str start_val (end_val - start_val) :: !acc;
            search end_val)
          else
            let rec search_close cpos =
              if cpos + close_len > str_len then ()
              else if String.sub str_lower cpos close_len = close_lower then (
                let end_val = cpos + close_len in
                acc :=
                  String.sub xml_str start_val (end_val - start_val) :: !acc;
                search end_val)
              else search_close (cpos + 1)
            in
            search_close (open_end + 1)
    else search (pos + 1)
  in
  search 0;
  List.rev !acc

let parse xml_str =
  let waypoints =
    let blocks = find_all_elements "wpt" xml_str in
    List.filter_map
      (fun block ->
        let lat = parse_float_opt (extract_attr "lat" block) in
        let lon = parse_float_opt (extract_attr "lon" block) in
        let ele = parse_float_opt (extract_tag "ele" block) in
        let name =
          match extract_tag "name" block with
          | Some n -> String.trim n
          | None -> "Waypoint"
        in
        let desc = extract_tag "desc" block in
        match (lat, lon) with
        | Some lat, Some lon -> Some { name; lat; lon; ele; desc }
        | _ -> None)
      blocks
  in

  let trk_containers = find_all_elements "trk" xml_str in
  let containers =
    if trk_containers <> [] then trk_containers
    else find_all_elements "rte" xml_str
  in
  let parse_single_container container_str =
    let name =
      match extract_tag "name" container_str with
      | Some n -> String.trim n
      | None -> "GPX Track"
    in
    let trk_blocks = find_all_elements "trkpt" container_str in
    let rte_blocks = find_all_elements "rtept" container_str in
    let blocks = if trk_blocks <> [] then trk_blocks else rte_blocks in
    let points =
      List.filter_map
        (fun block ->
          let lat = parse_float_opt (extract_attr "lat" block) in
          let lon = parse_float_opt (extract_attr "lon" block) in
          let ele = parse_float_opt (extract_tag "ele" block) in
          match (lat, lon) with
          | Some lat, Some lon -> Some { lat; lon; ele }
          | _ -> None)
        blocks
    in
    if points = [] then None else Some { name; points }
  in
  let tracks =
    if containers <> [] then List.filter_map parse_single_container containers
    else
      let single = parse_single_container xml_str in
      match single with Some t -> [ t ] | None -> []
  in

  { tracks; waypoints }
