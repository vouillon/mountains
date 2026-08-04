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

(* Elements may be written with a namespace prefix -- <gpx:trkpt> as much as
   <trkpt> -- which belongs to how the document spells the name, not to the
   element's identity. A prefix is an XML name followed by ':', so a tag is
   recognised by skipping one when it is there and comparing what follows.
   Attributes need no such care: [extract_attr] looks for the name anywhere
   inside the tag, so gpx:lat would be found as readily as lat. *)
let is_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
  | _ -> false

let is_ws = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

(* [tag_at tag ~ok str pos] is the position just after [tag], had [str] the name
   [tag] at [pos] -- just past a '<' or a '</' -- prefixed or not. [ok] decides
   whether what comes next ends the name, which is what tells <trk> from
   <trkpt>. Both spellings are offered to it, so a prefix that happens to
   repeat the local name cannot hide the element. [tag] must be lowercase. *)
let tag_at tag ~ok str pos =
  let len = String.length str in
  let tag_len = String.length tag in
  let candidate p =
    if
      p + tag_len <= len
      && String.lowercase_ascii (String.sub str p tag_len) = tag
      && ok (p + tag_len)
    then Some (p + tag_len)
    else None
  in
  match candidate pos with
  | Some stop -> Some stop
  | None -> (
      (* Skip a prefix, if that is what stands here: name characters up to ':' *)
      let rec prefix p =
        if p < len && is_name_char str.[p] then prefix (p + 1)
        else if p > pos && p < len && str.[p] = ':' then candidate (p + 1)
        else None
      in
      match prefix pos with Some stop -> Some stop | None -> None)

(* [close_tag_at tag str pos] is the position just past the '>' of a closing tag
   for [tag] starting at [pos]. *)
let close_tag_at tag str pos =
  let len = String.length str in
  let rec skip_ws p = if p < len && is_ws str.[p] then skip_ws (p + 1) else p in
  let terminator stop =
    let p = skip_ws stop in
    if p < len && str.[p] = '>' then Some (p + 1) else None
  in
  if pos + 1 >= len || str.[pos] <> '<' || str.[pos + 1] <> '/' then None
  else
    match
      tag_at tag ~ok:(fun stop -> terminator stop <> None) str (pos + 2)
    with
    | Some stop -> terminator stop
    | None -> None

(* GPX does not nest any of the elements read here, so the first closing tag is
   the one that closes this element. *)
let close_tag_from tag str pos =
  let len = String.length str in
  let rec search p =
    if p >= len then None
    else
      match close_tag_at tag str p with
      | Some stop -> Some stop
      | None -> search (p + 1)
  in
  search pos

let extract_tag tag str =
  let len = String.length str in
  let tag = String.lowercase_ascii tag in
  let rec search pos =
    if pos >= len then None
    else if str.[pos] <> '<' then search (pos + 1)
    else
      (* The elements read this way (<ele>, <name>, <desc>) carry no attributes,
         so the name runs straight into '>'. *)
      match
        tag_at tag ~ok:(fun stop -> stop < len && str.[stop] = '>') str (pos + 1)
      with
      | None -> search (pos + 1)
      | Some stop -> (
          let start_val = stop + 1 in
          match String.index_from_opt str start_val '<' with
          | Some cpos ->
              (* The value has to run into the closing tag: these elements hold
                 text, and something with children is not one of them. *)
              if close_tag_at tag str cpos <> None then
                Some (String.sub str start_val (cpos - start_val))
              else None
          | None -> None)
  in
  search 0

let find_all_elements tag xml_str =
  let str_len = String.length xml_str in
  let tag = String.lowercase_ascii tag in
  let acc = ref [] in
  let is_tag_boundary = function
    | ' ' | '\t' | '\r' | '\n' | '>' | '/' -> true
    | _ -> false
  in
  let ends_name p = p = str_len || is_tag_boundary xml_str.[p] in
  let rec search pos =
    if pos >= str_len then ()
    else if xml_str.[pos] <> '<' then search (pos + 1)
    else
      match tag_at tag ~ok:ends_name xml_str (pos + 1) with
      | None -> search (pos + 1)
      | Some stop -> (
          let start_val = pos in
          match String.index_from_opt xml_str stop '>' with
          | None -> ()
          | Some open_end -> (
              if open_end > start_val && xml_str.[open_end - 1] = '/' then begin
                let end_val = open_end + 1 in
                acc :=
                  String.sub xml_str start_val (end_val - start_val) :: !acc;
                search end_val
              end
              else
                match close_tag_from tag xml_str (open_end + 1) with
                | None -> ()
                | Some end_val ->
                    acc :=
                      String.sub xml_str start_val (end_val - start_val) :: !acc;
                    search end_val))
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
