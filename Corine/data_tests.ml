(* Regression tests for WKB decoding and Overpass response parsing.
   Run via `dune runtest`. Silent on success; fails loudly otherwise. *)

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("FAIL: " ^ s);
      exit 1)
    fmt

(* --- WKB building helpers --- *)

let u8 b v = Buffer.add_char b (Char.chr (v land 0xFF))

let u32 b v =
  u8 b v;
  u8 b (v lsr 8);
  u8 b (v lsr 16);
  u8 b (v lsr 24)

let f64 b f =
  let bits = Int64.bits_of_float f in
  for i = 0 to 7 do
    u8 b (Int64.to_int (Int64.shift_right_logical bits (i * 8)))
  done

(* --- WKB: plain 2D polygon --- *)

let () =
  let b = Buffer.create 256 in
  u8 b 1 (* little endian *);
  u32 b 3 (* Polygon *);
  u32 b 1 (* 1 ring *);
  u32 b 4 (* 4 points, closed *);
  List.iter
    (fun (x, y) ->
      f64 b x;
      f64 b y)
    [ (0., 0.); (1., 0.); (0., 1.); (0., 0.) ];
  match Wkb_decode.decode_wkb (Buffer.contents b) with
  | Some (Wkb_decode.Polygon [ ring ]) ->
      let coords = List.map (fun (p : Wkb_decode.point) -> (p.x, p.y)) ring in
      if coords <> [ (0., 0.); (1., 0.); (0., 1.); (0., 0.) ] then
        fail "2D polygon: wrong coordinates"
  | _ -> fail "2D polygon: decode failed"

(* --- WKB: ISO-coded PolygonZM (type 3003) with 4 doubles per point --- *)

let () =
  let b = Buffer.create 256 in
  u8 b 1;
  u32 b 3003 (* ISO PolygonZM *);
  u32 b 1;
  u32 b 4;
  List.iter
    (fun (x, y) ->
      f64 b x;
      f64 b y;
      f64 b 42.0 (* z *);
      f64 b nan (* m *))
    [ (0., 0.); (1., 0.); (0., 1.); (0., 0.) ];
  match Wkb_decode.decode_wkb (Buffer.contents b) with
  | Some (Wkb_decode.Polygon [ ring ]) ->
      let coords = List.map (fun (p : Wkb_decode.point) -> (p.x, p.y)) ring in
      if coords <> [ (0., 0.); (1., 0.); (0., 1.); (0., 0.) ] then
        fail "ISO PolygonZM: wrong coordinates (Z/M not skipped?)"
  | _ -> fail "ISO PolygonZM: decode failed"

(* --- WKB: GeoPackage-wrapped MultiPolygon, mirroring the structure of the
   CLC2018 GeoPackage: GP header with 2D envelope, ISO-coded MultiPolygonZM
   (3006) whose child polygon uses EWKB Z/M flag bits (0xC0000003) --- *)

let () =
  let b = Buffer.create 512 in
  (* GPKG header *)
  Buffer.add_string b "GP";
  u8 b 0 (* version *);
  u8 b 2 (* flags: envelope indicator 1 (32 bytes) *);
  u32 b 3035 (* SRID *);
  List.iter (f64 b) [ 0.; 1.; 0.; 1. ] (* minx maxx miny maxy *);
  (* WKB body *)
  u8 b 1;
  u32 b 3006 (* ISO MultiPolygonZM *);
  u32 b 1 (* 1 polygon *);
  u8 b 1;
  u32 b 0xC0000003 (* EWKB Polygon with Z and M flags *);
  u32 b 1;
  u32 b 4;
  List.iter
    (fun (x, y) ->
      f64 b x;
      f64 b y;
      f64 b 0.0;
      f64 b nan)
    [ (0., 0.); (1., 0.); (0., 1.); (0., 0.) ];
  match Wkb_decode.decode_wkb (Buffer.contents b) with
  | Some (Wkb_decode.MultiPolygon [ [ ring ] ]) ->
      let coords = List.map (fun (p : Wkb_decode.point) -> (p.x, p.y)) ring in
      if coords <> [ (0., 0.); (1., 0.); (0., 1.); (0., 0.) ] then
        fail "GPKG MultiPolygonZM: wrong coordinates"
  | _ -> fail "GPKG MultiPolygonZM: decode failed"

(* --- Projections, against PROJ (cs2cs) reference coordinates --- *)

let () =
  let close tol a b = abs_float (a -. b) < tol in
  (* EPSG:3035: IOGP Guidance Note 7-2 worked example (5E, 50N) *)
  let x, y = Proj_3035.wgs84_to_laea 5.0 50.0 in
  if not (close 0.001 x 3962799.4510 && close 0.001 y 2999718.8532) then
    fail "proj_3035 forward: got (%.4f, %.4f)" x y;
  let lon, lat = Proj_3035.laea_to_wgs84 3962799.4510 2999718.8532 in
  if not (close 1e-8 lon 5.0 && close 1e-8 lat 50.0) then
    fail "proj_3035 inverse: got (%.8f, %.8f)" lon lat;
  (* Projection origin maps back to (10E, 52N) *)
  let lon, lat = Proj_3035.laea_to_wgs84 4321000.0 3210000.0 in
  if not (close 1e-9 lon 10.0 && close 1e-9 lat 52.0) then
    fail "proj_3035 origin: got (%.8f, %.8f)" lon lat;
  (* Round trip in the Alps *)
  let x, y = Proj_3035.wgs84_to_laea 6.5 45.5 in
  let lon, lat = Proj_3035.laea_to_wgs84 x y in
  if not (close 1e-8 lon 6.5 && close 1e-8 lat 45.5) then
    fail "proj_3035 round trip: got (%.8f, %.8f)" lon lat

let () =
  let close tol a b = abs_float (a -. b) < tol in
  (* EPSG:2975 (UTM 40S): cs2cs reference for (55.5E, 21S) *)
  let x, y = Proj_2975.of_wgs84 55.5 (-21.0) in
  if not (close 0.001 x 344093.4543 && close 0.001 y 7677120.8840) then
    fail "proj_2975 forward: got (%.4f, %.4f)" x y;
  let lon, lat = Proj_2975.to_wgs84 344093.4543 7677120.8840 in
  if not (close 1e-8 lon 55.5 && close 1e-8 lat (-21.0)) then
    fail "proj_2975 inverse: got (%.8f, %.8f)" lon lat

(* --- osm_fetch: two inner rings sharing exactly one edge merge into one
   hole (the minimal touching-inner-rings case) --- *)

let overpass_json =
  {|{
  "version": 0.6,
  "elements": [
    {"type": "node", "id": 1, "lat": 0.0, "lon": 0.0},
    {"type": "node", "id": 2, "lat": 0.0, "lon": 10.0},
    {"type": "node", "id": 3, "lat": 10.0, "lon": 10.0},
    {"type": "node", "id": 4, "lat": 10.0, "lon": 0.0},
    {"type": "node", "id": 11, "lat": 2.0, "lon": 2.0},
    {"type": "node", "id": 12, "lat": 2.0, "lon": 4.0},
    {"type": "node", "id": 13, "lat": 4.0, "lon": 4.0},
    {"type": "node", "id": 14, "lat": 4.0, "lon": 2.0},
    {"type": "node", "id": 15, "lat": 2.0, "lon": 6.0},
    {"type": "node", "id": 16, "lat": 4.0, "lon": 6.0},
    {"type": "way", "id": 100, "nodes": [1, 2, 3, 4, 1]},
    {"type": "way", "id": 101, "nodes": [11, 12, 13, 14, 11]},
    {"type": "way", "id": 102, "nodes": [12, 15, 16, 13, 12]},
    {"type": "relation", "id": 500,
     "members": [
       {"type": "way", "ref": 100, "role": "outer"},
       {"type": "way", "ref": 101, "role": "inner"},
       {"type": "way", "ref": 102, "role": "inner"}
     ],
     "tags": {"natural": "water", "water": "lake"}}
  ]
}|}

let () =
  match Osm_fetch.parse_overpass_elements overpass_json with
  | [ f ] -> (
      if f.Osm_fetch.clc_code <> 512 then
        fail "merge: wrong clc code %d" f.Osm_fetch.clc_code;
      match f.Osm_fetch.polygons with
      | [ [ _outer; hole ] ] ->
          (* Two 4-node holes sharing one edge merge into a single 6-node
             ring (+1 closing duplicate) *)
          if List.length hole <> 7 then
            fail "merge: expected merged hole of 7 pts (6+close), got %d"
              (List.length hole)
      | [ rings ] ->
          fail "merge: expected outer + 1 merged hole, got %d rings"
            (List.length rings)
      | ps -> fail "merge: expected 1 polygon, got %d" (List.length ps))
  | l -> fail "merge: expected 1 feature, got %d" (List.length l)

(* --- osm_fetch: nested outer/inner/outer/inner (pond on an island in a
   lake): each hole must attach only to its innermost enclosing outer --- *)

let nested_json =
  {|{
  "elements": [
    {"type": "node", "id": 1, "lat": 0.0, "lon": 0.0},
    {"type": "node", "id": 2, "lat": 0.0, "lon": 30.0},
    {"type": "node", "id": 3, "lat": 30.0, "lon": 30.0},
    {"type": "node", "id": 4, "lat": 30.0, "lon": 0.0},
    {"type": "node", "id": 21, "lat": 5.0, "lon": 5.0},
    {"type": "node", "id": 22, "lat": 5.0, "lon": 25.0},
    {"type": "node", "id": 23, "lat": 25.0, "lon": 25.0},
    {"type": "node", "id": 24, "lat": 25.0, "lon": 5.0},
    {"type": "node", "id": 31, "lat": 10.0, "lon": 10.0},
    {"type": "node", "id": 32, "lat": 10.0, "lon": 20.0},
    {"type": "node", "id": 33, "lat": 20.0, "lon": 20.0},
    {"type": "node", "id": 34, "lat": 20.0, "lon": 10.0},
    {"type": "node", "id": 41, "lat": 13.0, "lon": 13.0},
    {"type": "node", "id": 42, "lat": 13.0, "lon": 17.0},
    {"type": "node", "id": 43, "lat": 17.0, "lon": 17.0},
    {"type": "node", "id": 44, "lat": 17.0, "lon": 13.0},
    {"type": "way", "id": 100, "nodes": [1, 2, 3, 4, 1]},
    {"type": "way", "id": 101, "nodes": [21, 22, 23, 24, 21]},
    {"type": "way", "id": 102, "nodes": [31, 32, 33, 34, 31]},
    {"type": "way", "id": 103, "nodes": [41, 42, 43, 44, 41]},
    {"type": "relation", "id": 600,
     "members": [
       {"type": "way", "ref": 100, "role": "outer"},
       {"type": "way", "ref": 102, "role": "outer"},
       {"type": "way", "ref": 101, "role": "inner"},
       {"type": "way", "ref": 103, "role": "inner"}
     ],
     "tags": {"natural": "water"}}
  ]
}|}

let () =
  match Osm_fetch.parse_overpass_elements nested_json with
  | [ f ] ->
      List.iter
        (fun polygon ->
          match polygon with
          | outer :: holes -> (
              let p0 = List.hd outer in
              match (p0.Osm_fetch.x, p0.Osm_fetch.y) with
              | 0.0, 0.0 ->
                  (* Lake: exactly the island hole (first point (5,5)) *)
                  if List.length holes <> 1 then
                    fail "nested: lake should have 1 hole, got %d"
                      (List.length holes);
                  let h0 = List.hd (List.hd holes) in
                  if h0.Osm_fetch.x <> 5.0 || h0.Osm_fetch.y <> 5.0 then
                    fail "nested: lake got the wrong hole (%.1f, %.1f)"
                      h0.Osm_fetch.x h0.Osm_fetch.y
              | 10.0, 10.0 ->
                  (* Pond: exactly the islet hole (first point (13,13)) *)
                  if List.length holes <> 1 then
                    fail "nested: pond should have 1 hole, got %d"
                      (List.length holes);
                  let h0 = List.hd (List.hd holes) in
                  if h0.Osm_fetch.x <> 13.0 || h0.Osm_fetch.y <> 13.0 then
                    fail "nested: pond got the wrong hole (%.1f, %.1f)"
                      h0.Osm_fetch.x h0.Osm_fetch.y
              | x, y -> fail "nested: unexpected outer start (%.1f, %.1f)" x y)
          | [] -> fail "nested: empty polygon")
        f.Osm_fetch.polygons
  | l -> fail "nested: expected 1 feature, got %d" (List.length l)

(* --- osm_fetch: a hole whose first vertex lies exactly on the outer
   boundary must still be assigned (single-point ray cast is unreliable
   there) --- *)

let touching_hole_json =
  {|{
  "elements": [
    {"type": "node", "id": 1, "lat": 0.0, "lon": 0.0},
    {"type": "node", "id": 2, "lat": 0.0, "lon": 10.0},
    {"type": "node", "id": 3, "lat": 10.0, "lon": 10.0},
    {"type": "node", "id": 4, "lat": 10.0, "lon": 0.0},
    {"type": "node", "id": 11, "lat": 5.0, "lon": 10.0},
    {"type": "node", "id": 12, "lat": 4.0, "lon": 8.0},
    {"type": "node", "id": 13, "lat": 5.0, "lon": 6.0},
    {"type": "node", "id": 14, "lat": 6.0, "lon": 8.0},
    {"type": "way", "id": 100, "nodes": [1, 2, 3, 4, 1]},
    {"type": "way", "id": 101, "nodes": [11, 12, 13, 14, 11]},
    {"type": "relation", "id": 700,
     "members": [
       {"type": "way", "ref": 100, "role": "outer"},
       {"type": "way", "ref": 101, "role": "inner"}
     ],
     "tags": {"natural": "water"}}
  ]
}|}

let () =
  match Osm_fetch.parse_overpass_elements touching_hole_json with
  | [ f ] -> (
      match f.Osm_fetch.polygons with
      | [ [ _outer; hole ] ] ->
          if List.length hole <> 5 then
            fail "touching hole: expected 5 pts, got %d" (List.length hole)
      | [ [ _outer ] ] -> fail "touching hole: hole was dropped"
      | _ -> fail "touching hole: unexpected polygon structure")
  | l -> fail "touching hole: expected 1 feature, got %d" (List.length l)

(* --- osm_fetch: a ring referencing a node absent from the response is
   dropped (with a warning), not emitted with vertices deleted --- *)

let () =
  let json =
    {|{"elements": [
      {"type": "node", "id": 1, "lat": 0.0, "lon": 0.0},
      {"type": "node", "id": 2, "lat": 0.0, "lon": 10.0},
      {"type": "node", "id": 3, "lat": 10.0, "lon": 10.0},
      {"type": "way", "id": 100, "nodes": [1, 2, 3, 4, 1],
       "tags": {"natural": "water"}}]}|}
  in
  match Osm_fetch.parse_overpass_elements json with
  | [] -> ()
  | l -> fail "missing node: feature should be dropped, got %d" (List.length l)

(* --- osm_fetch: untagged member relations must not raise --- *)

let () =
  let json =
    {|{"elements": [
      {"type": "relation", "id": 900,
       "members": [{"type": "relation", "ref": 901, "role": ""}],
       "tags": {"natural": "water"}},
      {"type": "relation", "id": 901, "members": []}]}|}
  in
  match Osm_fetch.parse_overpass_elements json with
  | _ -> ()
  | exception e -> fail "untagged relation raised: %s" (Printexc.to_string e)

(* --- Overpass response validation --- *)

let () =
  let check_error name resp =
    match Osm_fetch.response_error resp with
    | Some _ -> ()
    | None -> fail "response_error: %s should be rejected" name
  in
  check_error "remark" {|{"elements": [], "remark": "runtime error: timeout"}|};
  check_error "non-JSON" "<html>rate limited</html>";
  check_error "truncated" {|{"elements": [{"type": "no|};
  (match Osm_fetch.response_error {|{"elements": []}|} with
  | None -> ()
  | Some e -> fail "response_error: clean response rejected: %s" e);
  match Poi_fetch.response_error {|{"remark": "runtime error: timeout"}|} with
  | Some _ -> ()
  | None -> fail "poi response_error: remark should be rejected"

(* --- poi_fetch: elevation and coordinate parsing --- *)

let () =
  let json =
    {|{"elements": [
      {"type": "node", "id": 1, "lat": 45.5, "lon": 6.5,
       "tags": {"natural": "peak", "name": "Test", "ele": "3842"}},
      {"type": "node", "id": 2, "lat": 45.6, "lon": 6.6,
       "tags": {"natural": "peak", "name": "Bad ele", "ele": "ca. 3000"}},
      {"type": "node", "id": 3, "lat": 46, "lon": 7,
       "tags": {"natural": "saddle", "name": "Int coords", "ele": "2001.7"}}]}|}
  in
  match Poi_fetch.parse_overpass_elements json with
  | [ a; b; c ] ->
      if a.Poi_fetch.elevation <> Some 3842 then fail "poi: ele 3842 misparsed";
      if b.Poi_fetch.elevation <> None then
        fail "poi: unparseable ele should be None";
      if c.Poi_fetch.elevation <> Some 2002 then
        fail "poi: 2001.7 should round to 2002";
      if c.Poi_fetch.lat <> 46.0 || c.Poi_fetch.lon <> 7.0 then
        fail "poi: integer coordinates misread";
      if c.Poi_fetch.poi_type <> Poi_fetch.Saddle then fail "poi: wrong type"
  | l -> fail "poi: expected 3 POIs, got %d" (List.length l)
