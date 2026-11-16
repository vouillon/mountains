(* Round trips through Clc_codec, on features made to have what the tiles
   have: borders shared with a neighbour, a hole, several components, a
   boundary touching itself at a vertex. Run via `dune runtest`. Silent on
   success; fails loudly otherwise. *)

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("FAIL: " ^ s);
      exit 1)
    fmt

(* A feature from points and counterclockwise triangles *)
let feature code pts tris =
  {
    Clc_codec.code;
    xs = Array.of_list (List.map fst pts);
    ys = Array.of_list (List.map snd pts);
    tris = Array.of_list (List.concat_map (fun (a, b, c) -> [ a; b; c ]) tris);
  }

(* Unit squares of a grid, two triangles each *)
let cells code ?(scale = 10) ?(x0 = 0) ?(y0 = 0) squares =
  let index = Hashtbl.create 16 and pts = ref [] in
  let vertex x y =
    match Hashtbl.find_opt index (x, y) with
    | Some i -> i
    | None ->
        let i = Hashtbl.length index in
        Hashtbl.add index (x, y) i;
        pts := (x0 + (scale * x), y0 + (scale * y)) :: !pts;
        i
  in
  let tris =
    List.concat_map
      (fun (x, y) ->
        let a = vertex x y
        and b = vertex (x + 1) y
        and c = vertex (x + 1) (y + 1)
        and d = vertex x (y + 1) in
        [ (a, b, c); (a, c, d) ])
      squares
  in
  feature code (List.rev !pts) tris

let square = cells 211 [ (0, 0) ]

(* Square's neighbour to the east: their common side is an arc *)
let east = cells 311 ~x0:10 [ (0, 0); (1, 0); (0, 1) ]

(* A square with a square hole *)
let annulus =
  feature 312
    [
      (100, 0);
      (130, 0);
      (130, 30);
      (100, 30);
      (110, 10);
      (120, 10);
      (120, 20);
      (110, 20);
    ]
    [
      (0, 1, 5);
      (0, 5, 4);
      (1, 2, 6);
      (1, 6, 5);
      (2, 3, 7);
      (2, 7, 6);
      (3, 0, 4);
      (3, 4, 7);
    ]

(* Two triangles apart, and two touching at a vertex *)
let apart =
  feature 512
    [ (0, 100); (5, 100); (0, 105); (50, 150); (55, 150); (50, 155) ]
    [ (0, 1, 2); (3, 4, 5) ]

let bowtie =
  feature 512
    [ (200, 0); (210, 0); (205, 5); (210, 10); (200, 10) ]
    [ (0, 1, 2); (2, 3, 4) ]

(* A U closed at the top but for a corner: the hole it encloses touches the
   outside at a vertex, which its one component goes through twice *)
let pinched =
  cells 231 ~x0:300 [ (0, 0); (1, 0); (2, 0); (0, 1); (2, 1); (0, 2); (1, 2) ]

let check ~bits name features =
  let layer = Clc_codec.encode ~bits features in
  let back = Clc_codec.decode ~bits ~count:layer.count layer.streams in
  (match Clc_codec.differences features back with
  | None -> ()
  | Some d -> fail "%s: %s" name d);
  layer

let () =
  let layer =
    check ~bits:16 "land" [| square; east; annulus; apart; bowtie; pinched |]
  in
  (* The square's eastern side, stored once, and named by its neighbour *)
  if layer.arcs <> 1 then fail "land: %d arcs stored, not 1" layer.arcs;
  ignore (check ~bits:24 "water" [| pinched; annulus; square |]);
  ignore (check ~bits:16 "empty" [||])

(* A vertex inside a feature cannot be coded *)
let () =
  let fan =
    feature 211
      [ (0, 0); (10, 0); (10, 10); (0, 10); (5, 5) ]
      [ (0, 1, 4); (1, 2, 4); (2, 3, 4); (3, 0, 4) ]
  in
  match Clc_codec.encode ~bits:16 [| fan |] with
  | _ -> fail "a vertex inside a feature was coded"
  | exception Clc_codec.Unsupported _ -> ()
