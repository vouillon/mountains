(* The geometry of a layer of a CLC6 tile (land cover, or water): features
   given as triangles over quantized vertices, coded as the boundaries of the
   features and a walk of their triangles.

   Every feature is a set of triangles that the extraction makes by
   triangulating polygons: no vertex inside, every vertex on a boundary loop
   (a ring), all triangles wound the same way. So the rings give the
   vertices, and the triangles follow from little more:

   - The rings are traced over the half-edges that have no twin, turning
     around each vertex through its fan of triangles, so that a vertex where
     a boundary touches itself (a pinch) is a position of each of its rings.
     The ring positions are the vertices of the decoded feature.

   - Connectivity. The triangles of a connected component, cut along one
     interior edge for each ring past the first (an edge joining two rings
     not yet joined), make a disc: a polygon whose boundary walk [L] goes
     round the rings, along both sides of each cut. A triangulation of a
     polygon without inner vertices is a binary tree hanging from a boundary
     edge, and its preorder with two bits per triangle -- whether it has a
     left child, a right one -- gives each triangle's apex: the left
     subpolygon has as many triangles as the left subtree. The triangle on
     the edge (L.(i), L.(j)) is (L.(i), L.(k), L.(j)), wound as the walk
     goes, where k is i + 1 plus the size of its left subtree.

   - Coordinates. Neighbouring features share their boundaries, point for
     point. A ring is cut into arcs where the feature on the other side
     changes; an arc shared with a feature further on is stored once, with
     its points as deltas, and that feature names it instead.

   The streams of a layer, features in order (the client draws them in that
   order, and the first one drawn on a texel keeps it):

   - codes: per feature, its class code, u16 little-endian.
   - topology (varints): per feature, its number of components; per
     component, its number of rings (a component's rings follow each other)
     and, when more than one, its walk as runs: their number, then per run
     its ring (counted from the component's first), the position it starts
     from and its length. A component of one ring walks round it from its
     first position.
   - arcs (varints): per ring, its number of arcs; per arc, either 4k + 2s
     for an arc of k edges with points of its own, to be stored (as the next
     arc number) when s = 1, or 4i + 2r + 1 for arc i stored before,
     backwards when r = 1. A ring's first point is the start of its first
     arc, and its last arc ends on it.
   - coordinates: per point of an arc of its own, its delta from the point
     before it in the ring (the first point of a ring: from the first point
     of the ring before), modulo [2^bits], zigzagged, one stream per byte,
     most significant first, the x ones then the y ones.
   - connectivity: per component, its triangles in the preorder of the walk's
     tree from the edge between the walk's last position and its first, two
     bits each (1: a left child, 2: a right one), four to a byte from the low
     bits up, with no gap between components. *)

exception Unsupported of string

let fail fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type feature = { code : int; xs : int array; ys : int array; tris : int array }

type layer = {
  count : int;
  verts : int;
  indices : int;
  arcs : int;
  max_walk : int;
  streams : string list;
}

let put_varint b v =
  assert (v >= 0);
  let v = ref v in
  while !v >= 0x80 do
    Buffer.add_char b (Char.chr (!v land 0x7F lor 0x80));
    v := !v lsr 7
  done;
  Buffer.add_char b (Char.chr !v)

(* --- Half-edges: [h = 3t + i] goes from corner [i] of triangle [t] to the
   next one. It also names the corner it leaves from. --- *)

let next h = if h mod 3 = 2 then h - 2 else h + 1
let prev h = if h mod 3 = 0 then h + 2 else h - 1

let twins tris nv =
  let n = Array.length tris in
  let tbl = Hashtbl.create n in
  for h = 0 to n - 1 do
    let u = tris.(h) and v = tris.(next h) in
    if u = v then fail "a triangle repeats a vertex";
    if Hashtbl.mem tbl ((u * nv) + v) then
      fail "a half-edge is in two triangles";
    Hashtbl.replace tbl ((u * nv) + v) h
  done;
  Array.init n (fun h ->
      match Hashtbl.find_opt tbl ((tris.(next h) * nv) + tris.(h)) with
      | Some h' -> h'
      | None -> -1)

(* The boundary loops, as arrays of the half-edges without a twin, where
   [twin] says -1 for none. *)
let trace n twin =
  let nxt = Array.make n (-1) in
  for h = 0 to n - 1 do
    if twin h < 0 then begin
      let h1 = ref (next h) and guard = ref 0 in
      while twin !h1 >= 0 do
        h1 := next (twin !h1);
        incr guard;
        if !guard > n then fail "a vertex inside a feature"
      done;
      nxt.(h) <- !h1
    end
  done;
  let seen = Array.make n false in
  let loops = ref [] in
  for h = 0 to n - 1 do
    if nxt.(h) >= 0 && not seen.(h) then begin
      let loop = ref [] and x = ref h in
      while not seen.(!x) do
        seen.(!x) <- true;
        loop := !x :: !loop;
        x := nxt.(!x)
      done;
      if !x <> h then fail "a boundary loop does not close";
      loops := Array.of_list (List.rev !loop) :: !loops
    end
  done;
  Array.of_list (List.rev !loops)

(* The position in its loop of every corner: [pos.(h)] for the corner [h]
   leaves from, [loop_of.(h)] for the loop; -1 for a corner of no loop. *)
let fans n twin loops =
  let pos = Array.make n (-1) and loop_of = Array.make n (-1) in
  Array.iteri
    (fun l loop ->
      Array.iteri
        (fun p h ->
          pos.(h) <- p;
          loop_of.(h) <- l;
          let x = ref (twin (prev h)) in
          while !x >= 0 do
            if pos.(!x) >= 0 then fail "a fan closes on itself";
            pos.(!x) <- p;
            loop_of.(!x) <- l;
            x := twin (prev !x)
          done)
        loop)
    loops;
  (loop_of, pos)

let rec find par x =
  if par.(x) = x then x
  else begin
    let r = find par par.(x) in
    par.(x) <- r;
    r
  end

(* --- Encoder --- *)

type encoder = {
  bits : int;
  bytes : int; (* per coordinate *)
  codes : Buffer.t;
  topo : Buffer.t;
  arc_ops : Buffer.t;
  coords : Buffer.t array; (* x most significant byte first, then y *)
  conn : Buffer.t;
  mutable mask_count : int;
  mutable mask_byte : int;
  registered : (int * int * int * int, int * int array) Hashtbl.t;
      (* the arcs a feature further on will name: id and points, by their
         first two points, last point and length *)
  mutable arc_count : int;
  mutable cursor : int;
  mutable verts : int;
  mutable tri_count : int;
  mutable max_walk : int;
}

let put_mask e m =
  e.mask_byte <- e.mask_byte lor (m lsl (2 * (e.mask_count land 3)));
  e.mask_count <- e.mask_count + 1;
  if e.mask_count land 3 = 0 then begin
    Buffer.add_char e.conn (Char.chr e.mask_byte);
    e.mask_byte <- 0
  end

let point e x y = (x lsl e.bits) lor y
let px e p = p lsr e.bits
let py e p = p land ((1 lsl e.bits) - 1)

let put_delta e prev p =
  let m = 1 lsl e.bits in
  let put c d =
    let d = ((d mod m) + m) mod m in
    let d = if d >= m lsr 1 then d - m else d in
    let z = if d >= 0 then 2 * d else (-2 * d) - 1 in
    for b = 0 to e.bytes - 1 do
      Buffer.add_char
        e.coords.((c * e.bytes) + b)
        (Char.chr ((z lsr (8 * (e.bytes - 1 - b))) land 0xFF))
    done
  in
  put 0 (px e p - px e prev);
  put 1 (py e p - py e prev)

(* A feature's rings in its own vertices, before the rotations *)
type prepared = {
  f : feature;
  twin : int array;
  rings : int array array; (* boundary half-edges *)
}

let prepare f =
  let nv = Array.length f.xs in
  Array.iter
    (fun v -> if v < 0 || v >= nv then fail "a vertex index out of range")
    f.tris;
  let twin = twins f.tris nv in
  let n = Array.length f.tris in
  let rings = trace n (fun h -> twin.(h)) in
  let used = Array.make nv false in
  Array.iter (fun v -> used.(v) <- true) f.tris;
  if Array.exists not used then fail "a vertex in no triangle";
  (* Every corner on a ring: no vertex inside *)
  let ring_of, _ = fans n (fun h -> twin.(h)) rings in
  if Array.exists (fun r -> r < 0) ring_of then fail "a vertex inside a feature";
  { f; twin; rings }

let encode_feature e fi owner { f; twin; rings } =
  let tris = f.tris in
  let n = Array.length tris in
  let nt = n / 3 in
  let pt v = point e f.xs.(v) f.ys.(v) in
  Buffer.add_char e.codes (Char.chr (f.code land 0xFF));
  Buffer.add_char e.codes (Char.chr (f.code lsr 8));
  (* Rotate each ring to the start of an arc; its arcs as (start, edges,
     feature on the other side) *)
  let label a b =
    match Hashtbl.find_opt owner (b, a) with
    | Some o when o >= 0 && o <> fi && Hashtbl.find_opt owner (a, b) = Some fi
      ->
        o
    | _ -> -1
  in
  let rings, ring_arcs =
    Array.split
    @@ Array.map
         (fun ring ->
           let r = Array.length ring in
           let pts = Array.map (fun h -> pt tris.(h)) ring in
           let lab =
             Array.init r (fun i -> label pts.(i) pts.((i + 1) mod r))
           in
           let cuts =
             List.filter
               (fun i -> lab.(i) <> lab.((i + r - 1) mod r))
               (List.init r Fun.id)
           in
           let cuts =
             if cuts <> [] then cuts
             else if lab.(0) < 0 then [ 0 ]
             else begin
               let m = ref 0 in
               for i = 1 to r - 1 do
                 if pts.(i) < pts.(!m) then m := i
               done;
               [ !m ]
             end
           in
           let rot = List.hd cuts in
           let ring = Array.init r (fun i -> ring.((i + rot) mod r)) in
           let cs = List.map (fun c -> c - rot) cuts @ [ r ] in
           let rec arcs = function
             | c0 :: (c1 :: _ as rest) ->
                 (c0, c1 - c0, lab.((c0 + rot) mod r)) :: arcs rest
             | _ -> []
           in
           (ring, Array.of_list (arcs cs)))
         rings
  in
  let nr = Array.length rings in
  let twin_of h = twin.(h) in
  let ring_of, _ = fans n twin_of rings in
  (* The connected components *)
  let par = Array.init nt Fun.id in
  for h = 0 to n - 1 do
    let h' = twin.(h) in
    if h' >= 0 then par.(find par (h / 3)) <- find par (h' / 3)
  done;
  let comp_of_ring = Array.map (fun ring -> find par (ring.(0) / 3)) rings in
  let comps = ref [] in
  Array.iter
    (fun c -> if not (List.mem c !comps) then comps := c :: !comps)
    comp_of_ring;
  let comps = List.rev !comps in
  (* The cuts: interior edges joining two rings not yet joined *)
  let rpar = Array.init nr Fun.id in
  let cut = Array.make n false in
  for h = 0 to n - 1 do
    let h' = twin.(h) in
    if h' >= 0 && tris.(h) < tris.(next h) then begin
      let a = find rpar ring_of.(h) and b = find rpar ring_of.(next h) in
      if a <> b then begin
        rpar.(a) <- b;
        cut.(h) <- true;
        cut.(h') <- true
      end
    end
  done;
  (* Order the rings by component *)
  let order =
    List.concat_map
      (fun c ->
        List.filter (fun i -> comp_of_ring.(i) = c) (List.init nr Fun.id))
      comps
  in
  let rings = Array.of_list (List.map (fun i -> rings.(i)) order) in
  let ring_arcs = Array.of_list (List.map (fun i -> ring_arcs.(i)) order) in
  let comp_of_ring =
    Array.of_list (List.map (fun i -> comp_of_ring.(i)) order)
  in
  let ring_of, ring_pos = fans n twin_of rings in
  let twin_cut h = if cut.(h) then -1 else twin.(h) in
  let cut_loops = trace n twin_cut in
  if Array.length cut_loops <> List.length comps then
    fail "the cuts leave %d loops for %d components" (Array.length cut_loops)
      (List.length comps);
  put_varint e.topo (List.length comps);
  let ri = ref 0 in
  List.iter
    (fun c ->
      let crings = ref 0 in
      Array.iter (fun c' -> if c' = c then incr crings) comp_of_ring;
      let crings = !crings in
      let loop =
        match
          List.find_opt
            (fun l -> find par (l.(0) / 3) = c)
            (Array.to_list cut_loops)
        with
        | Some l -> l
        | None -> fail "no loop for a component"
      in
      let m = Array.length loop in
      let s = Array.map (fun h -> (ring_of.(h), ring_pos.(h))) loop in
      let s0 =
        let rec go i =
          if i = m then fail "the walk misses its first ring"
          else if s.(i) = (!ri, 0) then i
          else go (i + 1)
        in
        go 0
      in
      let loop = Array.init m (fun i -> loop.((i + s0) mod m)) in
      let s = Array.init m (fun i -> s.((i + s0) mod m)) in
      put_varint e.topo crings;
      if crings = 1 then begin
        if m <> Array.length rings.(!ri) then fail "a ring's walk differs";
        Array.iteri
          (fun i (r, q) ->
            if r <> !ri || q <> i then fail "a ring's walk differs")
          s
      end
      else begin
        let runs = ref [] in
        Array.iter
          (fun (r, q) ->
            match !runs with
            | (r', q', k) :: rest
              when r' = r && (q' + k) mod Array.length rings.(r) = q ->
                runs := (r', q', k + 1) :: rest
            | _ -> runs := (r, q, 1) :: !runs)
          s;
        let runs = List.rev !runs in
        put_varint e.topo (List.length runs);
        List.iter
          (fun (r, q, k) ->
            put_varint e.topo (r - !ri);
            put_varint e.topo q;
            put_varint e.topo k)
          runs
      end;
      ri := !ri + crings;
      e.max_walk <- max e.max_walk m;
      (* The triangles over the walk's positions *)
      let _, wpos = fans n twin_cut [| loop |] in
      let apex = Hashtbl.create m in
      let ntris = ref 0 in
      for t = 0 to nt - 1 do
        if find par t = c then begin
          incr ntris;
          let a = wpos.(3 * t)
          and b = wpos.((3 * t) + 1)
          and d = wpos.((3 * t) + 2) in
          let i, k, j =
            if a < b && a < d then (a, b, d)
            else if b < d then (b, d, a)
            else (d, a, b)
          in
          if not (i < k && k < j) then fail "a triangle against the walk";
          if Hashtbl.mem apex ((i * m) + j) then fail "two triangles on an edge";
          Hashtbl.replace apex ((i * m) + j) k
        end
      done;
      if !ntris <> m - 2 then
        fail "a component of %d triangles for a walk of %d" !ntris m;
      let stack = Stack.create () in
      Stack.push (0, m - 1) stack;
      let count = ref 0 in
      while not (Stack.is_empty stack) do
        let i, j = Stack.pop stack in
        let k =
          match Hashtbl.find_opt apex ((i * m) + j) with
          | Some k -> k
          | None -> fail "no triangle on an edge of the walk"
        in
        incr count;
        let left = k > i + 1 and right = j > k + 1 in
        put_mask e ((if left then 1 else 0) lor if right then 2 else 0);
        if right then Stack.push (k, j) stack;
        if left then Stack.push (i, k) stack
      done;
      if !count <> m - 2 then fail "the walk misses triangles")
    comps;
  (* The rings' points, arc by arc. An arc goes from the point where the
     previous one ended (or, first of its ring, from a point of its own), and
     the last one of a ring back to its first point, which it does not
     repeat. *)
  let key s = (s.(0), s.(1), s.(Array.length s - 1), Array.length s) in
  let find_registered s =
    List.find_map
      (fun (id, s') -> if s' = s then Some id else None)
      (Hashtbl.find_all e.registered (key s))
  in
  Array.iteri
    (fun r ring ->
      let len = Array.length ring in
      let pts = Array.map (fun h -> pt tris.(h)) ring in
      let arcs = ring_arcs.(r) in
      put_varint e.arc_ops (Array.length arcs);
      Array.iteri
        (fun ai (c0, k, lab) ->
          let closing = ai = Array.length arcs - 1 in
          let seq = Array.init (k + 1) (fun t -> pts.((c0 + t) mod len)) in
          let reference =
            if lab >= 0 && lab < fi then
              match
                find_registered (Array.init (k + 1) (fun t -> seq.(k - t)))
              with
              | Some id -> Some (id, 1)
              | None -> Option.map (fun id -> (id, 0)) (find_registered seq)
            else None
          in
          match reference with
          | Some (id, rev) -> put_varint e.arc_ops ((4 * id) + (2 * rev) + 1)
          | None ->
              let register = lab > fi in
              put_varint e.arc_ops ((4 * k) + if register then 2 else 0);
              if register then begin
                if find_registered seq = None then
                  Hashtbl.add e.registered (key seq) (e.arc_count, seq);
                e.arc_count <- e.arc_count + 1
              end;
              if ai = 0 then put_delta e e.cursor pts.(0);
              for t = 1 to if closing then k - 1 else k do
                put_delta e pts.(c0 + t - 1) pts.(c0 + t)
              done)
        arcs;
      e.cursor <- pts.(0))
    rings;
  e.verts <- e.verts + Array.fold_left (fun a r -> a + Array.length r) 0 rings;
  e.tri_count <- e.tri_count + nt

let encode ~bits features =
  let bytes = (bits + 7) / 8 in
  let e =
    {
      bits;
      bytes;
      codes = Buffer.create 1024;
      topo = Buffer.create 1024;
      arc_ops = Buffer.create 1024;
      coords = Array.init (2 * bytes) (fun _ -> Buffer.create 1024);
      conn = Buffer.create 1024;
      mask_count = 0;
      mask_byte = 0;
      registered = Hashtbl.create 1024;
      arc_count = 0;
      cursor = 0;
      verts = 0;
      tri_count = 0;
      max_walk = 0;
    }
  in
  let prepared =
    Array.mapi
      (fun fi f ->
        if f.code < 0 || f.code > 0xFFFF then fail "class code %d" f.code;
        Array.iter
          (fun c -> if c < 0 || c >= 1 lsl bits then fail "coordinate %d" c)
          (Array.append f.xs f.ys);
        try prepare f
        with Unsupported msg -> fail "feature %d (code %d): %s" fi f.code msg)
      features
  in
  (* The boundary edges, as pairs of points, with the feature that has each
     (-2 when there are several) *)
  let owner = Hashtbl.create 65536 in
  Array.iteri
    (fun fi { f; rings; _ } ->
      Array.iter
        (fun ring ->
          let r = Array.length ring in
          Array.iteri
            (fun i h ->
              let a = point e f.xs.(f.tris.(h)) f.ys.(f.tris.(h)) in
              let v = f.tris.(ring.((i + 1) mod r)) in
              let b = point e f.xs.(v) f.ys.(v) in
              Hashtbl.replace owner (a, b)
                (if Hashtbl.mem owner (a, b) then -2 else fi))
            ring)
        rings)
    prepared;
  Array.iteri
    (fun fi p ->
      try encode_feature e fi owner p
      with Unsupported msg -> fail "feature %d (code %d): %s" fi p.f.code msg)
    prepared;
  if e.mask_count land 3 <> 0 then Buffer.add_char e.conn (Char.chr e.mask_byte);
  {
    count = Array.length features;
    verts = e.verts;
    indices = 3 * e.tri_count;
    arcs = e.arc_count;
    max_walk = e.max_walk;
    streams =
      [
        Buffer.contents e.codes;
        Buffer.contents e.topo;
        Buffer.contents e.arc_ops;
      ]
      @ Array.to_list (Array.map Buffer.contents e.coords)
      @ [ Buffer.contents e.conn ];
  }

(* --- Decoder, as decode_clc.wat does it --- *)

let decode ~bits ~count streams =
  let bytes = (bits + 7) / 8 in
  let mask = (1 lsl bits) - 1 in
  let codes, topo, arc_ops, coords, conn =
    match streams with
    | codes :: topo :: arc_ops :: rest when List.length rest = (2 * bytes) + 1
      ->
        let coords =
          Array.of_list (List.filteri (fun i _ -> i < 2 * bytes) rest)
        in
        (codes, topo, arc_ops, coords, List.nth rest (2 * bytes))
    | _ -> invalid_arg "Clc_codec.decode: streams"
  in
  let varint s p =
    let rec go v shift =
      let c = Char.code s.[!p] in
      incr p;
      let v = v lor ((c land 0x7F) lsl shift) in
      if c < 0x80 then v else go v (shift + 7)
    in
    go 0 0
  in
  let tp = ref 0 and ap = ref 0 and cp = ref 0 and mp = ref 0 in
  let delta c =
    let z = ref 0 in
    for b = 0 to bytes - 1 do
      z := (!z lsl 8) lor Char.code coords.((c * bytes) + b).[!cp]
    done;
    (!z lsr 1) lxor -(!z land 1)
  in
  let xs = ref (Array.make 1024 0) and ys = ref (Array.make 1024 0) in
  let nv = ref 0 in
  let add x y =
    if !nv = Array.length !xs then begin
      let grow a = Array.append a (Array.make (Array.length a) 0) in
      xs := grow !xs;
      ys := grow !ys
    end;
    !xs.(!nv) <- x;
    !ys.(!nv) <- y;
    incr nv
  in
  let copy src = add !xs.(src) !ys.(src) in
  let arcs = ref [||] and arc_count = ref 0 in
  let cursor = ref 0 in
  Array.init count (fun fi ->
      let code =
        Char.code codes.[2 * fi] lor (Char.code codes.[(2 * fi) + 1] lsl 8)
      in
      let base = !nv in
      let tris = ref [] in
      for _ = 1 to varint topo tp do
        let nr = varint topo tp in
        let ring_base = Array.make nr 0 and ring_len = Array.make nr 0 in
        for r = 0 to nr - 1 do
          let rb = !nv in
          ring_base.(r) <- rb;
          let narcs = varint arc_ops ap in
          for a = 0 to narcs - 1 do
            let op = varint arc_ops ap in
            let closing = a = narcs - 1 in
            if op land 1 = 0 then begin
              let k = op lsr 2 in
              let read prev =
                let dx = delta 0 and dy = delta 1 in
                incr cp;
                add ((!xs.(prev) + dx) land mask) ((!ys.(prev) + dy) land mask)
              in
              if a = 0 then begin
                let dx = delta 0 and dy = delta 1 in
                incr cp;
                add
                  (((!cursor lsr bits) + dx) land mask)
                  (((!cursor land mask) + dy) land mask)
              end;
              let s = !nv - 1 in
              for _ = 1 to if closing then k - 1 else k do
                read (!nv - 1)
              done;
              if op land 2 <> 0 then begin
                if !arc_count = Array.length !arcs then
                  arcs :=
                    Array.append !arcs
                      (Array.make (max 16 !arc_count) (0, 0, 0));
                !arcs.(!arc_count) <- (s, (if closing then rb else s + k), k);
                incr arc_count
              end
            end
            else begin
              let p0, pk, k = !arcs.(op lsr 2) in
              if op land 2 <> 0 then begin
                if a = 0 then copy pk;
                for t = k - 1 downto 1 do
                  copy (p0 + t)
                done;
                if not closing then copy p0
              end
              else begin
                if a = 0 then copy p0;
                for t = 1 to k - 1 do
                  copy (p0 + t)
                done;
                if not closing then copy pk
              end
            end
          done;
          cursor := (!xs.(rb) lsl bits) lor !ys.(rb);
          ring_len.(r) <- !nv - rb
        done;
        let walk =
          if nr = 1 then Array.init ring_len.(0) (fun t -> ring_base.(0) + t)
          else begin
            let l = ref [] in
            for _ = 1 to varint topo tp do
              let r = varint topo tp in
              let q = varint topo tp in
              let k = varint topo tp in
              for t = 0 to k - 1 do
                l := (ring_base.(r) + ((q + t) mod ring_len.(r))) :: !l
              done
            done;
            Array.of_list (List.rev !l)
          end
        in
        let read_mask () =
          let m = (Char.code conn.[!mp lsr 2] lsr (2 * (!mp land 3))) land 3 in
          incr mp;
          m
        in
        (* walk i: the subtree on the edge from position i, returning the
           position it ends at *)
        let rec go i =
          let m = read_mask () in
          let k = if m land 1 <> 0 then go i else i + 1 in
          let j = if m land 2 <> 0 then go k else k + 1 in
          tris := (walk.(i), walk.(k), walk.(j)) :: !tris;
          j
        in
        if go 0 <> Array.length walk - 1 then failwith "Clc_codec.decode: walk"
      done;
      let n = !nv - base in
      let tris = Array.of_list (List.rev !tris) in
      {
        code;
        xs = Array.sub !xs base n;
        ys = Array.sub !ys base n;
        tris =
          Array.init
            (3 * Array.length tris)
            (fun i ->
              let a, b, c = tris.(i / 3) in
              (match i mod 3 with 0 -> a | 1 -> b | _ -> c) - base);
      })

(* Whether two layers hold the same features: the same class codes and the
   same triangles, as triples of points wound the same way, whatever the
   vertex numbering and the order of the triangles. *)
let differences (a : feature array) (b : feature array) =
  let triangles (f : feature) =
    let p v = (f.xs.(v) lsl 24) lor f.ys.(v) in
    let l =
      List.init
        (Array.length f.tris / 3)
        (fun t ->
          let a = p f.tris.(3 * t)
          and b = p f.tris.((3 * t) + 1)
          and c = p f.tris.((3 * t) + 2) in
          min (a, b, c) (min (b, c, a) (c, a, b)))
    in
    List.sort compare l
  in
  if Array.length a <> Array.length b then
    Some
      (Printf.sprintf "%d features against %d" (Array.length a) (Array.length b))
  else
    let rec go i =
      if i = Array.length a then None
      else if a.(i).code <> b.(i).code then
        Some (Printf.sprintf "feature %d: class code" i)
      else if triangles a.(i) <> triangles b.(i) then
        Some (Printf.sprintf "feature %d: triangles" i)
      else go (i + 1)
    in
    go 0
