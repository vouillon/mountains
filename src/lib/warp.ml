type t = {
  lin : Affine.t;
  xuu : float;
  xuv : float;
  xvv : float;
  yuu : float;
  yuv : float;
  yvv : float;
}

let of_affine lin =
  { lin; xuu = 0.; xuv = 0.; xvv = 0.; yuu = 0.; yuv = 0.; yvv = 0. }

let apply w u v =
  let x, y = Affine.apply w.lin u v in
  let uu = u *. u and uv = u *. v and vv = v *. v in
  ( x +. (w.xuu *. uu) +. (w.xuv *. uv) +. (w.xvv *. vv),
    y +. (w.yuu *. uu) +. (w.yuv *. uv) +. (w.yvv *. vv) )

let is_affine w =
  w.xuu = 0. && w.xuv = 0. && w.xvv = 0. && w.yuu = 0. && w.yuv = 0.
  && w.yvv = 0.

(* A polynomial of degree two in (u, v) as its six coefficients, in the order
   1, u, v, u^2, u v, v^2: enough arithmetic to compose with truncation. *)
type poly = {
  k : float;
  u : float;
  v : float;
  uu : float;
  uv : float;
  vv : float;
}

let poly_add p q =
  {
    k = p.k +. q.k;
    u = p.u +. q.u;
    v = p.v +. q.v;
    uu = p.uu +. q.uu;
    uv = p.uv +. q.uv;
    vv = p.vv +. q.vv;
  }

let poly_scale s p =
  {
    k = s *. p.k;
    u = s *. p.u;
    v = s *. p.v;
    uu = s *. p.uu;
    uv = s *. p.uv;
    vv = s *. p.vv;
  }

(* The product, dropping every term above degree two. *)
let poly_mul p q =
  {
    k = p.k *. q.k;
    u = (p.k *. q.u) +. (p.u *. q.k);
    v = (p.k *. q.v) +. (p.v *. q.k);
    uu = (p.k *. q.uu) +. (p.u *. q.u) +. (p.uu *. q.k);
    uv = (p.k *. q.uv) +. (p.u *. q.v) +. (p.v *. q.u) +. (p.uv *. q.k);
    vv = (p.k *. q.vv) +. (p.v *. q.v) +. (p.vv *. q.k);
  }

let polys w =
  let m = w.lin in
  ( { k = m.Affine.c; u = m.a; v = m.b; uu = w.xuu; uv = w.xuv; vv = w.xvv },
    { k = m.f; u = m.d; v = m.e; uu = w.yuu; uv = w.yuv; vv = w.yvv } )

let of_polys px py =
  {
    lin = { Affine.a = px.u; b = px.v; c = px.k; d = py.u; e = py.v; f = py.k };
    xuu = px.uu;
    xuv = px.uv;
    xvv = px.vv;
    yuu = py.uu;
    yuv = py.uv;
    yvv = py.vv;
  }

let compose g f =
  let fx, fy = polys f in
  let xx = poly_mul fx fx and xy = poly_mul fx fy and yy = poly_mul fy fy in
  let out (k, a, b, quu, quv, qvv) =
    List.fold_left poly_add
      { k; u = 0.; v = 0.; uu = 0.; uv = 0.; vv = 0. }
      [
        poly_scale a fx;
        poly_scale b fy;
        poly_scale quu xx;
        poly_scale quv xy;
        poly_scale qvv yy;
      ]
  in
  let m = g.lin in
  of_polys
    (out (m.Affine.c, m.a, m.b, g.xuu, g.xuv, g.xvv))
    (out (m.f, m.d, m.e, g.yuu, g.yuv, g.yvv))

let translate_input w ~du ~dv =
  compose w (of_affine (Affine.diagonal ~sx:1. ~sy:1. ~tx:du ~ty:dv))

let translate_output w ~dx ~dy =
  compose (of_affine (Affine.diagonal ~sx:1. ~sy:1. ~tx:dx ~ty:dy)) w

let jacobian w u v =
  let m = w.lin in
  ( m.Affine.a +. (2. *. w.xuu *. u) +. (w.xuv *. v),
    m.b +. (w.xuv *. u) +. (2. *. w.xvv *. v),
    m.d +. (2. *. w.yuu *. u) +. (w.yuv *. v),
    m.e +. (w.yuv *. u) +. (2. *. w.yvv *. v) )

(* Gaussian elimination with partial pivoting, on a copy. *)
let solve a b =
  let n = Array.length b in
  let a = Array.map Array.copy a and b = Array.copy b in
  for c = 0 to n - 1 do
    let p = ref c in
    for r = c + 1 to n - 1 do
      if Float.abs a.(r).(c) > Float.abs a.(!p).(c) then p := r
    done;
    if a.(!p).(c) = 0. then invalid_arg "Warp.fit: degenerate samples";
    let t = a.(c) in
    a.(c) <- a.(!p);
    a.(!p) <- t;
    let t = b.(c) in
    b.(c) <- b.(!p);
    b.(!p) <- t;
    for r = c + 1 to n - 1 do
      let f = a.(r).(c) /. a.(c).(c) in
      for k = c to n - 1 do
        a.(r).(k) <- a.(r).(k) -. (f *. a.(c).(k))
      done;
      b.(r) <- b.(r) -. (f *. b.(c))
    done
  done;
  let x = Array.make n 0. in
  for r = n - 1 downto 0 do
    let s = ref b.(r) in
    for k = r + 1 to n - 1 do
      s := !s -. (a.(r).(k) *. x.(k))
    done;
    x.(r) <- !s /. a.(r).(r)
  done;
  x

let fit samples =
  let n = float (Array.length samples) in
  let mean f = Array.fold_left (fun s p -> s +. f p) 0. samples /. n in
  let cu = mean (fun (u, _, _, _) -> u) and cv = mean (fun (_, v, _, _) -> v) in
  let spread f c =
    Array.fold_left (fun s p -> Float.max s (Float.abs (f p -. c))) 0. samples
  in
  let su = spread (fun (u, _, _, _) -> u) cu
  and sv = spread (fun (_, v, _, _) -> v) cv in
  if su = 0. || sv = 0. then invalid_arg "Warp.fit: degenerate samples";
  let basis p q = [| 1.; p; q; p *. p; p *. q; q *. q |] in
  let ata = Array.make_matrix 6 6 0. in
  let atx = Array.make 6 0. and aty = Array.make 6 0. in
  Array.iter
    (fun (u, v, x, y) ->
      let r = basis ((u -. cu) /. su) ((v -. cv) /. sv) in
      for i = 0 to 5 do
        atx.(i) <- atx.(i) +. (r.(i) *. x);
        aty.(i) <- aty.(i) +. (r.(i) *. y);
        for j = 0 to 5 do
          ata.(i).(j) <- ata.(i).(j) +. (r.(i) *. r.(j))
        done
      done)
    samples;
  let px = solve ata atx and py = solve ata aty in
  let poly c =
    { k = c.(0); u = c.(1); v = c.(2); uu = c.(3); uv = c.(4); vv = c.(5) }
  in
  (* Back from the centred, scaled coordinates the fit was solved in. *)
  compose
    (of_polys (poly px) (poly py))
    (of_affine
       (Affine.diagonal ~sx:(1. /. su) ~sy:(1. /. sv) ~tx:(-.cu /. su)
          ~ty:(-.cv /. sv)))
