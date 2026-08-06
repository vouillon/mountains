type t = { a : float; b : float; c : float; d : float; e : float; f : float }

let apply m u v =
  ((m.a *. u) +. (m.b *. v) +. m.c, (m.d *. u) +. (m.e *. v) +. m.f)

let diagonal ~sx ~sy ~tx ~ty =
  { a = sx; b = 0.; c = tx; d = 0.; e = sy; f = ty }

let det m = (m.a *. m.e) -. (m.b *. m.d)

let compose g f =
  {
    a = (g.a *. f.a) +. (g.b *. f.d);
    b = (g.a *. f.b) +. (g.b *. f.e);
    c = (g.a *. f.c) +. (g.b *. f.f) +. g.c;
    d = (g.d *. f.a) +. (g.e *. f.d);
    e = (g.d *. f.b) +. (g.e *. f.e);
    f = (g.d *. f.c) +. (g.e *. f.f) +. g.f;
  }

let inverse m =
  let dt = det m in
  if dt = 0. then invalid_arg "Affine.inverse: singular";
  let ia = m.e /. dt
  and ib = -.m.b /. dt
  and id = -.m.d /. dt
  and ie = m.a /. dt in
  {
    a = ia;
    b = ib;
    c = -.((ia *. m.c) +. (ib *. m.f));
    d = id;
    e = ie;
    f = -.((id *. m.c) +. (ie *. m.f));
  }
