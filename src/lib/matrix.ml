type t = float array
type vector = { x : float; y : float; z : float; w : float }

let c i j = (i * 4) + j
let o i = (i / 4, i mod 4)

let mult m1 m2 =
  let v p =
    let i, j = o p in
    (m1.(c i 0) *. m2.(c 0 j))
    +. (m1.(c i 1) *. m2.(c 1 j))
    +. (m1.(c i 2) *. m2.(c 2 j))
    +. (m1.(c i 3) *. m2.(c 3 j))
  in
  Array.init 16 v

let ( * ) = mult

let mult v m =
  let s j =
    (v.x *. m.(c 0 j))
    +. (v.y *. m.(c 1 j))
    +. (v.z *. m.(c 2 j))
    +. (v.w *. m.(c 3 j))
  in
  { x = s 0; y = s 1; z = s 2; w = s 3 }

let ( *< ) = mult

let mult m v =
  let s i =
    (m.(c i 0) *. v.x)
    +. (m.(c i 1) *. v.y)
    +. (m.(c i 2) *. v.z)
    +. (m.(c i 3) *. v.w)
  in
  { x = s 0; y = s 1; z = s 2; w = s 3 }

let ( *> ) = mult

let scale x y z : t =
  [| x; 0.; 0.; 0.; 0.; y; 0.; 0.; 0.; 0.; z; 0.; 0.; 0.; 0.; 1. |]

let translate x y z : t =
  [| 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0.; x; y; z; 1. |]

let rotate_x t : t =
  [|
    1.; 0.; 0.; 0.; 0.; cos t; sin t; 0.; 0.; -.sin t; cos t; 0.; 0.; 0.; 0.; 1.;
  |]

let rotate_y t : t =
  [|
    cos t; 0.; -.sin t; 0.; 0.; 1.; 0.; 0.; sin t; 0.; cos t; 0.; 0.; 0.; 0.; 1.;
  |]

let rotate_z t : t =
  [|
    cos t; sin t; 0.; 0.; -.sin t; cos t; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.;
  |]

let project ~x_scale ~y_scale ~near_plane =
  [|
    x_scale;
    0.;
    0.;
    0.;
    0.;
    y_scale;
    0.;
    0.;
    0.;
    0.;
    -1.;
    -1.;
    0.;
    0.;
    -2. *. near_plane;
    0.;
  |]

let ortho ~left ~right ~bottom ~top ~near ~far =
  let rl = right -. left in
  let tb = top -. bottom in
  let fn = far -. near in
  [|
    2. /. rl;
    0.;
    0.;
    0.;
    0.;
    2. /. tb;
    0.;
    0.;
    0.;
    0.;
    -2. /. fn;
    0.;
    -.(right +. left) /. rl;
    -.(top +. bottom) /. tb;
    -.(far +. near) /. fn;
    1.;
  |]

let look_at ~eye ~center ~up =
  let z =
    let v =
      {
        x = eye.x -. center.x;
        y = eye.y -. center.y;
        z = eye.z -. center.z;
        w = 0.;
      }
    in
    let len = sqrt ((v.x ** 2.) +. (v.y ** 2.) +. (v.z ** 2.)) in
    if len = 0. then v
    else { x = v.x /. len; y = v.y /. len; z = v.z /. len; w = 0. }
  in
  let x =
    let v =
      {
        x = (up.y *. z.z) -. (up.z *. z.y);
        y = (up.z *. z.x) -. (up.x *. z.z);
        z = (up.x *. z.y) -. (up.y *. z.x);
        w = 0.;
      }
    in
    let len = sqrt ((v.x ** 2.) +. (v.y ** 2.) +. (v.z ** 2.)) in
    if len = 0. then v
    else { x = v.x /. len; y = v.y /. len; z = v.z /. len; w = 0. }
  in
  let y =
    {
      x = (z.y *. x.z) -. (z.z *. x.y);
      y = (z.z *. x.x) -. (z.x *. x.z);
      z = (z.x *. x.y) -. (z.y *. x.x);
      w = 0.;
    }
  in
  let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z) in
  [|
    x.x;
    y.x;
    z.x;
    0.;
    x.y;
    y.y;
    z.y;
    0.;
    x.z;
    y.z;
    z.z;
    0.;
    -.dot x eye;
    -.dot y eye;
    -.dot z eye;
    1.;
  |]

let inverse m =
  let inv = Array.make 16 0. in

  inv.(0) <-
    (m.(5) *. m.(10) *. m.(15))
    -. (m.(5) *. m.(11) *. m.(14))
    -. (m.(9) *. m.(6) *. m.(15))
    +. (m.(9) *. m.(7) *. m.(14))
    +. (m.(13) *. m.(6) *. m.(11))
    -. (m.(13) *. m.(7) *. m.(10));

  inv.(4) <-
    (-.m.(4) *. m.(10) *. m.(15))
    +. (m.(4) *. m.(11) *. m.(14))
    +. (m.(8) *. m.(6) *. m.(15))
    -. (m.(8) *. m.(7) *. m.(14))
    -. (m.(12) *. m.(6) *. m.(11))
    +. (m.(12) *. m.(7) *. m.(10));

  inv.(8) <-
    (m.(4) *. m.(9) *. m.(15))
    -. (m.(4) *. m.(11) *. m.(13))
    -. (m.(8) *. m.(5) *. m.(15))
    +. (m.(8) *. m.(7) *. m.(13))
    +. (m.(12) *. m.(5) *. m.(11))
    -. (m.(12) *. m.(7) *. m.(9));

  inv.(12) <-
    (-.m.(4) *. m.(9) *. m.(14))
    +. (m.(4) *. m.(10) *. m.(13))
    +. (m.(8) *. m.(5) *. m.(14))
    -. (m.(8) *. m.(6) *. m.(13))
    -. (m.(12) *. m.(5) *. m.(10))
    +. (m.(12) *. m.(6) *. m.(9));

  inv.(1) <-
    (-.m.(1) *. m.(10) *. m.(15))
    +. (m.(1) *. m.(11) *. m.(14))
    +. (m.(9) *. m.(2) *. m.(15))
    -. (m.(9) *. m.(3) *. m.(14))
    -. (m.(13) *. m.(2) *. m.(11))
    +. (m.(13) *. m.(3) *. m.(10));

  inv.(5) <-
    (m.(0) *. m.(10) *. m.(15))
    -. (m.(0) *. m.(11) *. m.(14))
    -. (m.(8) *. m.(2) *. m.(15))
    +. (m.(8) *. m.(3) *. m.(14))
    +. (m.(12) *. m.(2) *. m.(11))
    -. (m.(12) *. m.(3) *. m.(10));

  inv.(9) <-
    (-.m.(0) *. m.(9) *. m.(15))
    +. (m.(0) *. m.(11) *. m.(13))
    +. (m.(8) *. m.(1) *. m.(15))
    -. (m.(8) *. m.(3) *. m.(13))
    -. (m.(12) *. m.(1) *. m.(11))
    +. (m.(12) *. m.(3) *. m.(9));

  inv.(13) <-
    (m.(0) *. m.(9) *. m.(14))
    -. (m.(0) *. m.(10) *. m.(13))
    -. (m.(8) *. m.(1) *. m.(14))
    +. (m.(8) *. m.(2) *. m.(13))
    +. (m.(12) *. m.(1) *. m.(10))
    -. (m.(12) *. m.(2) *. m.(9));

  inv.(2) <-
    (m.(1) *. m.(6) *. m.(15))
    -. (m.(1) *. m.(7) *. m.(14))
    -. (m.(5) *. m.(2) *. m.(15))
    +. (m.(5) *. m.(3) *. m.(14))
    +. (m.(13) *. m.(2) *. m.(7))
    -. (m.(13) *. m.(3) *. m.(6));

  inv.(6) <-
    (-.m.(0) *. m.(6) *. m.(15))
    +. (m.(0) *. m.(7) *. m.(14))
    +. (m.(4) *. m.(2) *. m.(15))
    -. (m.(4) *. m.(3) *. m.(14))
    -. (m.(12) *. m.(2) *. m.(7))
    +. (m.(12) *. m.(3) *. m.(6));

  inv.(10) <-
    (m.(0) *. m.(5) *. m.(15))
    -. (m.(0) *. m.(7) *. m.(13))
    -. (m.(4) *. m.(1) *. m.(15))
    +. (m.(4) *. m.(3) *. m.(13))
    +. (m.(12) *. m.(1) *. m.(7))
    -. (m.(12) *. m.(3) *. m.(5));

  inv.(14) <-
    (-.m.(0) *. m.(5) *. m.(14))
    +. (m.(0) *. m.(6) *. m.(13))
    +. (m.(4) *. m.(1) *. m.(14))
    -. (m.(4) *. m.(2) *. m.(13))
    -. (m.(12) *. m.(1) *. m.(6))
    +. (m.(12) *. m.(2) *. m.(5));

  inv.(3) <-
    (-.m.(1) *. m.(6) *. m.(11))
    +. (m.(1) *. m.(7) *. m.(10))
    +. (m.(5) *. m.(2) *. m.(11))
    -. (m.(5) *. m.(3) *. m.(10))
    -. (m.(9) *. m.(2) *. m.(7))
    +. (m.(9) *. m.(3) *. m.(6));

  inv.(7) <-
    (m.(0) *. m.(6) *. m.(11))
    -. (m.(0) *. m.(7) *. m.(10))
    -. (m.(4) *. m.(2) *. m.(11))
    +. (m.(4) *. m.(3) *. m.(10))
    +. (m.(8) *. m.(2) *. m.(7))
    -. (m.(8) *. m.(3) *. m.(6));

  inv.(11) <-
    (-.m.(0) *. m.(5) *. m.(11))
    +. (m.(0) *. m.(7) *. m.(9))
    +. (m.(4) *. m.(1) *. m.(11))
    -. (m.(4) *. m.(3) *. m.(9))
    -. (m.(8) *. m.(1) *. m.(7))
    +. (m.(8) *. m.(3) *. m.(5));

  inv.(15) <-
    (m.(0) *. m.(5) *. m.(10))
    -. (m.(0) *. m.(6) *. m.(9))
    -. (m.(4) *. m.(1) *. m.(10))
    +. (m.(4) *. m.(2) *. m.(9))
    +. (m.(8) *. m.(1) *. m.(6))
    -. (m.(8) *. m.(2) *. m.(5));

  let det =
    (m.(0) *. inv.(0))
    +. (m.(1) *. inv.(4))
    +. (m.(2) *. inv.(8))
    +. (m.(3) *. inv.(12))
  in

  if det = 0. then Array.make 16 0. (* Handle singular matrix? *)
  else
    let det = 1. /. det in
    for i = 0 to 15 do
      inv.(i) <- inv.(i) *. det
    done;
    inv

let array m =
  let a = Bigarray.(Array1.create Float32 c_layout (Array.length m)) in
  for i = 0 to Array.length m - 1 do
    a.{i} <- m.(i)
  done;
  a
