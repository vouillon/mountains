type t = float array

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

let array m =
  let a = Bigarray.(Array1.create Float32 c_layout (Array.length m)) in
  for i = 0 to Array.length m - 1 do
    a.{i} <- m.(i)
  done;
  a
