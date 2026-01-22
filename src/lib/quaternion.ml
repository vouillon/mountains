type t = { x : float; y : float; z : float; w : float }

let create x y z w = { x; y; z; w }
let identity = { x = 0.; y = 0.; z = 0.; w = 1. }

let normalize t =
  let len =
    sqrt ((t.x *. t.x) +. (t.y *. t.y) +. (t.z *. t.z) +. (t.w *. t.w))
  in
  if len = 0. then identity
  else { x = t.x /. len; y = t.y /. len; z = t.z /. len; w = t.w /. len }

let mult a b =
  {
    x = (a.w *. b.x) +. (a.x *. b.w) +. (a.y *. b.z) -. (a.z *. b.y);
    y = (a.w *. b.y) -. (a.x *. b.z) +. (a.y *. b.w) +. (a.z *. b.x);
    z = (a.w *. b.z) +. (a.x *. b.y) -. (a.y *. b.x) +. (a.z *. b.w);
    w = (a.w *. b.w) -. (a.x *. b.x) -. (a.y *. b.y) -. (a.z *. b.z);
  }

let ( * ) = mult
let conjugate t = { x = -.t.x; y = -.t.y; z = -.t.z; w = t.w }

let from_axis_angle axis angle =
  let half_angle = angle *. 0.5 in
  let s = sin half_angle in
  let len =
    sqrt ((axis.Matrix.x *. axis.x) +. (axis.y *. axis.y) +. (axis.z *. axis.z))
  in
  if len = 0. then identity
  else
    let scale = s /. len in
    {
      x = axis.x *. scale;
      y = axis.y *. scale;
      z = axis.z *. scale;
      w = cos half_angle;
    }

let transform_vector q v =
  let qx = q.x and qy = q.y and qz = q.z and w = q.w in
  let vx = v.Matrix.x and vy = v.y and vz = v.z in
  let uvx = (qy *. vz) -. (qz *. vy) in
  let uvy = (qz *. vx) -. (qx *. vz) in
  let uvz = (qx *. vy) -. (qy *. vx) in
  let uuvx = (qy *. uvz) -. (qz *. uvy) in
  let uuvy = (qz *. uvx) -. (qx *. uvz) in
  let uuvz = (qx *. uvy) -. (qy *. uvx) in
  let w_uvx = w *. uvx in
  let w_uvy = w *. uvy in
  let w_uvz = w *. uvz in
  {
    Matrix.x = vx +. (2. *. (w_uvx +. uuvx));
    y = vy +. (2. *. (w_uvy +. uuvy));
    z = vz +. (2. *. (w_uvz +. uuvz));
    w = v.Matrix.w;
  }

let to_matrix t =
  let xx = t.x *. t.x in
  let yy = t.y *. t.y in
  let zz = t.z *. t.z in
  let xy = t.x *. t.y in
  let xz = t.x *. t.z in
  let yz = t.y *. t.z in
  let wx = t.w *. t.x in
  let wy = t.w *. t.y in
  let wz = t.w *. t.z in

  [|
    1. -. (2. *. (yy +. zz));
    2. *. (xy +. wz);
    2. *. (xz -. wy);
    0.;
    2. *. (xy -. wz);
    1. -. (2. *. (xx +. zz));
    2. *. (yz +. wx);
    0.;
    2. *. (xz +. wy);
    2. *. (yz -. wx);
    1. -. (2. *. (xx +. yy));
    0.;
    0.;
    0.;
    0.;
    1.;
  |]

let to_euler t =
  (* Roll (x-axis rotation) *)
  let sinr_cosp = 2.0 *. ((t.w *. t.x) +. (t.y *. t.z)) in
  let cosr_cosp = 1.0 -. (2.0 *. ((t.x *. t.x) +. (t.y *. t.y))) in
  let roll = atan2 sinr_cosp cosr_cosp in

  (* Pitch (y-axis rotation) *)
  let sinp = 2.0 *. ((t.w *. t.y) -. (t.z *. t.x)) in
  let pitch =
    if abs_float sinp >= 1.0 then
      copysign (Float.pi /. 2.0) sinp (* use 90 degrees if out of range *)
    else asin sinp
  in

  (* Yaw (z-axis rotation) *)
  let siny_cosp = 2.0 *. ((t.w *. t.z) +. (t.x *. t.y)) in
  let cosy_cosp = 1.0 -. (2.0 *. ((t.y *. t.y) +. (t.z *. t.z))) in
  let yaw = atan2 siny_cosp cosy_cosp in

  (roll, pitch, yaw)
