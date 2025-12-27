type t = float array
type vector = { x : float; y : float; z : float; w : float }

val ( * ) : t -> t -> t
val ( *< ) : vector -> t -> vector
val ( *> ) : t -> vector -> vector
val scale : float -> float -> float -> t
val translate : float -> float -> float -> t
val rotate_x : float -> t
val rotate_y : float -> t
val rotate_z : float -> t
val project : x_scale:float -> y_scale:float -> near_plane:float -> t

val ortho :
  left:float ->
  right:float ->
  bottom:float ->
  top:float ->
  near:float ->
  far:float ->
  t

val look_at : eye:vector -> center:vector -> up:vector -> t
val inverse : t -> t

val array :
  t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
