type t

val ( * ) : t -> t -> t
val scale : float -> float -> float -> t
val translate : float -> float -> float -> t
val rotate_x : float -> t
val rotate_y : float -> t
val rotate_z : float -> t
val project : x_scale:float -> y_scale:float -> near_plane:float -> t

val array :
  t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
