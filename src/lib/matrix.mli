(** Matrix and Vector math for 3D transformations *)

type t = float array
(** A 4x4 matrix stored as a 16-element float array (column-major order) *)

type vector = { x : float; y : float; z : float; w : float }
(** A 4D vector *)

val ( * ) : t -> t -> t
(** Matrix multiplication *)

val mult_into : t -> t -> t -> unit
(** [mult_into dst m1 m2] stores the product [m1 * m2] into [dst], which must
    not alias [m1] or [m2]. Allocation-free variant for the render loop. *)

val ( *< ) : vector -> t -> vector
(** Vector-matrix multiplication (row vector) *)

val ( *> ) : t -> vector -> vector
(** Matrix-vector multiplication (column vector) *)

val scale : float -> float -> float -> t
(** Create a scaling matrix *)

val translate : float -> float -> float -> t
(** Create a translation matrix *)

val rotate_x : float -> t
(** Create a rotation matrix around the X axis *)

val rotate_y : float -> t
(** Create a rotation matrix around the Y axis *)

val rotate_z : float -> t
(** Create a rotation matrix around the Z axis *)

val project : x_scale:float -> y_scale:float -> near_plane:float -> t
(** Create a perspective projection matrix *)

val ortho :
  left:float ->
  right:float ->
  bottom:float ->
  top:float ->
  near:float ->
  far:float ->
  t
(** Create an orthographic projection matrix *)

val look_at : eye:vector -> center:vector -> up:vector -> t
(** Create a look-at view matrix *)

val inverse : t -> t
(** Compute the inverse of a matrix. Only supported for specific transformation
    matrices. *)

val array :
  t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Convert a matrix to a Bigarray for GPU upload *)

val blit :
  t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit
(** Copy matrix content into an existing Bigarray *)
