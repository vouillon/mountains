type t = { x : float; y : float; z : float; w : float }

val create : float -> float -> float -> float -> t
val identity : t
val normalize : t -> t
val mult : t -> t -> t
val ( * ) : t -> t -> t
val conjugate : t -> t
val dot : t -> t -> float
val slerp : t -> t -> float -> t
val from_axis_angle : Matrix.vector -> float -> t
val transform_vector : t -> Matrix.vector -> Matrix.vector
val to_matrix : t -> Matrix.t
val angle_between : t -> t -> float
val to_euler : t -> float * float * float
