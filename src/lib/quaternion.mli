type t = { x : float; y : float; z : float; w : float }

val create : float -> float -> float -> float -> t
val identity : t
val normalize : t -> t
val mult : t -> t -> t
val ( * ) : t -> t -> t
val conjugate : t -> t
val from_axis_angle : Matrix.vector -> float -> t
val transform_vector : t -> Matrix.vector -> Matrix.vector
val to_matrix : t -> Matrix.t
val to_euler : t -> float * float * float
