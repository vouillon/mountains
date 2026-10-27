(** A 2-D map of degree two: {!Affine} plus quadratic terms in each output.

    Introduced for the near-field elevation refinements served in a projected
    CRS. Between arcseconds from an anchor and the sample index of such a grid
    no affine map is exact: a second of longitude shortens with latitude, and
    the projection's grid convergence changes across the block. Over the 5 km of
    the middle LIDAR ring the best affine fit still misplaces the corners by
    over a metre; a quadratic one is good to a tenth of a millimetre. A grid on
    the graticule is the same thing with the quadratic terms zero, so both kinds
    go through one path. *)

type t = {
  lin : Affine.t;  (** the constant and linear terms *)
  xuu : float;  (** first output: coefficients of [u^2], [u v], [v^2] *)
  xuv : float;
  xvv : float;
  yuu : float;  (** second output: the same *)
  yuv : float;
  yvv : float;
}

val of_affine : Affine.t -> t
val apply : t -> float -> float -> float * float

val is_affine : t -> bool
(** Whether every quadratic term is exactly zero. *)

val compose : t -> t -> t
(** [compose g f] applies [f] first, truncated to degree two. Exact whenever
    either map is affine; otherwise the dropped terms are products of two
    curvatures, far below anything the maps this is used for can resolve. *)

val translate_input : t -> du:float -> dv:float -> t
(** [translate_input w ~du ~dv] is [fun (u, v) -> w (u + du, v + dv)], exactly.
*)

val translate_output : t -> dx:float -> dy:float -> t
(** [w] followed by adding [(dx, dy)], exactly. *)

val jacobian : t -> float -> float -> float * float * float * float
(** Partial derivatives at a point: [(dx/du, dx/dv, dy/du, dy/dv)]. *)

val fit : (float * float * float * float) array -> t
(** Least-squares fit to [(u, v, x, y)] samples. Solved in coordinates centred
    and scaled over the samples, so the normal equations stay well conditioned
    whatever the offsets. Needs at least six samples not on one conic. *)
