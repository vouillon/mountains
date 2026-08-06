(** A 2-D affine map, [(u, v) |-> (a u + b v + c, d u + e v + f)].

    Introduced for the near-field elevation refinements: a ring served in its
    own projected CRS sits on a grid whose axes are turned from north by that
    CRS's grid convergence, so the map between an offset in arcseconds and a
    sample index stops being one scale per axis and becomes this. A
    graticule-aligned ring is the same thing with [b] and [d] zero, so both
    kinds go through one path. *)

type t = { a : float; b : float; c : float; d : float; e : float; f : float }

val apply : t -> float -> float -> float * float

val diagonal : sx:float -> sy:float -> tx:float -> ty:float -> t
(** [(u, v) |-> (sx u + tx, sy v + ty)], the graticule-aligned case. *)

val compose : t -> t -> t
(** [compose g f] applies [f] first. *)

val inverse : t -> t
(** Raises [Invalid_argument] on a singular map, which for the maps this is used
    for would mean a grid with no extent. *)

val det : t -> float
