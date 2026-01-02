(** Sun position calculation *)

val position : lat:float -> lon:float -> time:float -> float * float * float
(** Calculate the sun position vector (East, North, Up) for a given location and
    time. Time is a unix timestamp. *)
