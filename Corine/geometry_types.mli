(** Shared geometric type definitions *)

type ring = { start : int; len : int }
(** A range of indices into a vertex array *)

type polygon = { outer : ring; holes : ring array }
(** A polygon defined by one outer ring and multiple hole rings *)

type rect = { min_x : float; min_y : float; max_x : float; max_y : float }
(** A bounding box / rectangle *)

val validate_polygon : float array -> polygon -> unit
(** Validates that polygon indices are within bounds of the vertex array.
    @raise Invalid_argument if checks fail. *)
