(** Shared geometric type definitions *)

type ring = { start : int; len : int }
type polygon = { outer : ring; holes : ring array }
type rect = { min_x : float; min_y : float; max_x : float; max_y : float }

(** Validates that polygon indices are within bounds of the vertex array. Raises
    Invalid_argument if checks fail. *)
let validate_polygon (verts : float array) poly =
  let v_len = Array.length verts in
  if v_len mod 2 <> 0 then invalid_arg "Vertices array length must be even";

  (* The logic operates on points (pairs of floats), so the limit is length / 2 *)
  let max_vertices = v_len / 2 in

  let check_ring r label =
    if r.start < 0 || r.len < 0 then
      invalid_arg (label ^ ": start and length must be non-negative");

    (* Indices verify: start + len <= max_vertices.
       We rewrite this as: start > max_vertices - len.
       
       Why? 
       If we did (start + len), large values could overflow to negative, bypassing the check.
       By subtracting len from max_vertices:
       1. If len > max_vertices, the RHS is negative. Since start >= 0, the check (0 > neg) is True. Fails correctly.
       2. Even if len is max_int, (0 - max_int) fits within the integer range without underflow wrapping to positive.
    *)
    if r.start > max_vertices - r.len then
      invalid_arg (label ^ ": indices exceed vertex array bounds")
  in

  check_ring poly.outer "Outer ring";
  Array.iter (fun h -> check_ring h "Hole ring") poly.holes
