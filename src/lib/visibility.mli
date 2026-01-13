val test :
  (int -> int -> float) ->
  src_x:int ->
  src_y:int ->
  dst_x:int ->
  dst_y:int ->
  bool
(** Test visibility between two points.
    @param get_height function to get height at (row, col)
    @param src_x source column
    @param src_y source row
    @param dst_x destination column
    @param dst_y destination row
    @return true if destination is visible from source *)
