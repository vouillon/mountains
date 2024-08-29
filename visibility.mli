val test :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  src_x:int ->
  src_y:int ->
  dst_x:int ->
  dst_y:int ->
  bool
