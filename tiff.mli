type t = {
  width : int;
  height : int;
  tile_width : int;
  tile_height : int;
  tile_offsets : int32 array;
  tile_byte_counts : int32 array;
}

val read_info : in_channel -> t

val read_tile :
  in_channel ->
  t ->
  int ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
