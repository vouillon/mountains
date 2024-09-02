module type READER = sig
  type t

  val select : lat:int -> lon:int -> (t -> 'a Lwt.t) -> 'a Lwt.t
  val seek : t -> int -> unit
  val read_string : t -> int -> string Lwt.t

  type chunk

  val read_chunk : t -> int -> chunk Lwt.t
  val inflate : chunk -> bytes -> unit Lwt.t
end

type t = {
  width : int;
  height : int;
  tile_width : int;
  tile_height : int;
  tile_offsets : int32 array;
  tile_byte_counts : int32 array;
}

module Make (R : READER) : sig
  val read_info : R.t -> t Lwt.t

  val read_tile :
    R.t ->
    t ->
    int ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t Lwt.t
end
