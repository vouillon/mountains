(** GeoTIFF loading and decompression *)

module type READER = sig
  type t
  (** Abstract type for the source (e.g. file channel or network fetcher) *)

  val select : lat:int -> lon:int -> (t -> 'a Lwt.t) -> 'a Lwt.t
  (** Select the TIFF file for a given tile coordinate and run the callback *)

  val prefetch : lat:int -> lon:int -> unit Lwt.t
  (** Proactively load a TIFF tile into cache *)

  val seek : t -> int -> unit
  (** Seek to a position in the source *)

  val read_string : t -> int -> string Lwt.t
  (** Read a string of fixed length from the source *)

  type chunk
  (** Abstract type for a compressed data chunk *)

  val read_chunk : t -> int -> chunk Lwt.t
  (** Read a compressed chunk from the source *)

  val inflate : chunk -> bytes -> unit Lwt.t
  (** Decompress a chunk into the provided byte buffer *)
end

type t = {
  width : int;
  height : int;
  tile_width : int;
  tile_height : int;
  tile_offsets : int32 array;
  tile_byte_counts : int32 array;
}
(** GeoTIFF metadata *)

module Make (R : READER) : sig
  val read_info : R.t -> t Lwt.t
  (** Read GeoTIFF metadata from the source *)

  val read_tile :
    R.t ->
    t ->
    int ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t Lwt.t
  (** Read and decompress a specific tile from the GeoTIFF *)
end
