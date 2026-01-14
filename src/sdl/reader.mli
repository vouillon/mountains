(** Native TIFF reader using standard IO channels *)

type t = in_channel
(** Source is a standard input channel *)

val select : lat:int -> lon:int -> (t -> 'a Lwt.t) -> 'a Lwt.t
(** Open a TIFF file for the given tile coordinate and run the callback *)

val prefetch : lat:int -> lon:int -> unit Lwt.t
(** No-op on native (files are on disk) *)

val seek : t -> int -> unit
(** Seek to a position in the file *)

val read_string : t -> int -> string Lwt.t
(** Read a string of fixed length from the file *)

type chunk = string
(** A compressed data chunk (string) *)

val read_chunk : t -> int -> chunk Lwt.t
(** Read a compressed chunk from the file *)

val inflate : chunk -> bytes -> unit Lwt.t
(** Decompress a chunk into the provided byte buffer using Zlib *)
