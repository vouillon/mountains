(** Web-based TIFF reader using browser Fetch API *)

type t
(** Abstract type for the source (a buffer containing the TIFF file) *)

val select : lat:int -> lon:int -> (t -> 'a Lwt.t) -> 'a Lwt.t
(** Fetch a TIFF file for the given tile coordinate and run the callback *)

val prefetch : lat:int -> lon:int -> unit Lwt.t
(** Proactively fetch a TIFF file into browser cache *)

val seek : t -> int -> unit
(** Seek to a position in the buffer *)

val read_string : t -> int -> string Lwt.t
(** Read a string of fixed length from the buffer *)

type chunk = Brr.Tarray.uint8
(** A compressed data chunk (typed array) *)

val read_chunk : t -> int -> chunk Lwt.t
(** Read a compressed chunk from the buffer *)

val inflate : chunk -> bytes -> unit Lwt.t
(** Decompress a chunk into the provided byte buffer using JS zlib *)

val inflate_to_string : chunk -> string Lwt.t
(** Decompress a chunk into a string using JS zlib *)

val uint8_of_string : string -> Brr.Tarray.uint8
(** Create a uint8 typed array from string bytes *)

val read_file : Jstr.t -> string Lwt.t
(** Fetch and read a file as a string *)
