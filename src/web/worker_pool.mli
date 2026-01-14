open Bigarray

type request_data =
  | DEM of Jv.t  (** ArrayBuffer containing DEM data *)
  | CLC of Jv.t  (** ArrayBuffer containing CLC data *)

type request = Decode of request_data  (** Worker request type *)

type dem_result = { heights : (int, int8_unsigned_elt, c_layout) Array1.t }
(** Decoded DEM heights (raw bytes) *)

type clc_result = Clc_data of Jv.t  (** Decoded CLC data *)

type response =
  | ResultDEM of dem_result
  | ResultCLC of clc_result
  | Error of string  (** Worker response type *)

type worker_handle
(** Opaque handle to a worker *)

val init : unit -> unit
(** Initialize the worker pool *)

val acquire : unit -> worker_handle Lwt.t
(** Acquire an available worker from the pool *)

val release : worker_handle -> unit
(** Release a worker back to the pool *)

val post : worker_handle -> request -> response Lwt.t
(** Post a request to a worker and wait for the response *)
