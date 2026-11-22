open Bigarray

type request_data =
  | DEM of Jv.t  (** ArrayBuffer containing DEM data *)
  | CLC of { data : Jv.t; pois : bool }
      (** an ArrayBuffer, and whether its POIs are wanted *)

type request =
  | Decode of request_data  (** Worker request type *)
  | Blend of { msg : Jv.t; transfer : Jv.t list }
      (** A chain of ring blends ([Blend_wasm.run_chain_in_worker]): their
          inputs and plans, and the buffers handed over with them *)

type dem_result = { heights : (int, int8_unsigned_elt, c_layout) Array1.t }
(** Decoded DEM heights (raw bytes) *)

type clc_result = Clc_data of Jv.t  (** Decoded CLC data *)

type response =
  | ResultDEM of dem_result
  | ResultHDT of Brr.Tarray.float32
      (** An HDT1 tile's heights in metres, nodata as -99999 *)
  | ResultCLC of clc_result
  | ResultBlend of Jv.t
      (** An array, one per ring blended: its grid's bytes ([data]), its
          [height_scale], [height_offset] and [range], and the call's time
          ([ms]); or [null] for a block with no valid sample, which ends it *)
  | BlendUnavailable  (** The worker has no blend module fit for the call *)
  | Error of string  (** Worker response type *)

type worker_handle
(** Opaque handle to a worker *)

val init : unit -> unit
(** Initialize the worker pool *)

val acquire : unit -> worker_handle Lwt.t
(** Acquire an available worker from the pool *)

val acquire_blender : unit -> worker_handle Lwt.t
(** The worker kept for ring blends ([Blend_wasm.run_chain_in_worker]), apart
    from the pool so that a blend never waits behind tiles. Released with
    {!release} like the others. *)

val release : worker_handle -> unit
(** Release a worker back to the pool *)

val post : worker_handle -> request -> response Lwt.t
(** Post a request to a worker and wait for the response *)
