(** {!Blend_core.run} as hand-written wasm over linear memory ([blend.wat]),
    which is several times faster because bigarray element access is not
    compiled to a plain load by wasm_of_ocaml today. See PLAN.md. *)

open Bigarray

val start : unit -> unit
(** Begin fetching and instantiating [blend.wasm]. Call once at startup; a
    failure is logged and leaves {!run} on the OCaml implementation. *)

val run :
  Blend_core.params ->
  samples:(float, float32_elt, c_layout) Array1.t ->
  win:(int, int8_unsigned_elt, c_layout) Array1.t ->
  Blend_core.result option
(** Exactly {!Blend_core.run}, to the bit: verified byte for byte on real blocks
    at all three pinned views, including the nodata path. Falls back to
    {!Blend_core.run} when the module is not instantiated yet, so callers need
    no branch of their own. *)

val run_chain_in_worker :
  (Blend_core.params * (float, float32_elt, c_layout) Array1.t) list ->
  win:(int, int8_unsigned_elt, c_layout) Array1.t ->
  (Blend_core.result * float) option list option Lwt.t
(** [run_chain_in_worker rings ~win] blends a chain of rings in a decode worker,
    in one request: [win] is the first one's window of the surface beneath it
    (handed over to the worker, not to be used again), and each later ring
    stands on its predecessor's output, whose height scale and offset replace
    those of its params. Each answer comes with the time of its call, in ms. The
    answers are for the rings up to and including the first with no valid
    sample, after which the plans are no longer right: the rest are left to the
    caller. [None] when no worker has a module fit for the chain. *)
