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
