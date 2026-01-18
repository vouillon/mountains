(** Generic WebGL utilities and math functions. *)

module Gl = Brr_canvas.Gl

val pi : float
(** The constant pi. *)

val next_power_of_two : int -> int -> int
(** [next_power_of_two n min_val] returns the smallest power of two [>= n] and
    [>= min_val]. *)

val log2 : int -> int
(** Base 2 logarithm for integers. *)

type buffer =
  | Buffer : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> buffer
      (** Wrapper for Bigarrays to be used with [create_buffer]. *)

val linearize2 : (float, 'a, Bigarray.c_layout) Bigarray.Array2.t -> buffer

val linearize3 : (float, 'a, Bigarray.c_layout) Bigarray.Array3.t -> buffer
(** Linearize 2D/3D Bigarrays into a [buffer]. *)

val on_gpu_finished : Gl.t -> unit Lwt.t
(** [on_gpu_finished ctx] returns a promise that resolves when all currently
    submitted GPU commands have completed. *)

val create_buffer : Gl.t -> int -> buffer -> Gl.buffer
(** [create_buffer ctx target data] creates and initializes a GL buffer. *)

val create_geometry :
  Gl.t ->
  indices:(_, _, Bigarray.c_layout) Bigarray.Array1.t ->
  buffers:(int * int * int * buffer) list ->
  Gl.vertex_array_object
(** [create_geometry ctx ~indices ~buffers] creates a VAO with the given indices
    and vertex attribute buffers. Each buffer is a tuple of (location,
    dimension, data). *)

val compile_shader : Gl.t -> string -> int -> Gl.shader
(** [compile_shader ctx src type] compiles a shader of the given type. *)

type program_spec = {
  vertex_shader : string;
  fragment_shader : string;
  attributes : string list;
}
(** Specification for a GL program. *)

val create_program : Gl.t -> program_spec -> Gl.program
(** [create_program ctx spec] creates, links, and returns a GL program from a
    spec. *)

val intersects :
  float * float * float * float ->
  float * float * float * float * float * float ->
  bool
(** [intersects (min_lon, min_lat, max_lon, max_lat)  (v_min_lon, v_min_lat, _,
     _, v_max_lon, v_max_lat)] checks if two AABBs intersect. *)

val set_texture_params_nearest_clamp : Gl.t -> int -> unit
(** Set texture parameters for nearest filtering with clamp-to-edge wrapping. *)

val set_texture_params_linear_clamp : Gl.t -> int -> unit
(** Set texture parameters for linear filtering with clamp-to-edge wrapping. *)

val set_texture_params_mipmap_repeat : Gl.t -> int -> unit
(** Set texture parameters for linear mipmapped filtering with repeat wrapping.
*)
