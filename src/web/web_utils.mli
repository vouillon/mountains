(** Generic WebGL utilities and math functions. *)

module Gl = Brr_canvas.Gl

val pi : float
(** The constant pi. *)

val next_power_of_two : int -> int -> int
(** [next_power_of_two n min_val] returns the smallest power of two [>= n] and
    [>= min_val]. *)

val log2 : int -> int
(** Base 2 logarithm for integers. *)

val arcsec_floor : float -> int
(** [arcsec_floor coord] is the index of the arc-second cell containing the
    latitude or longitude [coord]. It rounds towards -infinity, unlike
    [truncate]: all tile anchoring must use it so that a position's offset from
    its anchor stays inside the cell for negative coordinates. *)

type buffer =
  | Buffer : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> buffer
      (** Wrapper for Bigarrays to be used with [create_buffer]. *)

val linearize2 : (float, 'a, Bigarray.c_layout) Bigarray.Array2.t -> buffer

val linearize3 : (float, 'a, Bigarray.c_layout) Bigarray.Array3.t -> buffer
(** Linearize 2D/3D Bigarrays into a [buffer]. *)

val measure : track:string -> string -> float -> unit
(** [measure ~track name start] records a User Timing span from [start] (a
    [Performance.now_ms] reading) to now. Chrome's Performance panel shows it on
    track [track] of the "Mountains" group. *)

val timed : track:string -> string -> (unit -> 'a) -> 'a
(** [timed ~track name f] runs [f] and records it as a span. *)

val timed_lwt : track:string -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** [timed_lwt ~track name f] records a span from now until [f ()] resolves.
    Nothing is recorded if it fails. *)

val tiles_url : string -> string
(** [tiles_url path] is the URL of [path] among the tiles of the base grid and
    of the far field's land cover and POIs (compress_dem, extract_tiles), hosted
    on R2 by version: [?tiles=<base url>] in the page's URL overrides the base,
    to try a tile set. *)

val is_cached : Jstr.t -> bool Fut.t
(** [is_cached url] is whether the data cache holds an answer for [url]: a
    successful response, or a 404 -- a tile that does not exist, at sea or past
    the edge of the data, which is as settled as one that does. *)

val fetch_tile :
  ?serve_not_found:bool -> Jstr.t -> Brr_io.Fetch.Response.t Fut.or_error
(** [fetch_tile url] is a tile from the data cache ([Data_cache.name]) when it
    is there, read without going through the service worker, which doubled the
    cost of a cached read (on a Pixel 8a, a location's 345 tiles: 180-230 ms
    from the cache directly, 370-400 ms through the worker). Otherwise it is
    fetched as [Brr_io.Fetch.url] would, through the service worker, which
    stores it -- or, while no service worker controls the page, as on a first
    visit, the page stores it itself. A hit is what the service worker would
    serve: a successful response, or with [serve_not_found] (default [false]) a
    cached 404. *)

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

val prepare_programs : Gl.t -> program_spec list -> unit
(** [prepare_programs ctx specs] submits the compilation and linking of every
    one of [specs] at once, without waiting for any, so that the driver can work
    on them side by side ([KHR_parallel_shader_compile]); {!create_program} then
    takes them. Forgets any prepared earlier, from a context since lost. *)

val programs_ready : Gl.t -> unit Lwt.t
(** [programs_ready ctx] resolves once the GPU process is done with every
    program {!prepare_programs} submitted, the page free meanwhile. *)

val create_program : Gl.t -> program_spec -> Gl.program
(** [create_program ctx spec] creates, links, and returns a GL program from a
    spec -- the one {!prepare_programs} submitted, if it did, waiting for it
    then. *)

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
