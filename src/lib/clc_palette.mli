(** CLC Palette - CORINE Land Cover material definitions *)

type material = {
  code : int;  (** Official CORINE land cover code *)
  albedo : int * int * int;  (** RGB 0-255 *)
  roughness : float;  (** Surface roughness 0.0-1.0 *)
  detail_rock : float;  (** Rock detail weight *)
  detail_grass : float;  (** Grass detail weight *)
  detail_forest : float;  (** Forest detail weight *)
  water_factor : float;  (** Water blending factor *)
}
(** Material definition with color and texture weights *)

val materials : material array
(** All 45 material definitions *)

val n_materials : int
(** Number of materials *)

val get_index : int -> int
(** Get palette index from CLC code. Returns 0 for unknown codes. *)

val generate_palette :
  unit -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Generate 128x1 RGBA palette texture data. Uses 2 pixels per material:
    - Pixel A: Albedo RGB + Roughness Alpha
    - Pixel B: Detail Weights RGB + Water Factor Alpha *)
