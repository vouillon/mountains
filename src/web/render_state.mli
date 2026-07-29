(** Render state types and utilities for optimized GL uniform handling. *)

open Brr_canvas

(** {1 Shared Utilities} *)

val compute_deltas : lat:float -> float * float * float
(** [compute_deltas ~lat] returns [(deltax, deltay, avg_delta)] for the given
    latitude. *)

val compute_sub_arcsec_offset : float -> float
(** [compute_sub_arcsec_offset coord] returns the fractional part within the
    current arc-second for a geographic coordinate. *)

val compute_center_offset :
  lat:float -> lon:float -> x:int -> y:int -> float * float
(** [compute_center_offset ~lat ~lon ~x ~y] computes the center offset in meters
    from tile origin. Returns [(center_offset_x, center_offset_y)]. *)

(** {1 Radial Grid Parameters} *)

type radial_params = {
  w_mask : int;
  w_shift : int;
  inv_sectors_div : float;
  grid_k : float;
  grid_scale : float;
}
(** Pre-computed radial grid parameters. These are constant for the session. *)

type terrain_uniforms = {
  w_shift : Gl.uniform_location;
  w_mask : Gl.uniform_location;
  inv_sectors_div : Gl.uniform_location;
  grid_k : Gl.uniform_location;
  grid_scale : Gl.uniform_location;
  snapped_alpha : Gl.uniform_location;
  center_offset : Gl.uniform_location;
  center_height : Gl.uniform_location;
  inv_w : Gl.uniform_location;
  max_lod : Gl.uniform_location;
  inv_delta : Gl.uniform_location;
  inv_avg_delta : Gl.uniform_location;
  proj : Gl.uniform_location;
  transform : Gl.uniform_location;
  relief : Gl.uniform_location;
  ao : Gl.uniform_location;
  u_detailMap : Gl.uniform_location;
  u_lightDir : Gl.uniform_location;
  u_coverMap : Gl.uniform_location;
  u_paletteTex : Gl.uniform_location;
  u_numLevels : Gl.uniform_location;
  u_fogColor : Gl.uniform_location;
  u_zenithColor : Gl.uniform_location;
  shadow_matrices : Gl.uniform_location;
  shadow_splits : Gl.uniform_location;
  shadow_map : Gl.uniform_location;
}
(** Cached uniform locations for the terrain shader. *)

type shadow_uniforms = {
  w_shift : Gl.uniform_location;
  w_mask : Gl.uniform_location;
  inv_sectors_div : Gl.uniform_location;
  grid_k : Gl.uniform_location;
  grid_scale : Gl.uniform_location;
  snapped_alpha : Gl.uniform_location;
  center_offset : Gl.uniform_location;
  inv_w : Gl.uniform_location;
  max_lod : Gl.uniform_location;
  inv_delta : Gl.uniform_location;
  inv_avg_delta : Gl.uniform_location;
  relief : Gl.uniform_location;
  shadow_view_proj : Gl.uniform_location;
}
(** Cached uniform locations for the shadow shader. *)

type sky_uniforms = {
  inv_view : Gl.uniform_location;
  u_lightDir : Gl.uniform_location;
  sky_params : Gl.uniform_location;
  u_fogColor : Gl.uniform_location;
  u_zenithColor : Gl.uniform_location;
}
(** Cached uniform locations for the sky shader. *)

type relief_uniforms = {
  size : Gl.uniform_location;
  delta : Gl.uniform_location;
  uv_scale : Gl.uniform_location;
}
(** Cached uniform locations for the relief shader. *)

type downsample_uniforms = {
  k : Gl.uniform_location;
  source_texture : Gl.uniform_location;
  source_size : Gl.uniform_location;
  level : Gl.uniform_location;
}
(** Cached uniform locations for the downsample shader. *)

type ao_bake_uniforms = {
  relief : Gl.uniform_location;
  width : Gl.uniform_location;
  scale : Gl.uniform_location;
}
(** Cached uniform locations for the AO bake shader. *)

type ao_blur_uniforms = {
  ao_tex : Gl.uniform_location;
  relief : Gl.uniform_location;
  inv_res : Gl.uniform_location;
}
(** Cached uniform locations for the AO blur shader. *)

type clc_raster_uniforms = {
  u_tile_range : Gl.uniform_location;
  u_tile_min : Gl.uniform_location;
  u_tex_min : Gl.uniform_location;
  u_tex_range : Gl.uniform_location;
}
(** Cached uniform locations for the CLC raster shader. *)

type water_raster_uniforms = {
  u_tile_range : Gl.uniform_location;
  u_water_scale : Gl.uniform_location;
  u_tile_min : Gl.uniform_location;
  u_tex_min : Gl.uniform_location;
  u_tex_range : Gl.uniform_location;
}
(** Cached uniform locations for the Water raster shader. *)

val compute_radial_params : n_sectors:int -> n_rings:int -> radial_params
(** Compute radial grid parameters from sector/ring counts. *)

val init_terrain_uniforms : Gl.t -> Gl.program -> terrain_uniforms
val init_shadow_uniforms : Gl.t -> Gl.program -> shadow_uniforms
val init_sky_uniforms : Gl.t -> Gl.program -> sky_uniforms
val init_relief_uniforms : Gl.t -> Gl.program -> relief_uniforms
val init_downsample_uniforms : Gl.t -> Gl.program -> downsample_uniforms
val init_ao_bake_uniforms : Gl.t -> Gl.program -> ao_bake_uniforms
val init_ao_blur_uniforms : Gl.t -> Gl.program -> ao_blur_uniforms
val init_clc_raster_uniforms : Gl.t -> Gl.program -> clc_raster_uniforms
val init_water_raster_uniforms : Gl.t -> Gl.program -> water_raster_uniforms
val upload_radial_static : Gl.t -> terrain_uniforms -> radial_params -> unit

val upload_radial_static_shadow :
  Gl.t -> shadow_uniforms -> radial_params -> unit

val upload_texture_units : Gl.t -> terrain_uniforms -> unit
val upload_texture_units_shadow : Gl.t -> shadow_uniforms -> unit

val upload_session_static :
  Gl.t ->
  Gl.program ->
  Gl.program ->
  Gl.program ->
  terrain_uniforms ->
  sky_uniforms ->
  shadow_uniforms ->
  w:int ->
  lat:float ->
  x:int ->
  y:int ->
  lon:float ->
  light_dir:Matrix.vector ->
  shadow_matrices:Matrix.t array ->
  shadow_splits:float array ->
  fog_color:float * float * float ->
  zenith_color:float * float * float ->
  unit
(** Upload session-static uniforms. *)

type triangle_uniforms = {
  transform : Gl.uniform_location;
  color : Gl.uniform_location;
}

val init_triangle_uniforms : Gl.t -> Gl.program -> triangle_uniforms

type text_uniforms = { transform : Gl.uniform_location }

val init_text_uniforms : Gl.t -> Gl.program -> text_uniforms

val apply_anisotropic_filtering : Gl.t -> unit
(** Apply cached max anisotropy to the currently bound texture. *)

val init_anisotropic_filtering : Gl.t -> unit
(** Detect and initialize anisotropic filtering extension. *)
