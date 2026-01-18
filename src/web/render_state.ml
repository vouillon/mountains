(** Render state types and utilities for optimized GL uniform handling.

    This module provides:
    - Cached uniform locations to avoid repeated lookups
    - Pre-computed radial grid parameters
    - Helper functions for uploading shared uniforms
    - Shared constants and utilities *)

open Brr_canvas

(* ========== Shared Constants ========== *)

(** Meters per arc-second at the equator (latitude-independent Y delta). *)
let deltay = 40_000. /. 360. /. 3600. *. 1000.

(* ========== Shared Utilities ========== *)

(** Compute delta values for a given latitude. Returns (deltax, deltay,
    avg_delta) where deltax is longitude-adjusted. *)
let compute_deltas ~lat =
  let deltax = deltay *. cos (lat *. Web_utils.pi /. 180.) in
  let avg_delta = (deltax +. deltay) *. 0.5 in
  (deltax, deltay, avg_delta)

(** Compute sub-arcsecond offset for a coordinate. Returns the fractional part
    within the current arc-second. *)
let compute_sub_arcsec_offset coord = (coord *. 3600.) -. floor (coord *. 3600.)

(** Compute center offset in meters from tile origin. [x] and [y] are
    tile-relative indices, [lat] and [lon] are geographic coords. *)
let compute_center_offset ~lat ~lon ~x ~y =
  let deltax, deltay, _ = compute_deltas ~lat in
  let off_x = compute_sub_arcsec_offset lon in
  let off_y = compute_sub_arcsec_offset lat in
  let center_offset_x = deltax *. (float x +. off_x) in
  let center_offset_y = deltay *. (float y +. off_y) in
  (center_offset_x, center_offset_y)

type radial_params = {
  w_mask : int;
  w_shift : int;
  inv_sectors_div : float;
  grid_k : float;
  grid_base : float;
  grid_scale : float;
}
(** Pre-computed radial grid parameters. These are constant for the session. *)

type terrain_uniforms = {
  (* Radial grid *)
  w_shift : Gl.uniform_location;
  w_mask : Gl.uniform_location;
  inv_sectors_div : Gl.uniform_location;
  grid_k : Gl.uniform_location;
  grid_base : Gl.uniform_location;
  grid_scale : Gl.uniform_location;
  snapped_alpha : Gl.uniform_location;
  (* Tile parameters *)
  center_offset : Gl.uniform_location;
  w : Gl.uniform_location;
  inv_w : Gl.uniform_location;
  max_lod : Gl.uniform_location;
  inv_delta : Gl.uniform_location;
  inv_avg_delta : Gl.uniform_location;
  (* Matrices *)
  proj : Gl.uniform_location;
  transform : Gl.uniform_location;
  (* Texture samplers *)
  relief : Gl.uniform_location;
  ao : Gl.uniform_location;
  u_detailMap : Gl.uniform_location;
  (* Lighting *)
  u_lightDir : Gl.uniform_location;
  (* CLC *)
  u_coverMap : Gl.uniform_location;
  u_paletteTex : Gl.uniform_location;
  u_cameraOffset : Gl.uniform_location;
  u_baseExtent : Gl.uniform_location;
  u_numLevels : Gl.uniform_location;
  u_fogColor : Gl.uniform_location;
  u_zenithColor : Gl.uniform_location;
  (* Shadows *)
  shadow_matrices : Gl.uniform_location;
  shadow_splits : Gl.uniform_location;
  shadow_map : Gl.uniform_location;
}
(** Cached uniform locations for the terrain shader. *)

type shadow_uniforms = {
  (* Radial grid *)
  w_shift : Gl.uniform_location;
  w_mask : Gl.uniform_location;
  inv_sectors_div : Gl.uniform_location;
  grid_k : Gl.uniform_location;
  grid_base : Gl.uniform_location;
  grid_scale : Gl.uniform_location;
  snapped_alpha : Gl.uniform_location;
  (* Tile parameters *)
  center_offset : Gl.uniform_location;
  w : Gl.uniform_location;
  inv_w : Gl.uniform_location;
  max_lod : Gl.uniform_location;
  inv_delta : Gl.uniform_location;
  inv_avg_delta : Gl.uniform_location;
  (* Shadow-specific *)
  relief : Gl.uniform_location;
  shadow_view_proj : Gl.uniform_location;
}
(** Cached uniform locations for the shadow shader. *)

type sky_uniforms = {
  inv_view : Gl.uniform_location;
  u_lightDir : Gl.uniform_location;
  sky_params : Gl.uniform_location; (* x_scale, y_scale, unused, unused *)
  u_fogColor : Gl.uniform_location;
  u_zenithColor : Gl.uniform_location;
}
(** Cached uniform locations for the sky shader. *)

type relief_uniforms = {
  size : Gl.uniform_location;
  delta : Gl.uniform_location;
}
(** Cached uniform locations for the relief shader. *)

type mipmap_uniforms = {
  k : Gl.uniform_location;
  source_texture : Gl.uniform_location;
  source_size : Gl.uniform_location;
}
(** Cached uniform locations for the mipmap shader. *)

type copy_uniforms = {
  source : Gl.uniform_location;
  level : Gl.uniform_location;
  source_size : Gl.uniform_location;
}
(** Cached uniform locations for the copy shader. *)

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

(** Compute radial grid parameters from sector/ring counts. *)
let compute_radial_params ~n_sectors ~n_rings =
  let w_stride = Web_utils.next_power_of_two (n_sectors + 1) 1 in
  let w_mask = w_stride - 1 in
  let w_shift = Web_utils.log2 w_stride in
  let grid_k = Web_utils.pi /. 2. /. float n_sectors in
  let height_term = exp (grid_k *. float (n_rings - 1)) in
  let grid_base = exp grid_k in
  let grid_scale = 70000. /. (height_term -. 1.) in
  {
    w_mask;
    w_shift;
    inv_sectors_div = 1. /. float n_sectors;
    grid_k;
    grid_base;
    grid_scale;
  }

(** Initialize terrain uniform locations. Call once after program creation. *)
let init_terrain_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  {
    w_shift = u "w_shift";
    w_mask = u "w_mask";
    inv_sectors_div = u "inv_sectors_div";
    grid_k = u "grid_k";
    grid_base = u "grid_base";
    grid_scale = u "grid_scale";
    snapped_alpha = u "snapped_alpha";
    center_offset = u "center_offset";
    w = u "w";
    inv_w = u "inv_w";
    max_lod = u "max_lod";
    inv_delta = u "inv_delta";
    inv_avg_delta = u "inv_avg_delta";
    proj = u "proj";
    transform = u "transform";
    relief = u "relief";
    ao = u "ao";
    u_detailMap = u "u_detailMap";
    u_lightDir = u "u_lightDir";
    u_coverMap = u "u_coverMap";
    u_paletteTex = u "u_paletteTex";
    u_cameraOffset = u "u_cameraOffset";
    u_baseExtent = u "u_baseExtent";
    u_numLevels = u "u_numLevels";
    u_fogColor = u "u_fogColor";
    u_zenithColor = u "u_zenithColor";
    shadow_matrices = u "shadow_matrices";
    shadow_splits = u "shadow_splits";
    shadow_map = u "shadow_map";
  }

(** Initialize shadow uniform locations. Call once after program creation. *)
let init_shadow_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  {
    w_shift = u "w_shift";
    w_mask = u "w_mask";
    inv_sectors_div = u "inv_sectors_div";
    grid_k = u "grid_k";
    grid_base = u "grid_base";
    grid_scale = u "grid_scale";
    snapped_alpha = u "snapped_alpha";
    center_offset = u "center_offset";
    w = u "w";
    inv_w = u "inv_w";
    max_lod = u "max_lod";
    inv_delta = u "inv_delta";
    inv_avg_delta = u "inv_avg_delta";
    relief = u "relief";
    shadow_view_proj = u "shadow_view_proj";
  }

(** Initialize sky uniform locations. Call once after program creation. *)
let init_sky_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  {
    inv_view = u "inv_view";
    u_lightDir = u "u_lightDir";
    sky_params = u "sky_params";
    u_fogColor = u "u_fogColor";
    u_zenithColor = u "u_zenithColor";
  }

(** Initialize relief uniform locations. Call once after program creation. *)
let init_relief_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  { size = u "size"; delta = u "delta" }

(** Initialize mipmap uniform locations. Call once after program creation. *)
let init_mipmap_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  {
    k = u "k";
    source_texture = u "source_texture";
    source_size = u "source_size";
  }

(** Initialize copy uniform locations. Call once after program creation. *)
let init_copy_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  { source = u "source"; level = u "level"; source_size = u "source_size" }

(** Initialize AO bake uniform locations. Call once after program creation. *)
let init_ao_bake_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  { relief = u "relief"; width = u "width"; scale = u "scale" }

(** Initialize AO blur uniform locations. Call once after program creation. *)
let init_ao_blur_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  { ao_tex = u "ao_tex"; relief = u "relief"; inv_res = u "inv_res" }

(** Initialize CLC raster uniform locations. Call once after program creation.
*)
let init_clc_raster_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  {
    u_tile_range = u "u_tile_range";
    u_tile_min = u "u_tile_min";
    u_tex_min = u "u_tex_min";
    u_tex_range = u "u_tex_range";
  }

(** Initialize Water raster uniform locations. Call once after program creation.
*)
let init_water_raster_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  {
    u_tile_range = u "u_tile_range";
    u_water_scale = u "u_water_scale";
    u_tile_min = u "u_tile_min";
    u_tex_min = u "u_tex_min";
    u_tex_range = u "u_tex_range";
  }

(** Upload static radial grid uniforms. Call once at initialization. *)
let upload_radial_static ctx (u : terrain_uniforms) (p : radial_params) =
  Gl.uniform1i ctx u.w_shift p.w_shift;
  Gl.uniform1i ctx u.w_mask p.w_mask;
  Gl.uniform1f ctx u.inv_sectors_div p.inv_sectors_div;
  Gl.uniform1f ctx u.grid_k p.grid_k;
  Gl.uniform1f ctx u.grid_base p.grid_base;
  Gl.uniform1f ctx u.grid_scale p.grid_scale

(** Upload static radial grid uniforms for the shadow shader. *)
let upload_radial_static_shadow ctx (u : shadow_uniforms) (p : radial_params) =
  Gl.uniform1i ctx u.w_shift p.w_shift;
  Gl.uniform1i ctx u.w_mask p.w_mask;
  Gl.uniform1f ctx u.inv_sectors_div p.inv_sectors_div;
  Gl.uniform1f ctx u.grid_k p.grid_k;
  Gl.uniform1f ctx u.grid_base p.grid_base;
  Gl.uniform1f ctx u.grid_scale p.grid_scale

(** Upload static texture unit bindings. Call once at initialization. *)
let upload_texture_units ctx (u : terrain_uniforms) =
  Gl.uniform1i ctx u.relief 1;
  Gl.uniform1i ctx u.ao 3;
  Gl.uniform1i ctx u.shadow_map 4;
  Gl.uniform1i ctx u.u_detailMap 5;
  Gl.uniform1i ctx u.u_coverMap 7;
  Gl.uniform1i ctx u.u_paletteTex 8

(** Upload static texture unit bindings for shadow shader. *)
let upload_texture_units_shadow ctx (u : shadow_uniforms) =
  Gl.uniform1i ctx u.relief 0

(** Upload session-static uniforms. Call once after computing initial values.
    These uniforms don't change during the session:
    - Tile parameters (w, inv_w, max_lod, inv_delta, inv_avg_delta,
      center_offset)
    - CLC parameters (u_cameraOffset, u_baseExtent, u_numLevels)
    - Light direction (u_lightDir)
    - Shadow matrices and splits
    - Fog Color *)
let upload_session_static ctx terrain_pid sky_pid shadow_pid
    (u : terrain_uniforms) (sky_u : sky_uniforms) (shadow_u : shadow_uniforms)
    ~w ~lat ~x ~y ~lon ~light_dir ~shadow_matrices ~shadow_splits ~fog_color
    ~zenith_color =
  let deltax, deltay, avg_delta = compute_deltas ~lat in
  let max_lod = Web_utils.log2 w in
  let center_offset_x, center_offset_y =
    compute_center_offset ~lat ~lon ~x ~y
  in

  (* Terrain shader uniforms *)
  Gl.use_program ctx terrain_pid;
  Gl.uniform1i ctx u.w w;
  Gl.uniform1f ctx u.inv_w (1. /. float w);
  Gl.uniform1i ctx u.max_lod max_lod;
  Gl.uniform2f ctx u.inv_delta (1. /. deltax) (1. /. deltay);
  Gl.uniform1f ctx u.inv_avg_delta (1. /. avg_delta);
  Gl.uniform2f ctx u.center_offset center_offset_x center_offset_y;

  (* CLC clipmap parameters *)
  Gl.uniform2f ctx u.u_cameraOffset center_offset_x center_offset_y;
  Gl.uniform1f ctx u.u_baseExtent 2048.0;
  Gl.uniform1i ctx u.u_numLevels 7;

  (* Light direction *)
  Gl.uniform3f ctx u.u_lightDir light_dir.Matrix.x light_dir.Matrix.y
    light_dir.Matrix.z;

  (* Fog & Zenith Color *)
  let r, g, b = fog_color in
  Gl.uniform3f ctx u.u_fogColor r g b;
  let zr, zg, zb = zenith_color in
  Gl.uniform3f ctx u.u_zenithColor zr zg zb;

  (* Shadow matrices and splits *)
  let flat_matrices =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (16 * 3)
  in
  for i = 0 to 2 do
    let m = Matrix.array shadow_matrices.(i) in
    for j = 0 to 15 do
      flat_matrices.{(i * 16) + j} <- m.{j}
    done
  done;
  Gl.uniform_matrix4fv ctx u.shadow_matrices false
    (Brr.Tarray.of_bigarray1 flat_matrices);
  let splits_ba =
    Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout shadow_splits
  in
  Gl.uniform1fv ctx u.shadow_splits (Brr.Tarray.of_bigarray1 splits_ba);

  (* Shadow shader uniforms *)
  Gl.use_program ctx shadow_pid;
  Gl.uniform1i ctx shadow_u.w w;
  Gl.uniform1f ctx shadow_u.inv_w (1. /. float w);
  Gl.uniform1i ctx shadow_u.max_lod max_lod;
  Gl.uniform2f ctx shadow_u.inv_delta (1. /. deltax) (1. /. deltay);
  Gl.uniform1f ctx shadow_u.inv_avg_delta (1. /. avg_delta);
  Gl.uniform2f ctx shadow_u.center_offset center_offset_x center_offset_y;

  (* Sky Uniforms *)
  Gl.use_program ctx sky_pid;
  Gl.uniform3f ctx sky_u.u_lightDir light_dir.Matrix.x (-.light_dir.Matrix.y)
    light_dir.Matrix.z;
  Gl.uniform3f ctx sky_u.u_fogColor r g b;
  Gl.uniform3f ctx sky_u.u_zenithColor zr zg zb

type triangle_uniforms = {
  transform : Gl.uniform_location;
  color : Gl.uniform_location;
}

(** Initialize triangle uniform locations. Call once after program creation. *)
let init_triangle_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  { transform = u "transform"; color = u "color" }

type text_uniforms = { transform : Gl.uniform_location }

(** Initialize text uniform locations. Call once after program creation. *)
let init_text_uniforms ctx pid =
  let u name = Gl.get_uniform_location ctx pid (Jstr.v name) in
  { transform = u "transform" }

let anisotropy_ext = ref None

let apply_anisotropic_filtering ctx =
  match !anisotropy_ext with
  | None -> ()
  | Some (ext, max_val) -> Gl.tex_parameterf ctx Gl.texture_2d ext max_val

let init_anisotropic_filtering ctx =
  let ext_name = Jstr.v "EXT_texture_filter_anisotropic" in
  let ext = Gl.get_extension ctx ext_name in
  if Jv.is_some ext then
    let param = Jv.to_int (Jv.get ext "TEXTURE_MAX_ANISOTROPY_EXT") in
    let max_val_id = Jv.to_int (Jv.get ext "MAX_TEXTURE_MAX_ANISOTROPY_EXT") in
    let max_val = Gl.get_parameter ctx max_val_id in
    anisotropy_ext := Some (param, Jv.to_float max_val)
