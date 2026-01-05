(** Render state types and utilities for optimized GL uniform handling.

    This module provides:
    - Cached uniform locations to avoid repeated lookups
    - Pre-computed radial grid parameters
    - Helper functions for uploading shared uniforms *)

open Brr_canvas

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

(** Compute radial grid parameters from sector/ring counts. *)
let compute_radial_params ~n_sectors ~n_rings =
  let rec next_power_of_two n p =
    if n <= p then p else next_power_of_two n (p + p)
  in
  let w_stride = next_power_of_two (n_sectors + 1) 1 in
  let w_mask = w_stride - 1 in
  let w_shift =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n lsr 1) in
    log2 w_stride
  in
  let pi = 4. *. atan 1. in
  let grid_k = pi /. float n_sectors in
  let height_term = exp (grid_k *. float (n_rings - 1)) in
  let grid_base = exp grid_k in
  let grid_scale = 70000. /. (height_term -. 1.) in
  {
    w_mask;
    w_shift;
    inv_sectors_div = 1. /. (float n_sectors /. 2.);
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
    - Shadow matrices and splits *)
let upload_session_static ctx (u : terrain_uniforms) ~w ~lat ~x ~y ~lon
    ~light_dir ~shadow_matrices ~shadow_splits =
  let pi = 4. *. atan 1. in
  let deltay = 40_000. /. 360. /. 3600. *. 1000. in
  let deltax = deltay *. cos (lat *. pi /. 180.) in

  (* Tile parameters *)
  Gl.uniform1i ctx u.w w;
  Gl.uniform1f ctx u.inv_w (1. /. float w);
  let max_lod =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
    log2 w
  in
  Gl.uniform1i ctx u.max_lod max_lod;
  Gl.uniform2f ctx u.inv_delta (1. /. deltax) (1. /. deltay);
  let avg_delta = (deltax +. deltay) *. 0.5 in
  Gl.uniform1f ctx u.inv_avg_delta (1. /. avg_delta);

  (* Center offset *)
  let off_x = (lon *. 3600.) -. floor (lon *. 3600.) in
  let off_y = (lat *. 3600.) -. floor (lat *. 3600.) in
  let center_offset_x = deltax *. (float x +. off_x -. 0.5) in
  let center_offset_y = deltay *. (float y +. off_y -. 0.5) in
  Gl.uniform2f ctx u.center_offset center_offset_x center_offset_y;

  (* CLC clipmap parameters *)
  Gl.uniform2f ctx u.u_cameraOffset center_offset_x center_offset_y;
  Gl.uniform1f ctx u.u_baseExtent 2048.0;
  Gl.uniform1i ctx u.u_numLevels 7;

  (* Light direction *)
  Gl.uniform3f ctx u.u_lightDir light_dir.Matrix.x light_dir.Matrix.y
    light_dir.Matrix.z;

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
  Gl.uniform1fv ctx u.shadow_splits (Brr.Tarray.of_bigarray1 splits_ba)

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
