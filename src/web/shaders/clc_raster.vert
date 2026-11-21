#version 300 es
precision highp float;
layout(location = 0) in uvec2 in_q;        // u16, 0..u_qmax over the sub-cell
layout(location = 1) in uint in_color_idx; // u8 palette index (unsigned)
// Everything in quanta of the tile's grid, from the south-west corner of the
// level's first sub-cell (Clc_loader.sub_cells): the tile's corner is u_cell
// sub-cells from it, so a vertex is u_cell * u_qmax + in_q quanta from it, an
// integer under 2^24 a float holds exactly. A vertex on an edge between two
// sub-cells comes out the same from both, and so does its position: no crack.
uniform vec2 u_cell;      // tile's sub-cell, from the level's first one
uniform float u_qmax;     // quanta across a sub-cell
uniform vec2 u_tex_min;   // level's corner, in quanta
uniform vec2 u_tex_range; // level's extent, in quanta
flat out uint v_idx;
void main() {
  vec2 pos = u_cell * u_qmax + vec2(in_q);
  vec2 ndc = ((pos - u_tex_min) / u_tex_range) * 2.0 - 1.0;
  gl_Position = vec4(ndc, 0.0, 1.0);
  v_idx = in_color_idx;
}
