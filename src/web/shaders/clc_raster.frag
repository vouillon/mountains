#version 300 es
precision mediump float;
flat in uint v_idx;
out uvec4 out_color;
// Water courses and water bodies (palette indices 40 and 41) are left to the
// OSM water layer, drawn afterwards: CORINE has them too wide, and without
// their smaller islands. They leave a hole, 255, that cover_fill.frag fills
// with the land around.
void main() {
  out_color = uvec4(v_idx == 40u || v_idx == 41u ? 255u : v_idx, 0u, 0u, 1u);
}
