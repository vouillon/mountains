#version 300 es
precision mediump float;
flat in uint v_idx;
out uvec4 out_color;
// The water (palette indices 40 to 44: water courses, water bodies, lagoons,
// estuaries and the sea) is left to the water layer, OSM's, drawn afterwards:
// CORINE has it too wide, and without the smaller islands. It leaves a hole,
// 255, that cover_fill.frag fills with the land around. Where no tile draws
// anything, the level keeps what it was cleared to, the open sea.
void main() { out_color = uvec4(v_idx >= 40u ? 255u : v_idx, 0u, 0u, 1u); }
