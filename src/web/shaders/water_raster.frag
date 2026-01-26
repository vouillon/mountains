#version 300 es
precision mediump float;
flat in uint v_idx;
out uvec4 out_color;
void main() { out_color = uvec4(v_idx, 0u, 0u, 1u); }
