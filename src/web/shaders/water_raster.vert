#version 300 es
precision highp float;
layout(location = 0) in ivec2 in_pos;      // 24-bit quantized int32
layout(location = 1) in uint in_color_idx; // u8 palette index
uniform vec2 u_tile_range;                 // tile extent in degrees
uniform float u_water_scale;               // quantization scale (220000.0)
uniform vec2 u_tile_min;                   // tile origin
uniform vec2 u_tex_min;                    // DEM min
uniform vec2 u_tex_range;                  // DEM extent
flat out uint v_idx;
void main() {
  // Un-quantize and scale to degrees
  // in_pos is 0..220000 mapping to u_tile_range
  vec2 norm = vec2(in_pos) / u_water_scale;
  vec2 geo_pos = norm * u_tile_range + u_tile_min;

  vec2 ndc = ((geo_pos - u_tex_min) / u_tex_range) * 2.0 - 1.0;
  gl_Position = vec4(ndc, 0.0, 1.0);
  v_idx = in_color_idx;
}
