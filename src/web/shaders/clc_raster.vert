#version 300 es
precision highp float;
layout(location = 0) in vec2 in_norm_pos;  // u16 normalized to 0..1
layout(location = 1) in uint in_color_idx; // u8 palette index (unsigned)
uniform vec2
    u_tile_range; // tile extent in degrees (65535/scale_x, 65535/scale_y)
uniform vec2 u_tile_min;  // tile origin (min_lon, min_lat)
uniform vec2 u_tex_min;   // DEM min (lon, lat) in degrees
uniform vec2 u_tex_range; // DEM extent (lon_range, lat_range) in degrees
flat out uint v_idx;
void main() {
  // Map normalized u16 (0..1) to geographic coords
  vec2 geo_pos = in_norm_pos * u_tile_range + u_tile_min;
  // Map geographic coords to NDC [-1, 1] for the output texture
  vec2 ndc = ((geo_pos - u_tex_min) / u_tex_range) * 2.0 - 1.0;
  gl_Position = vec4(ndc, 0.0, 1.0);
  v_idx = in_color_idx;
}
