
uniform vec2 size;
uniform vec2 delta;
uniform vec2 uv_scale;
uniform sampler2D tile;
// The grid's own quantisation. Each blended refinement spans only the height
// range it holds, so this is not the base tile's 9500 m over 65536 steps; see
// [Hd_dem.blend]. Decoded and re-encoded with the same pair, so the pyramid
// inherits the grid's scale.
uniform highp float height_scale;  // metres per u16 step
uniform highp float height_offset; // metres at u16 zero
in vec2 uv;
// Two render targets: heights (16-bit fixed point, little-endian) and the
// encoded normal go to separate RG8 textures. Every consumer reads only one of
// the two pairs (heights via texelFetch in the vertex shaders and the AO
// passes' NEAREST sampler, normals only through terrain.frag's filtered fetch),
// so splitting halves the bytes per tap at identical total memory.
layout(location = 0) out mediump vec2 height_out;
layout(location = 1) out mediump vec2 normal_out;

float get_z(vec2 offset) {
  vec2 tileCoord = uv * (size - 1.0) + 0.5;
  // Decode from RG8: R=low byte, G=high byte (little-endian)
  // Samples are in [0, 1], need to multiply by 255 to get 0..255
  vec2 rg = texture(tile, ((tileCoord + offset) / size) * uv_scale).rg * 255.0;
  float h_val = rg.g * 256.0 + rg.r;
  return h_val * height_scale + height_offset;
}

void main() {
  // Sobel filter
  float tl = get_z(vec2(-1, -1));
  float t = get_z(vec2(0, -1));
  float tr = get_z(vec2(1, -1));
  float l = get_z(vec2(-1, 0));
  float c = get_z(vec2(0, 0));
  float r = get_z(vec2(1, 0));
  float bl = get_z(vec2(-1, 1));
  float b = get_z(vec2(0, 1));
  float br = get_z(vec2(1, 1));

  float dX = tr + 2.0 * r + br - (tl + 2.0 * l + bl);
  float dY = bl + 2.0 * b + br - (tl + 2.0 * t + tr);

  // Normal vector
  // Note: dX is dHeight/dPixelX * 8 (scaling of Sobel).
  // We divide by (8 * deltax) to get slope.
  vec3 n = normalize(vec3(-dX / (8.0 * delta.x), -dY / (8.0 * delta.y), 1.0));

  // Encode Normal (xy components to [0,1])
  vec2 encN = n.xy * 0.5 + 0.5;

  float h_val = (c - height_offset) / height_scale;
  float h_high = floor(h_val / 256.0) / 255.0;
  float h_low = mod(h_val, 256.0) / 255.0;

  // Little-Endian: R=Low, G=High
  height_out = vec2(h_low, h_high);
  normal_out = encN;
}
