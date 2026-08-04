#version 300 es
precision highp float;
uniform sampler2D source_texture;
uniform vec2 source_size;
uniform float k;
uniform int level;
// Metres per normalised unit, i.e. the grid's metres-per-u16-step times 255,
// since [decode] leaves values in u16/255. Only differences of decoded heights
// are weighted here, so the offset never enters.
uniform float height_scale_n;
in vec2 uv;
out vec2 frag_color;

float decode(vec2 c) { return c.r + c.g * 256.0; }

vec2 encode(float v) {
  v *= 255.;
  float h_high = floor(v / 256.0) / 255.0;
  float h_low = mod(v, 256.0) / 255.0;
  return vec2(h_low, h_high);
}

void main() {
  vec2 size = source_size;
  // Map UV to Source Pixel Coordinates
  // uv points to center of the 2x2 block in source
  ivec2 p = ivec2(uv * size);

  // Force alignment to even coordinates (top-left of 2x2 block)
  ivec2 p00 = (p / 2) * 2;

  ivec2 c00 = clamp(p00, ivec2(0), ivec2(size) - 1);
  ivec2 c10 = clamp(p00 + ivec2(1, 0), ivec2(0), ivec2(size) - 1);
  ivec2 c01 = clamp(p00 + ivec2(0, 1), ivec2(0), ivec2(size) - 1);
  ivec2 c11 = clamp(p00 + ivec2(1, 1), ivec2(0), ivec2(size) - 1);

  // Fetch rg components (Little-Endian height)
  vec2 h00_v = texelFetch(source_texture, c00, level).rg;
  vec2 h10_v = texelFetch(source_texture, c10, level).rg;
  vec2 h01_v = texelFetch(source_texture, c01, level).rg;
  vec2 h11_v = texelFetch(source_texture, c11, level).rg;

  float h00 = decode(h00_v);
  float h10 = decode(h10_v);
  float h01 = decode(h01_v);
  float h11 = decode(h11_v);

  float max_h = max(max(h00, h10), max(h01, h11));
  float h_scale = height_scale_n;

  float w00 = exp(k * (h00 - max_h) * h_scale);
  float w10 = exp(k * (h10 - max_h) * h_scale);
  float w01 = exp(k * (h01 - max_h) * h_scale);
  float w11 = exp(k * (h11 - max_h) * h_scale);

  float sum_exp = w00 + w10 + w01 + w11;
  float avg_exp = sum_exp / 4.;
  float h_avg = max_h + (log(avg_exp) / k / h_scale) + 0.5 / 255.;

  frag_color = encode(h_avg);
}
