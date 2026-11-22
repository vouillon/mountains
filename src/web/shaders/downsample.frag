#version 300 es
precision highp float;
uniform sampler2D source_texture;
uniform float k;
// Metres per normalised unit, i.e. the grid's metres-per-u16-step times 255,
// since decoding leaves values in u16/255. Only differences of decoded heights
// are weighted here, so the offset never enters.
uniform float height_scale_n;
out mediump vec2 frag_color;

// Rounds to the nearest u16 step here, so both bytes are exact and the store
// does not depend on how the hardware converts to UNORM.
vec2 encode(float v) {
  v = floor(v * 255.0 + 0.5);
  float h_high = floor(v * (1.0 / 256.0));
  float h_low = v - h_high * 256.0;
  return vec2(h_low, h_high) * (1.0 / 255.0);
}

void main() {
  // Exact 2x2 block origin in the source mip (pinned via TEXTURE_BASE_LEVEL).
  ivec2 p00 = ivec2(gl_FragCoord.xy) * 2;

  vec4 H = vec4(dot(texelFetch(source_texture, p00, 0).rg, vec2(1.0, 256.0)),
                dot(texelFetch(source_texture, p00 + ivec2(1, 0), 0).rg,
                    vec2(1.0, 256.0)),
                dot(texelFetch(source_texture, p00 + ivec2(0, 1), 0).rg,
                    vec2(1.0, 256.0)),
                dot(texelFetch(source_texture, p00 + ivec2(1, 1), 0).rg,
                    vec2(1.0, 256.0)));

  float max_h = max(max(H.x, H.y), max(H.z, H.w));
  float kh2 = k * height_scale_n * 1.4426950408889634;
  vec4 w = exp2((H - max_h) * kh2);
  float h_avg = max_h + log2(dot(w, vec4(0.25))) / kh2;

  frag_color = encode(h_avg);
}
