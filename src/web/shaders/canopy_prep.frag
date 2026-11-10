// Bakes the canopy texture (Viewer.canopy_texture) from the raw LIDAR HD
// canopy heights, once per block, RGBA8 per 2 m sample, rows from the south:
// - R: height, 0.2 m per step up to 50.8 m, 255 for nodata;
// - G, B: its slope east and north (central differences), 1/32 per step
//   around 128, clamped at +-4: what shades each crown on its sun side;
// - A: crown tone, 1 at a crown's top falling to 0 six metres below the
//   highest canopy within 4 m, and 0 under 2 m of canopy: tops lit, gaps dark.
// The neighbourhood is why this is a bake and not done per terrain pixel.

uniform highp sampler2D u_heights; // R32F, rows north-up as served
uniform int u_size;
uniform float u_step; // metres per sample

out vec4 color;

// Height at sample [p] (rows from the south), clamped to the block; -1 for
// nodata (the service answers -9999, a missing tile reads -99999).
float H(ivec2 p) {
  p = clamp(p, ivec2(0), ivec2(u_size - 1));
  float v = texelFetch(u_heights, ivec2(p.x, u_size - 1 - p.y), 0).r;
  return v < -1000.0 ? -1.0 : max(v, 0.0);
}

// A neighbour for the slope: a missing one stands in as the centre, so there
// is no slope across a nodata edge.
float N(ivec2 p, float v) {
  float w = H(p);
  return w < 0.0 ? v : w;
}

void main() {
  ivec2 p = ivec2(gl_FragCoord.xy);
  float v = H(p);
  if (v < 0.0) {
    color = vec4(1.0, 128.0 / 255.0, 128.0 / 255.0, 0.0);
    return;
  }
  vec2 slope = vec2(N(p + ivec2(1, 0), v) - N(p - ivec2(1, 0), v),
                    N(p + ivec2(0, 1), v) - N(p - ivec2(0, 1), v)) /
               (2.0 * u_step);
  float top = 0.0;
  for (int dy = -2; dy <= 2; dy++)
    for (int dx = -2; dx <= 2; dx++)
      top = max(top, H(p + ivec2(dx, dy)));
  float tone = v < 2.0 ? 0.0 : max(0.0, 1.0 - (top - v) / 6.0);
  color = vec4(min(254.0, floor(v * 5.0 + 0.5)) / 255.0,
               clamp(floor(slope * 32.0 + 128.5), 0.0, 255.0) / 255.0, tone);
}
