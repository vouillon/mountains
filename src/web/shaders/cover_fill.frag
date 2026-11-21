#version 300 es
// Fills the holes of a cover-map level, the texels where clc_raster.frag left
// HOLE for the water CORINE has, with the class of the nearest land, by jump
// flooding. The flood runs over cells of 2x2 texels: a cell holds the place
// and the class of a land texel, the nearest found so far.
//   u_step = 0: the seeds, a land texel of each cell that has one;
//   u_step > 0: a pass of the flood, over the cells u_step apart;
//   u_step = -1: the level itself, its holes filled;
//   u_step = -2: the level from [u_far], the far field's land cover as RG8
//   (the palette index in red), [u_shift] texels off and clamped at its edges.
precision highp float;
precision highp int;
precision highp usampler2D;
uniform usampler2D u_cover;  // the level as the polygons drew it
uniform usampler2D u_seeds;  // the cells of the previous pass
uniform usampler2D u_coarse; // the level above, filled
uniform int u_step;
uniform bool u_has_coarse;
uniform uint u_default; // for a hole no land was found for
uniform ivec2 u_shift;
// Normalised rather than integer: on a Pixel 8a, uploading the 2048 x 2048
// grid as RG8UI took 55 ms, as RG8 a fraction of that.
uniform highp sampler2D u_far;
out uint out_value;

const uint HOLE = 255u;
// No land found: no seed has HOLE for a class.
const uint NONE = 0xFFFFFFFFu;

uint pack(ivec2 p, uint c) { return uint(p.x) | (uint(p.y) << 12) | (c << 24); }

// Of the seeds of the cells around [cell], [stride] apart, the nearest to the
// texel position [p].
uint nearest(ivec2 cell, int stride, vec2 p) {
  ivec2 size = textureSize(u_seeds, 0);
  uint best = NONE;
  float best_d = 1e12;
  for (int dy = -1; dy <= 1; dy++) {
    for (int dx = -1; dx <= 1; dx++) {
      ivec2 c = cell + ivec2(dx, dy) * stride;
      if (any(lessThan(c, ivec2(0))) || any(greaterThanEqual(c, size)))
        continue;
      uint s = texelFetch(u_seeds, c, 0).r;
      if (s == NONE)
        continue;
      vec2 d = vec2(float(s & 0xFFFu), float((s >> 12) & 0xFFFu)) - p;
      float dd = dot(d, d);
      if (dd < best_d) {
        best_d = dd;
        best = s;
      }
    }
  }
  return best;
}

void main() {
  ivec2 p = ivec2(gl_FragCoord.xy);
  if (u_step == -2) {
    ivec2 last = textureSize(u_far, 0) - 1;
    out_value = uint(
        texelFetch(u_far, clamp(p + u_shift, ivec2(0), last), 0).r * 255.0 +
        0.5);
  } else if (u_step < 0) {
    uint c = texelFetch(u_cover, p, 0).r;
    if (c == HOLE) {
      uint s = nearest(p / 2, 1, vec2(p));
      c = s == NONE ? u_default : s >> 24;
    }
    out_value = c;
  } else if (u_step == 0) {
    uint seed = NONE;
    for (int i = 3; i >= 0; i--) {
      ivec2 q = 2 * p + ivec2(i & 1, i >> 1);
      uint c = texelFetch(u_cover, q, 0).r;
      if (c != HOLE)
        seed = pack(q, c);
    }
    // A level may lie inside a hole altogether, or see none of the land
    // nearest to its holes: the cells of its border take what the level
    // above, twice as wide around the same centre, has found there.
    ivec2 cells = textureSize(u_cover, 0) / 2;
    if (seed == NONE && u_has_coarse &&
        (p.x == 0 || p.y == 0 || p.x == cells.x - 1 || p.y == cells.y - 1))
      seed = pack(2 * p, texelFetch(u_coarse, p + cells / 2, 0).r);
    out_value = seed;
  } else {
    out_value = nearest(p, u_step, vec2(2 * p) + 0.5);
  }
}
