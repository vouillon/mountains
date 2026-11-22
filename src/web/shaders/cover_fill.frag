#version 300 es
// Fills the holes of a cover-map level, the texels where clc_raster.frag left
// HOLE for the water CORINE has, with the class of the nearest land, by jump
// flooding. The flood runs over cells of 2x2 texels: a cell holds the place
// and the class of a land texel, the nearest found so far.
//   u_step = 0: the seeds, a land texel of each cell that has one;
//   u_step > 0: a pass of the flood, over the cells u_step apart;
//   u_step = -1: the level itself, its holes filled, and the ground whose land
//   cover is not loaded yet (UNLOADED) taken from the level above;
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
// Ground whose tiles a first view goes without: neither land nor a hole, it
// takes what the level above has there, down from the far field's own grid.
const uint UNLOADED = 254u;
// No land found: no seed has HOLE for a class.
const uint NONE = 0xFFFFFFFFu;

uint pack(ivec2 p, uint c) { return uint(p.x) | (uint(p.y) << 12) | (c << 24); }

void updateBest(uint s, vec2 target, inout uint best, inout float best_d) {
  if (s != NONE) {
    vec2 d = vec2(float(s & 0xFFFu), float((s >> 12) & 0xFFFu)) - target;
    float dd = dot(d, d);
    if (dd < best_d) {
      best_d = dd;
      best = s;
    }
  }
}

void main() {
  ivec2 p = ivec2(gl_FragCoord.xy);
  if (u_step == -2) {
    ivec2 last = textureSize(u_far, 0) - 1;
    out_value = uint(
        texelFetch(u_far, clamp(p + u_shift, ivec2(0), last), 0).r * 255.0 +
        0.5);
    return;
  }
  if (u_step == 0) {
    ivec2 q0 = p * 2;
    // Pick the first valid (non-HOLE, non-UNLOADED) texel in 0..3 order: on
    // land cells (the common case) the first fetch already succeeds.
    uint c0 = texelFetch(u_cover, q0, 0).r;
    if (c0 < UNLOADED) {
      out_value = pack(q0, c0);
      return;
    }
    uint c1 = texelFetch(u_cover, q0 + ivec2(1, 0), 0).r;
    uint c2 = texelFetch(u_cover, q0 + ivec2(0, 1), 0).r;
    uint c3 = texelFetch(u_cover, q0 + ivec2(1, 1), 0).r;
    uint seed = NONE;
    if (c1 < UNLOADED)
      seed = pack(q0 + ivec2(1, 0), c1);
    else if (c2 < UNLOADED)
      seed = pack(q0 + ivec2(0, 1), c2);
    else if (c3 < UNLOADED)
      seed = pack(q0 + ivec2(1, 1), c3);
    else if (u_has_coarse) {
      // A level may lie inside a hole altogether, or see none of the land
      // nearest to its holes: the cells of its border take what the level
      // above (u_coarse holds its central half) has found there.
      ivec2 cells = textureSize(u_cover, 0) / 2;
      if (p.x == 0 || p.y == 0 || p.x == cells.x - 1 || p.y == cells.y - 1)
        seed = pack(q0, texelFetch(u_coarse, p, 0).r);
    }
    out_value = seed;
    return;
  }

  // Resolve (u_step < 0) and JFA flood (u_step > 0) share one row-batched
  // 3x3 nearest-seed search below.
  ivec2 cell;
  int stride;
  vec2 target;
  if (u_step < 0) {
    uint c = texelFetch(u_cover, p, 0).r;
    if (c < UNLOADED) {
      out_value = c;
      return;
    }
    if (c == UNLOADED) {
      // u_coarse holds the central half of the level above.
      out_value = u_has_coarse ? texelFetch(u_coarse, p / 2, 0).r : u_default;
      return;
    }
    cell = p / 2;
    stride = 1;
    target = vec2(p);
  } else {
    // A cell whose own seed lies inside its 2x2 texels is already at the
    // unique global minimum squared distance (0.5) to vec2(2*p) + 0.5; no
    // neighbour at any stride can tie or beat it.
    uint sc = texelFetch(u_seeds, p, 0).r;
    if (sc != NONE &&
        ivec2(int(sc & 0xFFFu) >> 1, int((sc >> 12) & 0xFFFu) >> 1) == p) {
      out_value = sc;
      return;
    }
    cell = p;
    stride = u_step;
    target = vec2(2 * p) + 0.5;
  }

  ivec2 size = textureSize(u_seeds, 0);
  uint best = NONE;
  float best_d = 1e12;
  int x0 = cell.x - stride;
  int x2 = cell.x + stride;
  int cx0 = max(x0, 0);
  int cx2 = min(x2, size.x - 1);
  for (int dy = -1; dy <= 1; dy++) {
    int cy = cell.y + dy * stride;
    if (cy < 0 || cy >= size.y)
      continue;
    uint s0 = texelFetch(u_seeds, ivec2(cx0, cy), 0).r;
    uint s1 = texelFetch(u_seeds, ivec2(cell.x, cy), 0).r;
    uint s2 = texelFetch(u_seeds, ivec2(cx2, cy), 0).r;
    if (x0 >= 0)
      updateBest(s0, target, best, best_d);
    updateBest(s1, target, best, best_d);
    if (x2 < size.x)
      updateBest(s2, target, best, best_d);
  }
  out_value = u_step < 0 ? (best == NONE ? u_default : best >> 24) : best;
}
