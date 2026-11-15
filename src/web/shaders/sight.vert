
// Which POIs the eye sees (Viewer.start_sight): one vertex per POI, captured
// by transform feedback with the rasterizer off. Each marches its own sight
// line over the surface the mesh is drawn from ([sampleTerrainHeight], at
// full resolution), by the rules of [Visibility.test_precise]: in the
// eye's frame, where the ground is lowered by the Earth's curvature and sight
// lines are straight.

// The POI on the eye's plane, metres east and north of it, and the height of
// the ground there: that of the nearest sample, which for a summit is the
// summit's, where the surface between samples falls short of it.
in highp vec3 target;

// 1 seen, 0 hidden, 0.5 if the march ran out of steps.
out highp float f_seen;

// As far as the mesh goes.
const highp float REACH = 140000.0;
// The ground this close to a POI does not hide it, so that a summit sticks
// out of its own massif; no more than this share of the way for a near one.
const highp float EXEMPT = 300.0;
const highp float EXEMPT_SHARE = 0.06;
// How steeply the ground is taken to rise between two samples: a clearance
// [c] above it lets the march skip [c / MAX_SLOPE] metres.
const highp float MAX_SLOPE = 4.0;
// The eye's surroundings, in base samples, are walked at the finest step
// whatever the clearance: the lip of the summit one stands on rises faster
// than [MAX_SLOPE].
const highp float NEAR_SAMPLES = 6.0;
const int MAX_STEPS = 20000;

// The ground [p] metres from the eye, lowered by the curvature, and [pitch],
// half the spacing in metres of the grid it was read from: sampling a
// bilinear surface any finer tells nothing new.
highp float ground(highp vec2 p, out highp float pitch) {
  highp vec2 proj = vec2(p.x * (1.0 + p.y * meridian_conv),
                         p.y - 0.5 * p.x * p.x * meridian_conv);
  highp vec2 coord = (center_offset + proj) * inv_delta;
  highp vec2 norm_coord = coord * inv_w + 0.5;
  pitch = (baseWeight(norm_coord) > 0.0 ? 0.5 : 2.0) / inv_avg_delta;
  for (int i = HD_SLOTS - 1; i >= 0; i--)
    if (insideHd(i, hdCoord(i, coord)))
      pitch = 0.5 * exp2(-hd_lod_bias[i]) / inv_avg_delta;
  return sampleTerrainHeight(coord, norm_coord, -100.0) - dot(p, p) * 6.8306e-8;
}

void main() {
  gl_Position = vec4(0.0, 0.0, 0.0, 1.0);
  highp float d = length(target.xy);
  if (d > REACH) {
    f_seen = 0.0;
    return;
  }
  // A POI is placed no better than to a sample of the base grid: one this
  // close is where the eye stands.
  highp float base = 1.0 / inv_avg_delta;
  if (d < base) {
    f_seen = 1.0;
    return;
  }
  highp vec2 dir = target.xy / d;
  highp float pitch;
  highp float rise = (target.z - d * d * 6.8306e-8 - center_height) / d;
  highp float stop = d - min(EXEMPT, EXEMPT_SHARE * d);
  highp float near_step = 0.5 * base;
  for (int i = 0; i < HD_SLOTS; i++)
    if (hd_valid[i])
      near_step = min(near_step, 0.5 * exp2(-hd_lod_bias[i]) * base);
  highp float t = 0.0;
  f_seen = 0.5;
  for (int i = 0; i < MAX_STEPS; i++) {
    if (t >= stop) {
      f_seen = 1.0;
      return;
    }
    highp float h = ground(dir * t, pitch);
    highp float clearance = center_height + rise * t - h;
    // A metre of slack over the base grid and beyond, a tenth of one over
    // the rings: what the data is good for.
    if (clearance < (pitch < 0.5 * base ? 0.1 : 1.0)) {
      f_seen = 0.0;
      return;
    }
    // Never further than a sample of the base grid, or of the far field
    // past it: a ridge narrower than that can be steeper than [MAX_SLOPE].
    t += t < NEAR_SAMPLES * base
             ? near_step
             : clamp(clearance / MAX_SLOPE, pitch, max(base, 2.0 * pitch));
  }
}
