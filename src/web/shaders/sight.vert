
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

// Single-pass mip-0 ground height and grid pitch at [coord], short-circuiting
// on the innermost active HD ring without evaluating hdCoord twice or running
// LOD selection (sight lines always sample mip 0).
highp float ground0(highp vec2 coord, int base_w, highp float base,
                    out highp float pitch, out bool in_hd) {
  highp vec3 q = vec3(coord.x * coord.x, coord.x * coord.y, coord.y * coord.y);
  if (hd_bias4[0].w > 0.5) {
    highp vec4 m = hd_mat4[0];
    highp vec2 c =
        mat2(m.xy, m.zw) * coord + hd_bias4[0].xy +
        vec2(dot(hd_quad[0].xyz, q), dot(vec3(hd_quad[0].w, hd_quad[1].xy), q));
    if (all(greaterThanEqual(c, vec2(0.0))) &&
        all(lessThanEqual(c, vec2(1.0)))) {
      pitch = 0.5 * exp2(-hd_params[0].x) * base;
      in_hd = true;
      return sampleReliefHeight(hd_relief[0], c, 0, 1 << int(hd_params[0].y),
                                hd_params[0].z, hd_params[0].w);
    }
  }
  if (hd_bias4[1].w > 0.5) {
    highp vec4 m = hd_mat4[1];
    highp vec2 c =
        mat2(m.xy, m.zw) * coord + hd_bias4[1].xy +
        vec2(dot(vec3(hd_quad[1].zw, hd_quad[2].x), q), dot(hd_quad[2].yzw, q));
    if (all(greaterThanEqual(c, vec2(0.0))) &&
        all(lessThanEqual(c, vec2(1.0)))) {
      pitch = 0.5 * exp2(-hd_params[1].x) * base;
      in_hd = true;
      return sampleReliefHeight(hd_relief[1], c, 0, 1 << int(hd_params[1].y),
                                hd_params[1].z, hd_params[1].w);
    }
  }
  if (hd_bias4[2].w > 0.5) {
    highp vec4 m = hd_mat4[2];
    highp vec2 c =
        mat2(m.xy, m.zw) * coord + hd_bias4[2].xy +
        vec2(dot(hd_quad[3].xyz, q), dot(vec3(hd_quad[3].w, hd_quad[4].xy), q));
    if (all(greaterThanEqual(c, vec2(0.0))) &&
        all(lessThanEqual(c, vec2(1.0)))) {
      pitch = 0.5 * exp2(-hd_params[2].x) * base;
      in_hd = true;
      return sampleReliefHeight(hd_relief[2], c, 0, 1 << int(hd_params[2].y),
                                hd_params[2].z, hd_params[2].w);
    }
  }
  if (hd_bias4[3].w > 0.5) {
    highp vec4 m = hd_mat4[3];
    highp vec2 c =
        mat2(m.xy, m.zw) * coord + hd_bias4[3].xy +
        vec2(dot(vec3(hd_quad[4].zw, hd_quad[5].x), q), dot(hd_quad[5].yzw, q));
    if (all(greaterThanEqual(c, vec2(0.0))) &&
        all(lessThanEqual(c, vec2(1.0)))) {
      pitch = 0.5 * exp2(-hd_params[3].x) * base;
      in_hd = true;
      return sampleReliefHeight(hd_relief[3], c, 0, 1 << int(hd_params[3].y),
                                hd_params[3].z, hd_params[3].w);
    }
  }
  in_hd = false;
  highp vec2 norm_coord = coord * inv_w + 0.5;
  highp float w = baseWeight(norm_coord);
  pitch = (w > 0.0 ? 0.5 : 2.0) * base;
  highp float h = BASE_OFFSET;
  if (w < 1.0 && far_valid) {
    h = sampleReliefHeight(far_relief, (coord + far_map.xy) * far_map.z, 0,
                           max(1, base_w >> 1), BASE_SCALE_N, BASE_OFFSET);
  }
  if (w > 0.0) {
    h = mix(h,
            sampleReliefHeight(relief, norm_coord, 0, base_w, BASE_SCALE_N,
                               BASE_OFFSET),
            w);
  }
  return h;
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
  highp float rise = (target.z - d * d * 6.8306e-8 - center_height) / d;
  highp float stop = d - min(EXEMPT, EXEMPT_SHARE * d);
  int base_w = int(1.0 / inv_w + 0.5);
  highp float near_step = 0.5 * base;
  for (int i = 0; i < HD_SLOTS; i++)
    if (hd_bias4[i].w > 0.5)
      near_step = min(near_step, 0.5 * exp2(-hd_params[i].x) * base);
  highp float t = 0.0;
  f_seen = 0.5;
  for (int i = 0; i < MAX_STEPS; i++) {
    if (t >= stop) {
      f_seen = 1.0;
      return;
    }
    highp vec2 p = dir * t;
    highp vec2 proj = vec2(p.x * (1.0 + p.y * meridian_conv),
                           p.y - 0.5 * p.x * p.x * meridian_conv);
    highp vec2 coord = (center_offset + proj) * inv_delta;
    highp float pitch;
    bool in_hd;
    highp float h = ground0(coord, base_w, base, pitch, in_hd);
    highp float clearance = center_height + rise * t - (h - t * t * 6.8306e-8);
    // A metre of slack over the base grid and beyond, a tenth of one over
    // the rings: what the data is good for.
    if (clearance < (in_hd && pitch < 0.49 * base ? 0.1 : 1.0)) {
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
