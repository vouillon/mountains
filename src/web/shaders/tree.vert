#version 300 es
precision highp float;
precision highp int;

// Trees over the canopy block, one instance per tree found by tree_find.vert,
// drawn as triangles from gl_VertexID alone: near the eye a conifer as three
// stacked cones with ragged rims, a broadleaf as a lumpy ellipsoid, both on a
// trunk; further out ([u_far], Viewer.tree_far) one cone of five sides, and
// a lumpy bipyramid, on a flat trunk. One draw per species and distance
// ([u_broadleaf], [u_far]). Also drawn into the sun's shadow map
// ([u_shadowPass]), so that trees cast shadows on the ground and on each other.

uniform mat4 proj;
uniform mat4 transform;
uniform highp float u_eyeHeight;   // metres
uniform highp vec2 u_centerOffset; // the eye, metres from the anchor
uniform bool u_broadleaf;
uniform bool u_far;
uniform highp float u_radius; // metres: the search's (tree_find.vert)
uniform bool u_shadowPass;
uniform highp mat4 u_shadowViewProj;

in vec4 a_pos;   // metres from the eye east and north, ground, height
in vec4 a_shape; // crown radius, 1 for a conifer, two draws in [0, 1]

out vec3 v_normal;
out highp float v_dist;
out highp float v_z; // metres, relative to the eye
out float v_up;      // 0 at the crown's base, 1 at its top
out highp vec3 v_world;
out vec3 v_fromEye; // the eye to this point, metres
flat out float v_tint;
flat out float v_bark;
// For the foliage pattern (tree.frag): the point from the tree's foot,
// metres; the tree's seed; its height and crown radius.
out highp vec3 v_local;
flat out highp float v_seed;
flat out vec2 v_size;

const int SIDES = 7;
const float TAU = 6.2831853;

highp uint hash(highp uvec2 v) {
  v = v * 1664525u + 1013904223u;
  v.x += v.y * 1664525u;
  v.y += v.x * 1664525u;
  v ^= v >> 16u;
  v.x += v.y * 1664525u;
  v.y += v.x * 1664525u;
  v ^= v >> 16u;
  return v.x ^ v.y;
}

float rand(highp uint seed, int a, int b) {
  return float(hash(uvec2(seed, uint(a * 64 + b))) & 0xffffu) / 65535.0;
}

void collapse() {
  gl_Position = vec4(2.0, 2.0, 2.0, 1.0);
  v_normal = vec3(0.0, 0.0, 1.0);
  v_dist = 0.0;
  v_z = 0.0;
  v_up = 0.0;
  v_world = vec3(0.0);
  v_fromEye = vec3(0.0, 0.0, 1.0);
  v_tint = 1.0;
  v_bark = 0.0;
  v_local = vec3(0.0);
  v_seed = 0.0;
  v_size = vec2(1.0);
}

// Conifer: tier [tier] of three overlapping cones, each one narrower and
// higher than the one below, so the outline steps in as on a spruce.
const vec3 TIER_BOTTOM = vec3(0.0, 0.3, 0.55);
const vec3 TIER_TOP = vec3(0.55, 0.8, 1.0);
const vec3 TIER_RADIUS = vec3(1.0, 0.72, 0.45);

void main() {
  highp float h = a_pos.w;
  float r = a_shape.x;
  highp uint seed =
      uint(a_shape.z * 255.0 + 0.5) * 256u + uint(a_shape.w * 255.0 + 0.5);
  float spin = a_shape.z * TAU;

  // Gone by 10-120 m inside the search's radius, a random distance per tree,
  // shrinking over its last 40 m: the stand thins out where the raised
  // canopy mesh (radial_common.vert's [canopyLift]) takes over, instead of
  // ending on a circle.
  highp float horiz = length(a_pos.xy);
  float cut =
      u_radius - 120.0 + 110.0 * fract(a_shape.z * 13.7 + a_shape.w * 5.3);
  float grow = 1.0 - smoothstep(cut - 40.0, cut, horiz);
  if (grow <= 0.0) {
    collapse();
    return;
  }
  h *= grow;
  r *= grow;
  highp float ground = a_pos.z;

  if (!u_shadowPass) {
    // Off screen: the tree's axis against the side, top, bottom and near
    // planes, as a capsule of its crown radius.
    highp vec4 lo = proj * transform * vec4(a_pos.xy, ground - 1.0, 1.0);
    highp vec4 hi = proj * transform * vec4(a_pos.xy, ground + h, 1.0);
    highp vec2 m =
        (r + 1.0) *
        sqrt(vec2(proj[0][0] * proj[0][0], proj[1][1] * proj[1][1]) + 1.0);
    highp vec4 margin = vec4(m.x, m.x, m.y, m.y);
    highp vec4 plo = vec4(lo.w + lo.x, lo.w - lo.x, lo.w + lo.y, lo.w - lo.y);
    highp vec4 phi = vec4(hi.w + hi.x, hi.w - hi.x, hi.w + hi.y, hi.w - hi.y);
    if (any(lessThan(max(plo, phi), -margin)) || max(lo.w, hi.w) < -(r + 1.0)) {
      collapse();
      return;
    }
  }

  int tri = gl_VertexID / 3;
  int corner = gl_VertexID % 3;
  highp vec3 p;
  vec3 n;
  v_up = 0.0;
  v_bark = 0.0;
  // Crown triangles first, then the trunk's.
  int sides = u_far ? 5 : SIDES;
  int crownTris =
      u_broadleaf ? (u_far ? 2 : 4) * sides : (u_far ? 1 : 3) * sides;
  if (tri >= crownTris) {
    // Trunk: a three-sided prism from below the ground into the crown; far
    // out, a quad turned to the eye (the shadow pass's is turned to it too:
    // a thin shadow either way).
    int q = tri - crownTris;
    float tr =
        u_broadleaf ? clamp(0.03 * h, 0.15, 0.4) : clamp(0.025 * h, 0.12, 0.35);
    highp float top = (u_broadleaf ? 0.5 : 0.4) * h;
    bool upper, right;
    vec2 dir;
    if (u_far) {
      // (left low, right low, right high) and (left low, right high, left
      // high).
      bool second = q == 1;
      upper = second ? corner != 0 : corner == 2;
      right = second ? corner == 1 : corner != 0;
      vec2 toEye = -normalize(a_pos.xy);
      dir = vec2(-toEye.y, toEye.x) * (right ? 1.0 : -1.0);
      n = vec3(toEye, 0.0);
    } else {
      int side = q / 2;
      bool second = (q & 1) == 1;
      upper = second ? corner != 0 : corner == 2;
      int s = side + (second ? (corner == 2 ? 0 : 1) : (corner == 1 ? 1 : 0));
      float a = float(s) * (TAU / 3.0) + spin;
      dir = vec2(cos(a), sin(a));
      n = vec3(dir, 0.0);
    }
    p = vec3(a_pos.xy + dir * tr, ground + (upper ? top : -0.5));
    v_tint = 1.0;
    v_bark = 1.0;
  } else if (!u_broadleaf) {
    int tier = tri / sides;
    int side = tri % sides;
    // The crown starts 8-25% of the way up: a bare trunk below, longer in
    // some trees than others.
    highp float zb = (0.08 + 0.17 * rand(seed, 11, 0)) * h;
    highp float len = h - zb;
    // Far out, one cone over the whole crown.
    float tierBottom = u_far ? 0.0 : TIER_BOTTOM[tier];
    float tierTop = u_far ? 1.0 : TIER_TOP[tier];
    float tierRadius = u_far ? 0.85 : TIER_RADIUS[tier];
    highp float bot = zb + tierBottom * len;
    highp float top = zb + tierTop * len;
    if (corner == 2) {
      float a =
          (float(side) + 0.5 + 0.5 * float(tier)) * (TAU / float(sides)) + spin;
      vec2 dir = vec2(cos(a), sin(a));
      p = vec3(a_pos.xy, ground + top);
      n = normalize(vec3(dir * (top - bot), r * tierRadius));
      v_up = tierTop;
    } else {
      int s = (side + corner) % sides;
      float a = (float(s) + 0.5 * float(tier)) * (TAU / float(sides)) + spin;
      vec2 dir = vec2(cos(a), sin(a));
      float rr = r * tierRadius * (0.75 + 0.5 * rand(seed, tier, s));
      // The rim hangs a little, as branches do.
      p = vec3(a_pos.xy + rr * dir, ground + bot - 0.15 * rr);
      n = normalize(vec3(dir * (top - bot), rr));
      v_up = tierBottom;
    }
    v_tint = (0.75 + 0.4 * a_shape.w) * (0.8 + 0.4 * rand(seed, 7, tri));
  } else {
    // Broadleaf crown: an ellipsoid from 35% of the height to the top, as
    // rings (0 the bottom pole, 1 and 2 at -25 and +30 degrees, 3 the top),
    // their radii and the poles jittered per vertex into lumps. Triangles:
    // the bottom fan, the band between rings 1 and 2, the top fan. Far out,
    // ring 1 alone, on the equator, between the two fans.
    int ring, s;
    if (tri < sides) {
      ring = corner == 0 ? 0 : 1;
      s = tri + (corner == 2 ? 1 : 0);
    } else if (!u_far && tri < 3 * sides) {
      int q = tri - sides;
      int side = q / 2;
      bool second = (q & 1) == 1;
      // (1 s, 1 s+1, 2 s) and (1 s+1, 2 s+1, 2 s).
      ring = second ? (corner == 0 ? 1 : 2) : (corner == 2 ? 2 : 1);
      s = side + (second ? (corner == 2 ? 0 : 1) : (corner == 1 ? 1 : 0));
    } else {
      ring = corner == 2 ? 3 : (u_far ? 1 : 2);
      s = tri - (u_far ? 1 : 3) * sides + (corner == 1 ? 1 : 0);
    }
    s = s % sides;
    highp float zb = 0.35 * h;
    highp float c = 0.5 * (h - zb);
    highp float zc = zb + c;
    float phi = ring == 0   ? -1.5708
                : ring == 1 ? (u_far ? 0.0 : -0.44)
                : ring == 2 ? 0.52
                            : 1.5708;
    float lump =
        0.75 + 0.5 * rand(seed, 8 + ring, ring == 0 || ring == 3 ? 0 : s);
    float a =
        (float(s) + (ring == 2 ? 0.5 : 0.0)) * (TAU / float(sides)) + spin;
    vec2 dir = vec2(cos(a), sin(a));
    float cp = cos(phi), sp = sin(phi);
    // Far out, the poles are lowered: a bipyramid read as a diamond.
    float pole = u_far && (ring == 0 || ring == 3) ? 0.65 : 1.0;
    p = vec3(a_pos.xy + dir * (r * cp * lump),
             ground + zc + c * sp * lump * pole);
    n = normalize(vec3(dir * (cp / r), sp / c + 0.15 / c));
    v_up = 0.5 + 0.5 * sp;
    v_tint = (0.75 + 0.4 * a_shape.w) * (0.8 + 0.4 * rand(seed, 7, tri));
  }
  v_normal = n;
  v_local = p - vec3(a_pos.xy, ground);
  v_seed = float(seed);
  v_size = vec2(h, r);

  // Earth curvature, as the mesh (radial_common.vert).
  p.z -= dot(p.xy, p.xy) * 6.8306e-8;
  v_world = vec3(p.xy + u_centerOffset, p.z);
  v_z = p.z - u_eyeHeight;
  v_dist = length(vec3(p.xy, v_z));
  v_fromEye = vec3(p.xy, v_z);
  gl_Position = u_shadowPass ? u_shadowViewProj * vec4(v_world, 1.0)
                             : proj * transform * vec4(p, 1.0);
}
