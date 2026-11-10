#version 300 es
precision highp float;
precision highp int;

// Finds the trees near the eye, once per location (Viewer.find_trees): one
// vertex per 4 m cell of the LIDAR HD canopy on a square grid centred on the
// eye, captured by transform feedback with the rasterizer off. A cell holds a
// tree if its highest canopy sample is a local maximum; [f_pos] is then where
// it stands and how tall it is, [f_shape] its crown radius, species and
// random draws; otherwise [f_pos.w] is 0. tree.vert draws them.

// The canopy bake (Viewer.canopy_texture): R height (0.2 m per step).
uniform mediump sampler2D u_canopy;
// The 1 m ring's height pyramid, and its decoding (as [sampleReliefHeight] in
// radial_common.vert).
uniform highp sampler2D u_ground;
uniform highp float u_groundScale;
uniform highp float u_groundOffset;
// The 1 m ring's frame: sample index -> arcseconds from the anchor.
uniform highp mat2 u_ofMat;
uniform highp vec2 u_ofBias;
uniform highp vec3 u_ofQx;
uniform highp vec3 u_ofQy;
uniform highp vec2 u_delta;        // metres per arcsecond, east and north
uniform highp vec2 u_centerOffset; // the eye, metres from the anchor
uniform highp vec2 u_eyeTexel;     // the eye, in canopy texels
uniform int u_grid;                // cells per side
uniform highp float u_radius;      // metres
// The land cover, as terrain.frag's [sampleCLCBilinear] reads it (nearest
// here): the species, and no trees in built-up areas, whose roofs the
// canopy model carries as crowns.
uniform mediump usampler2DArray u_coverMap;
uniform mediump sampler2D u_paletteTex;
uniform highp float u_invW; // 1 / the base grid's size

out vec4 f_pos;   // metres from the eye east and north, ground, height
out vec4 f_shape; // crown radius, 1 for a conifer, two draws in [0, 1]

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

// terrain.frag's [valueNoise] (same hash), so that the trees' species follow
// the stands the ground under them is shaded as.
float valueNoise(highp vec2 p) {
  highp vec2 i = floor(p);
  highp vec2 f = p - i;
  highp ivec2 c = ivec2(i);
  float a = float(hash(uvec2(c)) & 0xffffu) / 65535.0;
  float b = float(hash(uvec2(c + ivec2(1, 0))) & 0xffffu) / 65535.0;
  float d = float(hash(uvec2(c + ivec2(0, 1))) & 0xffffu) / 65535.0;
  float e = float(hash(uvec2(c + ivec2(1, 1))) & 0xffffu) / 65535.0;
  vec2 u = vec2(f * f * (3.0 - 2.0 * f));
  return mix(mix(a, b, u.x), mix(d, e, u.x), u.y);
}

float canopyAt(ivec2 t) {
  vec4 s = texelFetch(u_canopy, t, 0);
  return s.r > 0.995 ? -1.0 : s.r * (255.0 / 5.0);
}

highp float groundAt(highp vec2 norm) {
  ivec2 size = textureSize(u_ground, 0);
  highp vec2 p = clamp(norm * vec2(size), vec2(0.0), vec2(size - 1));
  ivec2 b = ivec2(p);
  highp vec2 f = fract(p);
  highp vec4 H =
      vec4(dot(texelFetch(u_ground, b, 0).rg, vec2(1.0, 256.0)),
           dot(texelFetch(u_ground, min(b + ivec2(1, 0), size - 1), 0).rg,
               vec2(1.0, 256.0)),
           dot(texelFetch(u_ground, min(b + ivec2(0, 1), size - 1), 0).rg,
               vec2(1.0, 256.0)),
           dot(texelFetch(u_ground, min(b + ivec2(1, 1), size - 1), 0).rg,
               vec2(1.0, 256.0))) *
          u_groundScale +
      u_groundOffset;
  return mix(mix(H.x, H.y, f.x), mix(H.z, H.w, f.x), f.y);
}

// The land-cover palette's conifer share (R) and built-up flag (G) at [arc]
// arcseconds from the anchor; the level choice is [sampleCLCBilinear]'s.
vec2 coverAt(highp vec2 arc) {
  highp vec2 uv = arc * u_invW + 0.5 + 0.5 * u_invW;
  highp float d = max(max(abs(uv.x - 0.5), abs(uv.y - 0.5)), 0.000001);
  int level = clamp(int(ceil(7.0 + log2(d))), 0, 6);
  highp vec2 luv = (uv - 0.5) * exp2(float(6 - level)) + 0.5;
  ivec2 size = textureSize(u_coverMap, 0).xy;
  ivec2 p = clamp(ivec2(floor(luv * vec2(size))), ivec2(0), size - 1);
  uint id = texelFetch(u_coverMap, ivec3(p, level), 0).r;
  return texelFetch(u_paletteTex, ivec2(int(id) * 3 + 2, 0), 0).rg;
}

highp vec2 arcAt(highp vec2 idx) {
  highp vec3 q = vec3(idx.x * idx.x, idx.x * idx.y, idx.y * idx.y);
  return u_ofMat * idx + u_ofBias + vec2(dot(u_ofQx, q), dot(u_ofQy, q));
}

void main() {
  f_pos = vec4(0.0);
  f_shape = vec4(0.0);
  ivec2 cell =
      ivec2(gl_VertexID % u_grid, gl_VertexID / u_grid) - ivec2(u_grid / 2);
  ivec2 t0 = ivec2(floor(u_eyeTexel * 0.5)) * 2 + cell * 2;
  // The near canopy is the left half of the atlas (Viewer.canopy_atlas).
  int csize = textureSize(u_canopy, 0).y;
  if (any(lessThan(t0, ivec2(2))) || any(greaterThan(t0, ivec2(csize - 4))))
    return;
  // Beyond the radius, or within 40 m of the eye: a tree beside a viewer
  // standing in forest fills the screen, and the view is what the app is for.
  highp vec2 c = arcAt(vec2(t0) * 2.0 + 2.0) * u_delta - u_centerOffset;
  if (dot(c, c) > u_radius * u_radius || dot(c, c) < 40.0 * 40.0)
    return;

  // The crown top in this cell: its highest sample, if no neighbour is
  // higher (a local maximum at 2 m -- the bake's 4 m window allowed about
  // one tree per 10 m, and groves came out as two or three giants).
  ivec2 tt = t0;
  float h = canopyAt(t0);
  for (int k = 1; k < 4; k++) {
    ivec2 t = t0 + ivec2(k & 1, k >> 1);
    float v = canopyAt(t);
    if (v > h) {
      h = v;
      tt = t;
    }
  }
  if (h < 3.0 || h > 40.0)
    return;
  for (int k = 0; k < 9; k++) {
    ivec2 o = ivec2(k % 3, k / 3) - 1;
    if (k != 4 && canopyAt(tt + o) > h)
      return;
  }
  // A flat top over 4 m is a roof, not a crown.
  if (canopyAt(tt + ivec2(2, 0)) > h - 1.0 &&
      canopyAt(tt - ivec2(2, 0)) > h - 1.0 &&
      canopyAt(tt + ivec2(0, 2)) > h - 1.0 &&
      canopyAt(tt - ivec2(0, 2)) > h - 1.0)
    return;

  highp uint hs = hash(uvec2(tt));
  float j0 = float(hs & 0xffu) / 255.0;
  float j1 = float((hs >> 8u) & 0xffu) / 255.0;
  float j2 = float((hs >> 16u) & 0xffu) / 255.0;
  float j3 = float((hs >> 24u) & 0xffu) / 255.0;
  // Thinning out over the block's last 3% (~60 m), where the far canopy's
  // raised mesh takes over (radial_common.vert's [canopyEdge]).
  vec2 e = min(vec2(tt) + 0.5, float(csize) - 0.5 - vec2(tt)) / float(csize);
  if (smoothstep(0.0, 0.03, min(e.x, e.y)) < float((hs >> 4u) & 0xffu) / 255.0)
    return;

  // Canopy texel -> 1 m ring index (texel j's centre is index 2 j + 0.5),
  // with up to half a metre of jitter, -> arcseconds -> metres from the eye.
  highp vec2 idx = vec2(tt) * 2.0 + 0.5 + (vec2(j0, j1) - 0.5);
  highp vec2 arc = arcAt(idx);
  vec2 cover = coverAt(arc);
  if (cover.g > 0.5)
    return;
  // Conifer stands within the class's share, as terrain.frag's [coniferAmt]
  // (its world position is metres from the anchor, as [arc * u_delta]).
  float share = cover.r;
  bool conifer = share >= 0.98 ||
                 (share > 0.02 &&
                  share > valueNoise(arc * u_delta / 60.0 + vec2(53.0, 7.0)));

  highp vec2 base = arc * u_delta - u_centerOffset;
  highp float ground = groundAt(idx / float(textureSize(u_ground, 0).x));
  // Crown radius: conifers narrow, broadleaves about as wide as half their
  // height (LIDAR crowns of 6-10 m across).
  float r = conifer ? clamp(0.22 * h, 1.0, 3.5) * (0.85 + 0.3 * j2)
                    : clamp(0.3 * h, 1.5, 5.0) * (0.85 + 0.3 * j2);
  f_pos = vec4(base, ground, h);
  f_shape = vec4(r, conifer ? 1.0 : 0.0, j0, j3);
}
