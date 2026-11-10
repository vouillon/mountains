// Bakes one layer of the bump-gradient array (see [make_bump_gradients] in
// viewer.ml). Each texel holds the gradients, in height per texture-coordinate
// unit, of two tileable patterns A (rg) and B (ba), so the terrain shader gets
// a bump gradient in one fetch per projection plane and scale, and blends the
// materials as amplitudes of A and B instead of fetching each material's own
// heights. The gradients are analytic -- no finite differences -- and the mip
// chain is the box filter of them, which is the gradient of the box-filtered
// heights.
//
// Layers: 2 * scale + side, scale 0/1/2 = macro/mid/fine (500/71/10 m
// periods), side 0 = the horizontal (XY) projection, 1 = the vertical ones,
// whose v axis is world z: their rock pattern is stretched along it, so on a
// wall the ribs and gullies run down the fall line. Layers 6 and 7 are
// different: broadleaf and conifer crowns, height and gradient (see
// [broadleafCrowns], [coniferCrowns]).
//
// Every pattern is a lattice of integer frequency per tile, so it tiles. A is
// ridged (rock), B smooth (soil, vegetation, ice; none at the macro scale);
// together the three scales cover 31 m down to 8 cm without gaps. (Forest
// crowns come from the canopy photo in the colour, not from here: a crown
// pattern in the bump has no energy between its 7 m size and the next band,
// which shaded as dark domes on everything that used it.)

// The hash and the lattice arithmetic need 32-bit integers: a fragment
// shader's ints default to mediump, 16 bits on Mali, which turned the baked
// patterns into garbage on the phone (desktop GPUs run mediump ints at 32).
precision highp int;

uniform int u_layer;
// Per layer, the factors that bring A's and B's gradient RMS to one (measured
// on the baked chain), and the encoding range in those RMS units.
uniform vec2 u_norm;
uniform float u_enc;

in vec2 uv;
out vec4 color;

uint hash(uvec2 v) {
  // pcg2d
  v = v * 1664525u + 1013904223u;
  v.x += v.y * 1664525u;
  v.y += v.x * 1664525u;
  v ^= v >> 16u;
  v.x += v.y * 1664525u;
  v.y += v.x * 1664525u;
  v ^= v >> 16u;
  return v.x ^ v.y;
}

int floorDiv(int n, int d) { // d > 0
  return n >= 0 ? n / d : -((-n + d - 1) / d);
}

// An octave samples lattice noise at x = M p, M an integer matrix with
// columns [m1] and [m2]: moving p by a whole tile moves x by m1 or m2, so the
// noise tiles exactly when it is periodic over the lattice M Z^2. The hash
// therefore reads a lattice point's canonical representative modulo M Z^2 --
// i - M floor(M^-1 i) -- instead of wrapping each axis. This is what lets each
// octave turn by its own angle (an axis-aligned stack of ridged Perlin
// octaves shows its square lattice) and lets the side layers stretch.
vec2 latticeGrad(ivec2 i, ivec2 m1, ivec2 m2, uint seed) {
  int det = m1.x * m2.y - m2.x * m1.y;
  ivec2 adj = ivec2(m2.y * i.x - m2.x * i.y, -m1.y * i.x + m1.x * i.y);
  if (det < 0) {
    det = -det;
    adj = -adj;
  }
  ivec2 q = ivec2(floorDiv(adj.x, det), floorDiv(adj.y, det));
  ivec2 r = i - (m1 * q.x + m2 * q.y);
  uint h = hash(uvec2(r) + uvec2(seed * 7919u, seed * 104729u));
  float a = float(h) * (6.28318530718 / 4294967296.0);
  return vec2(cos(a), sin(a));
}

// Gradient noise with its analytic derivative (value, d/dp), sampled at
// x = M p (see [latticeGrad]); the derivative is taken back to p.
vec3 noised(vec2 p, ivec2 m1, ivec2 m2, uint seed) {
  vec2 x = vec2(m1) * p.x + vec2(m2) * p.y;
  ivec2 i = ivec2(floor(x));
  vec2 f = fract(x);
  vec2 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);
  vec2 du = 30.0 * f * f * (f * (f - 2.0) + 1.0);
  vec2 ga = latticeGrad(i, m1, m2, seed);
  vec2 gb = latticeGrad(i + ivec2(1, 0), m1, m2, seed);
  vec2 gc = latticeGrad(i + ivec2(0, 1), m1, m2, seed);
  vec2 gd = latticeGrad(i + ivec2(1, 1), m1, m2, seed);
  float va = dot(ga, f);
  float vb = dot(gb, f - vec2(1.0, 0.0));
  float vc = dot(gc, f - vec2(0.0, 1.0));
  float vd = dot(gd, f - vec2(1.0, 1.0));
  float k = va - vb - vc + vd;
  float v = va + u.x * (vb - va) + u.y * (vc - va) + u.x * u.y * k;
  vec2 d = ga + u.x * (gb - ga) + u.y * (gc - ga) +
           u.x * u.y * (ga - gb - gc + gd) +
           du * (u.yx * k + vec2(vb, vc) - va);
  return vec3(v, dot(vec2(m1), d), dot(vec2(m2), d));
}

// fBm from frequency [f0] (cycles per tile) over [octaves] octaves, the
// frequency doubling and the amplitude falling by 0.75, so each finer octave
// carries 1.5x the gradient of the one before. The photo textures this
// replaced rose 1.6-2.2x per octave; at an equal gradient per octave a
// scale's coarsest octaves drew large shaded blobs wherever the footprint
// left only them. Octave o turns by its own angle; [stretch] > 1 divides the
// frequency along v (world z on the side layers) by it, elongating the
// features down the fall line. [sharp] > 0 makes it ridged: (1 - |n|)^sharp,
// sharp crests whose width narrows as [sharp] grows. Returns the gradient per
// tile unit.
vec2 fbm(vec2 p, float f0, int octaves, uint seed, float sharp, float stretch) {
  vec2 g = vec2(0.0);
  float amp = 1.0;
  float f = f0;
  for (int o = 0; o < octaves; o++) {
    float th = 0.6 + 1.3 * float(o) + 0.37 * float(seed % 7u);
    vec2 dir = vec2(cos(th), sin(th));
    // Columns of M: the step in x for a whole tile along u, and along v.
    // Along u the stretched variants stay within 17 degrees of horizontal,
    // so every octave's elongation stays near vertical.
    ivec2 m1, m2;
    if (stretch > 1.0) {
      float th1 = 0.3 * sin(th);
      vec2 d1 = vec2(cos(th1), sin(th1));
      m1 = ivec2(round(f * d1));
      m2 = ivec2(round(f / stretch * vec2(-d1.y, d1.x)));
      if (m2 == ivec2(0))
        m2 = ivec2(0, 1);
    } else {
      m1 = ivec2(round(f * dir));
      m2 = ivec2(-m1.y, m1.x);
    }
    vec3 n = noised(p, m1, m2, seed + uint(o));
    float v = n.x;
    vec2 d = n.yz;
    if (sharp > 0.0) {
      // |n| rounded over a width that grows with the frequency: the top
      // octaves' creases would otherwise be sharper than a texel, and
      // magnified up close bilinear filtering turns them into staircases.
      float e = 0.25 * f / 128.0;
      float an = sqrt(n.x * n.x + e * e);
      float r = 1.0 - an + e;
      v = pow(max(r, 0.0), sharp);
      d = -sharp * pow(max(r, 0.0), sharp - 1.0) * (n.x / an) * d;
    }
    g += amp * d;
    f *= 2.0;
    amp *= 0.75;
  }
  return g;
}

// One population of crowns: [n] x [n] cells per tile, each holding a crown
// with probability [keep], its site anywhere in the cell, its radius in
// [rmin, rmax] cells (at most 1, so a 3 x 3 neighbourhood finds it), a
// profile 1 - (d / r)^[shape] -- 1 a cone, 2 a rounded dome -- and a height
// that grows with the radius, jittered. Folds the crowns into the running
// upper envelope [best] and its gradient [g] (per tile unit), scaled by [hs].
void crownPop(vec2 p, int n, uint seed, float keep, float rmin, float rmax,
              float hs, float shape, inout float best, inout vec2 g) {
  vec2 x = p * float(n);
  ivec2 c = ivec2(floor(x));
  for (int dy = -1; dy <= 1; dy++)
    for (int dx = -1; dx <= 1; dx++) {
      ivec2 cell = c + ivec2(dx, dy);
      ivec2 w = cell - n * ivec2(floorDiv(cell.x, n), floorDiv(cell.y, n));
      uint h = hash(uvec2(w) + uvec2(seed * 7919u, seed * 104729u));
      uint h2 = hash(uvec2(h, seed));
      if (float(h2 >> 16u) / 65535.0 > keep)
        continue;
      vec2 site =
          vec2(cell) + vec2(float(h & 0xffffu), float(h >> 16u)) / 65535.0;
      float u = float(h2 & 0xffu) / 255.0;
      float r = mix(rmin, rmax, u);
      float k =
          hs * (r / rmax) * (0.75 + 0.5 * float((h2 >> 8u) & 0xffu) / 255.0);
      vec2 dv = x - site;
      float d = length(dv);
      if (d < r) {
        float hh = k * (1.0 - pow(d / r, shape));
        if (hh > best) {
          best = hh;
          g = d > 1e-5
                  ? -k * shape * pow(d / r, shape - 1.0) / r * dv / d * float(n)
                  : vec2(0.0);
        }
      }
    }
}

// Forest crowns: the upper envelope of two populations -- big trees, a few
// cells empty, and smaller ones at twice the density, more of them empty,
// filling in between -- so they neither line up in rows nor come in one size.
// Gaps sit at zero. Returns (height in [0, 1], gradient per tile unit).
// Broadleaf (layer 6), at the terrain shader's 131 m tile: rounded domes, big
// ones up to ~9 m across, small ones 3-5 m.
vec3 broadleafCrowns(vec2 p, uint seed) {
  float best = 0.0;
  vec2 g = vec2(0.0);
  crownPop(p, 14, seed, 0.85, 0.5, 1.0, 0.85, 1.8, best, g);
  crownPop(p, 27, seed + 1u, 0.75, 0.6, 1.0, 0.6, 1.8, best, g);
  return vec3(best, g);
}

// Conifers (layer 7): narrow cones, 4-8 m across and 2.5-4.5 m between them,
// more of the ground between crowns showing.
vec3 coniferCrowns(vec2 p, uint seed) {
  float best = 0.0;
  vec2 g = vec2(0.0);
  crownPop(p, 30, seed, 0.85, 0.45, 0.9, 0.9, 1.0, best, g);
  crownPop(p, 52, seed + 1u, 0.6, 0.5, 0.9, 0.6, 1.0, best, g);
  return vec3(best, g);
}

// Encoding range of the crown gradient, per tile unit (CROWN_GMAX in
// terrain.frag).
const float CROWN_GMAX = 128.0;

void main() {
  // Texel centres: [uv] runs 0..1 across the viewport.
  vec2 p = uv;
  if (u_layer >= 6) {
    vec3 c = u_layer == 6 ? broadleafCrowns(p, 900u) : coniferCrowns(p, 950u);
    color = vec4(clamp(c.x, 0.0, 1.0),
                 clamp(c.yz / (2.0 * CROWN_GMAX) + 0.5, 0.0, 1.0), 1.0);
    return;
  }
  int scale = u_layer / 2;
  bool side = (u_layer % 2) == 1;
  // Each scale keeps only the top octaves of its tile, so the three tile the
  // spectrum from ~30 m down to ~8 cm without overlapping much: what a pixel
  // shows is the band just above its footprint, while longer wavelengths only
  // tilt whole patches of pixels (blotches), and those are the relief data's
  // to draw. Octaves stop at 128 cycles per tile (4 texels per cycle at 512).
  // 2:1 -- at 4:1 moderately steep slopes read as brushed metal.
  float stretch = side ? 2.0 : 1.0;
  uint seed = uint(scale) * 101u;
  vec2 a, b;
  if (scale == 0) {
    // Macro, 500 m tile. A: 31 m to 4 m, uniform (calm and broken patches
    // baked in made the tile recognisable, and forest showed it repeating; a
    // sinusoid mask in the terrain shader looked worse and repeated too). No
    // B: nothing uses it at this scale.
    a = fbm(p, 16.0, 4, seed, 2.0, stretch);
    b = vec2(0.0);
  } else if (scale == 1) {
    // Mid, 71 m tile, 9 m to 0.55 m: fractured rock; soft ground.
    a = fbm(p, 8.0, 5, seed, 2.0, stretch);
    b = fbm(p, 8.0, 5, seed + 50u, 0.0, 1.0);
  } else {
    // Fine, 10 m tile, 1.25 m to 8 cm: fractured rock; soft tussocky ground.
    a = fbm(p, 8.0, 5, seed, 2.0, stretch);
    b = fbm(p, 8.0, 5, seed + 50u, 0.0, 1.0);
  }
  vec4 g = vec4(a * u_norm.x, b * u_norm.y);
  color = clamp(g / (2.0 * u_enc) + 0.5, 0.0, 1.0);
}
