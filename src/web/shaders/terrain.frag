precision mediump float; // Default mediump for mobile performance
precision highp sampler2DArray;

// Encoded normal only (RG8). The heights live in a separate RG8 texture that
// the vertex stage declares as `relief`; this stage never reads them.
uniform highp sampler2D relief_normal;
// Encoded normals of the near-field refinement rings (see [Hd_dem]), innermost
// first. Selected by the same extent tests as the vertex stage; [hd_valid] is
// shared with it, and bool needs no precision qualifier, so the two
// declarations cannot disagree.
#define HD_SLOTS 4
uniform bvec4 hd_valid;
uniform highp sampler2D hd_relief_normal[HD_SLOTS];
// Encoded normals of the far field, which has the ground beyond the base grid
// (see radial_common.vert, which shares [far_valid]).
uniform bool far_valid;
uniform highp vec3 far_uv_map; // (offset.x, offset.y, scale) from reliefCoord
uniform highp sampler2D far_relief_normal;
uniform mediump sampler2D far_ao; // and its ambient occlusion
// Each ring's sample pitch in metres (the coarser axis). The bump weighs
// itself against it per pixel in [perturbNormal]: footprints far below the
// pitch show scales no grid sample can express, footprints at the pitch and
// above show exactly the band the data owns (see [Render_state]).
uniform mediump vec4 hd_step;
uniform mediump sampler2D ao;
uniform mediump sampler2D
    u_detailMap; // Packed RGBA: R=Rock, G=Grass, B=Forest, A=Ice
// Baked bump gradients (bump_gen.frag): layer 2 * scale + side, the
// gradients of two patterns A (rg) and B (ba) per texel, in RMS units encoded
// over +-BUMP_ENC (must match [bump_enc] in viewer.ml).
uniform mediump sampler2DArray u_bumpGrad;
// LIDAR HD canopy over the 1 m ring's block (Hd_dem.canopy), RGBA8: height
// (0.2 m per step, 255 where the service had no data), its slope east and
// north, and a crown tone (see Viewer.canopy_texture). Read through ring slot
// [u_canopySlot]'s normalised coordinate; -1 when there is no canopy for the
// block on screen. The far canopy (Hd_dem.canopy_far, 5 m over the 5 m
// ring's block) likewise through [u_canopyFarSlot]; both share [u_canopy]
// (Viewer.canopy_atlas), the near one in its first slot. [u_treesStand]:
// whether tree.vert stands trees over the near canopy.
// Declared in radial_common.vert too, with the same precisions.
uniform mediump sampler2D u_canopy;
uniform mediump int u_canopySlot;
uniform mediump int u_canopyFarSlot;
uniform bool u_treesStand;
uniform mediump vec2 u_canopyStep; // as in radial_common.vert
// The 20 m canopy (Hd_dem.canopy_mid, ~20 km), if [u_canopyMidOn]: slot 4 of
// the atlas, through a map of its own from the base grid's coordinate.
uniform bool u_canopyMidOn;
uniform highp mat2 u_canopyMidMat;
uniform highp vec2 u_canopyMidBias;
uniform highp vec3 u_canopyMidQx;
uniform highp vec3 u_canopyMidQy;
// The EEA tree cover (Tree_cover, ~20 m over ~20 km), the atlas's third
// region (see tree_cover_prep.frag): R density, B data, G conifer share
// premultiplied by A, where the leaf type is known. Read
// at [reliefCoord * u_treeCoverMap.xy + u_treeCoverMap.zw], if
// [u_treeCoverOn].
uniform bool u_treeCoverOn;
uniform highp vec4 u_treeCoverMap;
// Beyond it, its coarser level (~80 m over ~80 km), the fourth region,
// likewise.
uniform bool u_treeCoverFarOn;
uniform highp vec4 u_treeCoverFarMap;
// Metres above which the trees turn to conifers, over 500 m, whatever the
// land cover's class says: in the Alps scattered trees up high are larch,
// arolla and mountain pine, spruce (Viewer.apply_tree_cover).
uniform highp float u_coniferLine;
const float BUMP_ENC = 4.0;
// Forest crowns, layers 6-7 of u_bumpGrad (bump_gen.frag's crown fields): tile
// size (big crowns up to ~9 m across, small ones 3-5 m), gradient encoding
// range per tile unit
// (CROWN_GMAX there), and the crowns' height against their width, which
// sets how steep their sides are.
const float CROWN_TILE = 131.0;
const float CROWN_GMAX = 128.0;
const float CROWN_RELIEF = 3.0;
// The mid and fine tiles (70.7 m, 10.24 m) are deliberately incommensurate
// with the macro one (500 m): at 1/7 and 1/50 of it every layer realigned
// every 500 m and the whole bump repeated on that grid. (The top plane also
// turns by 30 and 63 degrees at these scales, see [bumpGrad].)
const float BUMP_SCALE_MID = 0.0141421;
const float BUMP_SCALE_FINE = 0.0976563;
uniform highp sampler2DArrayShadow shadow_map; // Hardware shadow comparison

// CLC Material System Uniforms
uniform mediump usampler2DArray u_coverMap; // CLC ID clipmap (layers)
uniform mediump sampler2D u_paletteTex;     // 256x1 RGBA palette
uniform int u_numLevels;                    // Number of clipmap levels
// Where the cover map is centred, from the anchor, in base-grid normalised
// units: it is baked around the eye, and the anchor is held across moves
// that it is not (Viewer.cover_shift).
uniform highp vec2 u_coverShift;
// Orthographic shadow matrices with clip -> [0, 1] UV/depth (* 0.5 + 0.5)
// baked in on the CPU: mat4x3 saves 16 uniform registers across the 4 layers
// and skips the / w division and * 0.5 + 0.5 per fragment.
uniform highp mat4x3 shadow_matrices[3]; // Must be highp for projection
uniform mediump float shadow_splits[3];
// Per cascade: world metres per shadow texel, and the depth range its [0,1]
// spans, both from the ortho box [calculate_shadow_matrices] builds.
uniform highp vec2 shadow_cascades[3];
// Shadow layer 3: the trees near the eye alone (Viewer.tree_shadow_box), if
// it holds any.
uniform highp mat4x3 u_treeShadowMatrix;
uniform bool u_treeShadowOn;
// Pre-normalized on CPU; highp to match sky.frag and highp half-vector math.
uniform highp vec3 u_lightDir;
// What the air leaves of the sun's colour at its height (Viewer.sun_tint):
// white with the sun high, warmer and dimmer as it drops.
uniform vec3 u_sunTint;
uniform highp float center_height;

in highp vec2 reliefCoord; // Highp for texture coords
in highp vec2 hdReliefCoord0;
in highp vec2 hdReliefCoord1;
in highp vec2 hdReliefCoord2;
in highp vec2 hdReliefCoord3;
in highp vec3 v_world_pos; // Highp for world coords

out mediump vec4 color;

// ========== CLC Material System ==========

struct Surface {
  vec3 albedo;
  float roughness;
  vec4 detailWeights; // RGBA = Rock, Grass, Forest, Ice
  float waterFactor;
  float conifer; // share of conifers in the tree cover
  float built;   // artificial surfaces (buildings read as canopy there)
};

// Manual bilinear filtering of CLC IDs: merge duplicate IDs into weights
// w00..w11 and accumulate raw palette texels (pixelC.b carries the pre-baked
// ice flag from Clc_palette), fetching each unique ID once without holding
// four full Surface structs live.
Surface sampleCLCBilinear(highp vec2 uv) {
  // Distance-based LOD (Coverage Constraint)
  // The concentric clipmaps have limited extent. Level L covers radius 0.5 /
  // 2^(6-L). We must choose a level large enough to cover the current UV.
  highp vec2 dist_from_center = abs(uv - 0.5);
  highp float d_center = max(dist_from_center.x, dist_from_center.y);
  // Avoid log2(0)
  d_center = max(d_center, 0.000001);

  // Formula: Level > u_numLevels + log2(d)
  float level_pos = ceil(float(u_numLevels) + log2(d_center));

  int level = clamp(int(level_pos), 0, u_numLevels - 1);

  // Calculate texture coordinates for this concentric level
  // The last level covers the whole map (scale 1x), each one below it half
  // the side of the one above
  float scale = exp2(float((u_numLevels - 1) - level));
  // Texture coordinate: must be highp all the way to [texelPos]. At fp16 the
  // ulp of a [0, 1] coordinate is 2.4-4.9e-4, which is a quarter to a half of
  // a texel of the 1024-wide cover map: the bilinear weight below would take
  // two to four values per texel instead of a ramp, and every land-cover
  // transition would come out as a staircase (~120 m wide at the outer
  // clipmap levels).
  highp vec2 levelUV = (uv - 0.5) * scale + 0.5;

  highp vec2 texSize = vec2(textureSize(u_coverMap, 0).xy);
  highp vec2 texelPos = levelUV * texSize - 0.5;

  ivec2 p00_raw = ivec2(floor(texelPos));
  vec2 f = vec2(fract(texelPos));

  // Clamp to valid range (2 clamps instead of 4)
  ivec2 maxCoord = ivec2(texSize) - 1;
  ivec2 p0 = clamp(p00_raw, ivec2(0), maxCoord);
  ivec2 p1 = clamp(p00_raw + 1, ivec2(0), maxCoord);

  // Sample 4 neighbors from selected array layer
  uint id00 = texelFetch(u_coverMap, ivec3(p0, level), 0).r;
  uint id10 = texelFetch(u_coverMap, ivec3(p1.x, p0.y, level), 0).r;
  uint id01 = texelFetch(u_coverMap, ivec3(p0.x, p1.y, level), 0).r;
  uint id11 = texelFetch(u_coverMap, ivec3(p1, level), 0).r;

  float w00 = (1.0 - f.x) * (1.0 - f.y);
  float w10 = f.x * (1.0 - f.y);
  float w01 = (1.0 - f.x) * f.y;
  float w11 = f.x * f.y;
  if (id10 == id00) {
    w00 += w10;
    w10 = 0.0;
  }
  if (id01 == id00) {
    w00 += w01;
    w01 = 0.0;
  } else if (id01 == id10) {
    w10 += w01;
    w01 = 0.0;
  }
  if (id11 == id00) {
    w00 += w11;
    w11 = 0.0;
  } else if (id11 == id10) {
    w10 += w11;
    w11 = 0.0;
  } else if (id11 == id01) {
    w01 += w11;
    w11 = 0.0;
  }

  int c00 = int(id00) * 3;
  vec4 pA = texelFetch(u_paletteTex, ivec2(c00, 0), 0) * w00;
  vec4 pB = texelFetch(u_paletteTex, ivec2(c00 + 1, 0), 0) * w00;
  vec3 pC = texelFetch(u_paletteTex, ivec2(c00 + 2, 0), 0).rgb * w00;
  if (w10 > 0.0) {
    int c = int(id10) * 3;
    pA += texelFetch(u_paletteTex, ivec2(c, 0), 0) * w10;
    pB += texelFetch(u_paletteTex, ivec2(c + 1, 0), 0) * w10;
    pC += texelFetch(u_paletteTex, ivec2(c + 2, 0), 0).rgb * w10;
  }
  if (w01 > 0.0) {
    int c = int(id01) * 3;
    pA += texelFetch(u_paletteTex, ivec2(c, 0), 0) * w01;
    pB += texelFetch(u_paletteTex, ivec2(c + 1, 0), 0) * w01;
    pC += texelFetch(u_paletteTex, ivec2(c + 2, 0), 0).rgb * w01;
  }
  if (w11 > 0.0) {
    int c = int(id11) * 3;
    pA += texelFetch(u_paletteTex, ivec2(c, 0), 0) * w11;
    pB += texelFetch(u_paletteTex, ivec2(c + 1, 0), 0) * w11;
    pC += texelFetch(u_paletteTex, ivec2(c + 2, 0), 0).rgb * w11;
  }

  Surface result;
  result.albedo = pA.rgb;
  result.roughness = pA.a;
  result.detailWeights = vec4(pB.rgb, pC.b);
  result.waterFactor = pB.a;
  result.conifer = pC.r;
  result.built = pC.g;
  return result;
}

// Modify detail weights based on slope (steep = more rock)
void applySlopeModification(inout Surface s, float slope) {
  // Steep slopes increase rock weight, decrease others
  float rockForce = smoothstep(0.4, 0.8, slope);

  if (rockForce > 0.01) {
    // Blend albedo and roughness towards Bare Rock (ID 31)
    // Optimization: Hardcoded values to avoid texture fetch & unpacking setup
    // Albedo (125, 120, 115) -> Linear ~ (0.21, 0.19, 0.17)
    const vec3 rockAlbedo = vec3(0.21, 0.19, 0.17);
    const float rockRoughness = 0.65;

    s.albedo = mix(s.albedo, rockAlbedo, rockForce);
    s.roughness = mix(s.roughness, rockRoughness, rockForce);

    // Scale down non-rock weights
    s.detailWeights.g *= (1.0 - rockForce); // Reduce grass
    s.detailWeights.b *= (1.0 - rockForce); // Reduce forest
    s.detailWeights.a *= (1.0 - rockForce); // Reduce ice

    // Increase rock weight
    s.detailWeights.r += rockForce;

    // Optimization: Removed intermediate normalization.
    // Weights are renormalized in main() after height blending.
  }
}

// The side projections only pay for themselves where the XY projection is
// genuinely distorted: the stretch is 1/normal.z, a benign 1.4x at 45
// degrees. Below ~50 degrees they are indistinguishable noise-for-noise,
// but a blend-weight threshold alone would engage them on a 10-degree
// hillside (abs(normal).y is already 0.15 there) -- which made "triplanar"
// run 2-3 planes over most of an alpine frame. Smooth in slope, so the
// fetch gates stay quad-coherent.
float triplanarSideGate(vec3 normal) {
  return smoothstep(0.35, 0.55, 1.0 - abs(normal.z));
}

// Triplanar sample of the packed RGBA detail map, combined directly to avoid
// keeping 3 raw vec4 plane samples live. Returns the combined sample and
// outputs the per-plane weights [triW = (w_x, w_y, blend.z)] for [bumpGrad].
// Top/Bottom (blend.z) -> XY, Side-Y (blend.y) -> XZ, Side-X (blend.x) -> YZ.
vec4 sampleTriplanar(highp vec3 worldPos, vec3 normal, float scale,
                     float sideGate, out vec3 triW) {
  vec3 blend = abs(normal);
  // Biplanar past the gate: the weakest plane's weight goes to exactly zero
  // and its fetches are skipped. It is the plane the surface is most oblique
  // to, so it carried the least and the most stretched texture -- on a wall
  // facing one axis the other side plane, on a wall facing a diagonal the
  // top-down one -- and a wall pixel drops from three planes per scale to
  // two (20 detail fetches to 13 facing Mont Aiguille). Continuous: a plane
  // leaves as its weight reaches the minimum, so no seam where the choice
  // changes. Two noises average less than three, so walls read a little
  // crisper. Below the gate the blend is untouched.
  float weakest = min(min(blend.x, blend.y), blend.z);
  blend = mix(blend, max(blend - weakest, 0.0), sideGate);
  blend /= (blend.x + blend.y + blend.z + 0.0001);

  // Sample each projection plane. The side projections are faded by their
  // blend weight and by [sideGate] (see [triplanarSideGate]), and default to
  // the neutral 0.5. What the material blend loses under the gate is a
  // statistically identical share of the same noise, sampled less obliquely.
  float bias = 1.;
  float w_y = blend.y * smoothstep(0.003, 0.01, blend.y) * sideGate;
  float w_x = blend.x * smoothstep(0.003, 0.01, blend.x) * sideGate;
  triW = vec3(w_x, w_y, blend.z);

  vec4 res = vec4(0.5 * (blend.y - w_y + blend.x - w_x));
  if (blend.z > 0.0)
    res += texture(u_detailMap, worldPos.xy * scale, bias) * blend.z;
  if (w_y > 0.0)
    res += texture(u_detailMap, worldPos.xz * scale, bias) * w_y;
  if (w_x > 0.0)
    res += texture(u_detailMap, worldPos.yz * scale, bias) * w_x;
  return res;
}

// A ring's weight at this fragment: 1 well inside its extent, fading to 0
// over the outer band, 0 outside (or when the slot is empty). Both the
// normal source and the bump amplitude switch rings through this fade: the
// data's own edge blend makes the *heights* agree at a ring border, but not
// the derivatives (PLAN.md 2026-08-05), so a hard switch leaves a seam line,
// and the per-ring bump amplitude would step with it.
mediump float ringFade(bool valid, highp vec2 c) {
  if (!valid)
    return 0.0;
  highp float d = min(min(c.x, 1.0 - c.x), min(c.y, 1.0 - c.y));
  return smoothstep(0.0, 0.02, d);
}

// The base grid's weight at this fragment, as radial_common.vert's
// [baseWeight] has it for the heights: 1 well inside it, fading to 0 at its
// edge, beyond which the far field has the ground.
uniform highp vec4 base_valid;
mediump float baseFade() {
  highp vec2 c = reliefCoord;
  highp vec2 d2 = min(c - base_valid.xy, base_valid.zw - c);
  return smoothstep(0.0, 0.03, min(d2.x, d2.y));
}

highp vec2 farReliefCoord() {
  return reliefCoord * far_uv_map.z + far_uv_map.xy;
}

// The encoded normal beneath every ring: the base grid's, the far field's
// beyond it, and both across the band where one fades into the other ([fb],
// from [baseFade]). Single texture() site per sampler.
mediump vec2 baseNormal(mediump float fb) {
  mediump vec2 n = vec2(0.0);
  if (fb > 0.0 || !far_valid)
    n = texture(relief_normal, reliefCoord).rg;
  if (fb < 1.0 && far_valid) {
    mediump vec2 nf = texture(far_relief_normal, farReliefCoord()).rg;
    n = fb > 0.0 ? mix(nf, n, fb) : nf;
  }
  return n;
}

// Per scale (macro, mid, fine), each material's amplitude of pattern A and of
// pattern B, materials in (rock, grass, forest, ice) order, with each scale's
// (2 * BUMP_ENC * scale / divisor) factor pre-folded in:
//   macro: 2 * 4 * 0.002              = 0.016        (B is all zeros)
//   mid:   2 * 4 * BUMP_SCALE_MID / 8 = 0.0141421
//   fine:  2 * 4 * BUMP_SCALE_FINE/60 = 0.01302084
const vec4 BUMP_AMP_A[3] = vec4[3](
    vec4(174.2, 0.0, -103.9, -114.8) * (2.0 * BUMP_ENC * 0.002),
    vec4(290.5, 0.0, -259.7, -116.1) * (2.0 * BUMP_ENC * BUMP_SCALE_MID / 8.0),
    vec4(290.5, 0.0, -259.7, 0.0) * (2.0 * BUMP_ENC * BUMP_SCALE_FINE / 60.0));
const vec4 BUMP_AMP_B_MID =
    vec4(0.0, 44.2, 0.0, 0.0) * (2.0 * BUMP_ENC * BUMP_SCALE_MID / 8.0);
const vec4 BUMP_AMP_B_FINE =
    vec4(0.0, 44.2, 0.0, 117.8) * (2.0 * BUMP_ENC * BUMP_SCALE_FINE / 60.0);

// Vectorized 4-corner value noise in [0, 1] over unit cells: single-round
// 32-bit integer mix (highp: mediump ints are 16-bit on Mali), 10-bit extract
// (exact in FP16, 1/1024 well above FP16 min normal) scaled once after mix.
float valueNoise(highp vec2 p) {
  highp vec2 i = floor(p);
  vec2 f = vec2(p - i);
  highp ivec2 c = ivec2(i);
  highp uint cx0 = uint(c.x) * 374761393u;
  highp uint cx1 = uint(c.x + 1) * 374761393u;
  highp uint cy0 = uint(c.y) * 668265263u;
  highp uint cy1 = uint(c.y + 1) * 668265263u;
  highp uvec4 h = uvec4(cx0 + cy0, cx1 + cy0, cx0 + cy1, cx1 + cy1);
  h = (h ^ (h >> 13u)) * 1274126177u;
  h ^= h >> 16u;
  vec4 v = vec4(h >> 22u);
  vec2 u = f * f * (3.0 - 2.0 * f);
  return mix(mix(v.x, v.y, u.x), mix(v.z, v.w, u.x), u.y) * (1.0 / 1024.0);
}

// 1D variant (two hashes instead of four) for the canopy-flank trunk bands.
float valueNoise1D(highp float p, highp uint seed) {
  highp float i = floor(p);
  float f = float(p - i);
  highp int ci = int(i);
  highp uint c = uint(ci);
  highp uvec2 h = (uvec2(c, c + 1u) * 374761393u + seed) * 1274126177u;
  h ^= h >> 16u;
  vec2 v = vec2(h >> 22u);
  return mix(v.x, v.y, f * f * (3.0 - 2.0 * f)) * (1.0 / 1024.0);
}

// One species' crown layer of u_bumpGrad ([layer], 6 broadleaf, 7 conifer):
// two copies of the tile at pre-rotated coordinates [uv1] (23 deg) and [uv2]
// (106 deg + offset), blended by [pick]. Returns vec3(height, world_grad.xy)
// with gradients rotated directly to world space.
vec3 crownSample(float layer, float pick, highp vec2 uv1, highp vec2 uv2) {
  const mat2 R1_T = mat2(0.921, -0.389, 0.389, 0.921);
  const mat2 R2_T = mat2(-0.276, -0.961, 0.961, -0.276);
  vec3 res = vec3(0.0);
  if (pick < 1.0) {
    vec3 s1 = texture(u_bumpGrad, vec3(uv1, layer), 0.0).rgb;
    res = vec3(s1.r, R1_T * (s1.gb - 0.5));
  }
  if (pick > 0.0) {
    vec3 s2 = texture(u_bumpGrad, vec3(uv2, layer), 0.0).rgb;
    vec3 r2 = vec3(s2.r, R2_T * (s2.gb - 0.5));
    res = pick < 1.0 ? mix(res, r2, pick) : r2;
  }
  return res;
}

// What the canopy says at this fragment: (tree share, confidence, height in
// m). The share is 0 below 1.5 m of canopy and 1 above 4 m. The confidence is
// 0 without a canopy for the block on screen, outside it or where the
// service had no data, fades out over the block's last ~60 m so the hand-over
// to land cover is not a seam, and drops above 40 m: heights like that are
// mostly cliffs, where surface and ground models disagree (La Reunion reads
// 200 m), rather than trees. The fetch happens in uniform control flow.
// The canopy sample [canopyTrees] read (see [Viewer.canopy_texture] for the
// channels), and how far the real canopy replaces the procedural crowns here:
// its confidence times its tree share, set by the canopy block in main.
vec4 g_canopy = vec4(0.0);
float g_canopyReal = 0.0;
// Weighted slope (slope * rest) of the canopy the mesh stands up
// (radial_common.vert's [canopyLift]).
vec2 g_liftGrad = vec2(0.0);

// Radial grid spacing, as in radial_common.vert (same uniforms).
uniform highp float grid_k;
uniform highp float grid_scale;

// As in radial_common.vert.
// Slot [slot] of the atlas (Viewer.canopy_atlas: three columns, two rows):
// 0 the near canopy, 1 the far one, 2 and 3 the EEA tree cover's two levels,
// 4 the 20 m canopy.
highp vec2 atlasUV(highp vec2 c, int slot) {
  return (vec2(float(slot % 3), float(slot / 3)) + clamp(c, 0.0, 1.0)) /
         vec2(3.0, 2.0);
}
highp vec2 canopyNearUV(highp vec2 c) { return atlasUV(c, 0); }
highp vec2 canopyFarUV(highp vec2 c) { return atlasUV(c, 1); }
// The tree cover's level [level] (0 the finer).
highp vec2 canopyCoverUV(highp vec2 c, int level) {
  return atlasUV(c, 2 + level);
}
// The 20 m canopy's normalised coordinate, from the base grid's
// (u_canopyMid*: Viewer.apply_canopy).
highp vec2 canopyMidCoord() {
  highp vec2 c = reliefCoord;
  highp vec3 q = vec3(c.x * c.x, c.x * c.y, c.y * c.y);
  return u_canopyMidMat * c + u_canopyMidBias +
         vec2(dot(u_canopyMidQx, q), dot(u_canopyMidQy, q));
}
float canopyEdge(highp vec2 c) {
  highp vec2 e = min(c, 1.0 - c);
  return smoothstep(0.0, 0.03, min(e.x, e.y));
}
uniform highp vec2 center_offset;
float treesStood(highp float r, float nearEdge) {
  return u_treesStand ? (1.0 - smoothstep(840.0, 960.0, r)) * nearEdge : 0.0;
}
float canopyRest(highp float r, float nearEdge) {
  float clearing = u_canopySlot >= 0 ? smoothstep(40.0, 80.0, r)
                                     : smoothstep(300.0, 600.0, r);
  return (1.0 - treesStood(r, nearEdge)) * clearing;
}
highp vec2 slotCoord(int slot) {
  return slot == 0   ? hdReliefCoord0
         : slot == 1 ? hdReliefCoord1
         : slot == 2 ? hdReliefCoord2
                     : hdReliefCoord3;
}

// Read the canopies at the level the mesh did, and the share of each the
// mesh was raised by -- the same factors as [canopyLift] (its [canopyCover]
// included, and the near canopy counting for nothing where it has no data).
void canopyGeometry(highp float r, highp vec2 cn, highp vec2 cf, float nearEdge,
                    float farEdge) {
  if (nearEdge + farEdge <= 0.0)
    return;
  if ((u_canopySlot >= 0 && r < 40.0) || (u_canopySlot < 0 && r < 300.0))
    return;
  if (u_treesStand && r < 840.0 && nearEdge >= 1.0 && g_canopy.a >= 0.5 / 255.0)
    return;
  highp float spacing = grid_k * (r + grid_scale);
  highp float log_sp = log2(max(spacing, 1e-4));
  vec4 sn = vec4(0.0);
  if (nearEdge > 0.0) {
    sn = textureLod(u_canopy, canopyNearUV(cn),
                    max(log_sp - log2(u_canopyStep.x), 0.0) + 2.0);
    if (sn.a < 0.5 / 255.0)
      nearEdge = 0.0;
  }
  float rest = canopyRest(r, nearEdge);
  if (rest <= 0.0)
    return;
  vec2 slope = vec2(0.0);
  for (int k = 0; k < 2; k++) {
    float w = k == 0 ? nearEdge : (1.0 - nearEdge) * farEdge;
    if (w <= 0.0)
      continue;
    vec4 s = k == 0 ? sn
                    : textureLod(u_canopy, canopyFarUV(cf),
                                 max(log_sp - log2(u_canopyStep.y), 0.0) + 2.0);
    float h = s.r * (255.0 / 5.0);
    float c = w * smoothstep(1.0, 3.0, h) * (1.0 - smoothstep(40.0, 48.0, h)) *
              smoothstep(0.2, 0.4, s.a);
    slope += c * (s.gb * (255.0 / 32.0) - 4.0);
  }
  g_liftGrad = slope * rest;
}

// The share of this fragment's canopy reading taken from the near canopy:
// only it resolves crowns. And how far tree_find.vert has trees to stand
// here: where the near canopy has data and is the 3 m it takes for one --
// lower, it is scrub, shaded as canopy rather than as the ground under trees
// that are not there.
float g_canopyNear = 0.0;
float g_canopyStands = 0.0;

vec3 canopyTrees(highp vec2 cn, highp vec2 cf, float nearEdge, float farEdge) {
  // Derivatives inside uniform gates stay defined in GLES 3.00 while skipping
  // work whenever the corresponding canopy layer is inactive.
  highp vec2 uv_n = vec2(0.0), dx_n = vec2(0.0), dy_n = vec2(0.0);
  if (u_canopySlot >= 0) {
    uv_n = canopyNearUV(cn);
    dx_n = dFdx(uv_n);
    dy_n = dFdy(uv_n);
  }
  float midEdge = 0.0;
  highp vec2 uv_m = vec2(0.0), dx_m = vec2(0.0), dy_m = vec2(0.0);
  if (u_canopyMidOn) {
    highp vec2 cm = canopyMidCoord();
    midEdge = canopyEdge(cm);
    uv_m = atlasUV(cm, 4);
    dx_m = dFdx(uv_m);
    dy_m = dFdy(uv_m);
  }
  if (nearEdge + farEdge + midEdge <= 0.0)
    return vec3(0.0);
  if (nearEdge > 0.0)
    g_canopy = textureGrad(u_canopy, uv_n, dx_n, dy_n);
  float wn = g_canopy.a < 0.5 / 255.0 ? 0.0 : nearEdge;
  vec4 f = farEdge > 0.0 && wn < 1.0
               ? textureLod(u_canopy, canopyFarUV(cf), 0.0)
               : vec4(0.0);
  float wf = f.a < 0.5 / 255.0 ? 0.0 : (1.0 - wn) * farEdge;
  // Its A is the share of the read that has data, its R their heights'
  // sum (canopy_prep.frag's [u_flagged]).
  vec4 m = (midEdge > 0.0 && wn + wf < 1.0)
               ? textureGrad(u_canopy, uv_m, dx_m, dy_m)
               : vec4(0.0);
  float wm = (1.0 - wn - wf) * midEdge * m.a;
  float wsum = wn + wf + wm;
  if (wsum <= 0.0)
    return vec3(0.0);
  float hn = g_canopy.r * (255.0 / 5.0);
  float hf = f.r * (255.0 / 5.0);
  float hm = m.a > 0.0 ? m.r / m.a * (255.0 / 5.0) : 0.0;
  // The 20 m canopy is a mean over each sample: a sparse forest reads as a
  // metre or two.
  float share = (wn * smoothstep(1.5, 4.0, hn) + wf * smoothstep(1.5, 4.0, hf) +
                 wm * smoothstep(0.5, 3.0, hm)) /
                wsum;
  float h = (wn * hn + wf * hf + wm * hm) / wsum;
  g_canopyNear = wn / wsum;
  g_canopyStands = wn * smoothstep(2.5, 3.5, hn);
  return vec3(share, wsum * (1.0 - smoothstep(40.0, 48.0, h)), h);
}

// 1 while a band whose longest wavelength is [lmax] still has content at a
// pixel footprint [fp] (both in m), 0 once it is filtered away.
float lodFade(float lmax, highp float fp) {
  return 1.0 - smoothstep(lmax / 8.0, lmax / 3.0, fp);
}

// World-space gradient of macro scale (s == 0): BUMP_AMP_B is zero and top
// rotation is identity; [ampA] already includes 2 * BUMP_ENC * scale *
// strength.
vec3 bumpGradMacro(vec3 triW, highp vec3 pos, float ampA, float mix2) {
  float w_x = triW.x;
  float w_y = triW.y;
  float bias = -0.5 * ampA;
  vec3 g = vec3(0.0);
  if (triW.z > 0.0) {
    vec2 gt =
        texture(u_bumpGrad, vec3(pos.xy * 0.002, 0.0), 1.0).xy * ampA + bias;
    // [mix2] > 0: blend in a second copy of the layer, turned 63 degrees and
    // offset half a tile, which breaks the tile's repetition on flat forest.
    if (mix2 > 0.0) {
      const vec2 rot2 = vec2(0.454, 0.891);
      highp vec2 r2 = mat2(rot2.x, rot2.y, -rot2.y, rot2.x) * pos.xy;
      vec2 d2 =
          texture(u_bumpGrad, vec3(r2 * 0.002 + 0.5, 0.0), 1.0).xy * ampA +
          bias;
      vec2 gt2 = mat2(rot2.x, -rot2.y, rot2.y, rot2.x) * d2;
      gt = (gt * (1.0 - mix2) + gt2 * mix2) *
           inversesqrt((1.0 - mix2) * (1.0 - mix2) + mix2 * mix2);
    }
    g.xy = (1.0 - w_x - w_y) * gt;
  }
  if (w_y > 0.0)
    g.xz +=
        w_y *
        (texture(u_bumpGrad, vec3(pos.xz * 0.002, 1.0), 1.0).xy * ampA + bias);
  if (w_x > 0.0)
    g.yz +=
        w_x *
        (texture(u_bumpGrad, vec3(pos.yz * 0.002, 1.0), 1.0).xy * ampA + bias);
  return g;
}

// World-space gradient of mid/fine bump scales: [amp] already includes
// 2 * BUMP_ENC * (scale / divisor) * bumpStrength.
vec3 bumpGrad(vec3 triW, highp vec3 pos, float scale, int s, vec2 amp,
              vec2 rot) {
  float w_x = triW.x;
  float w_y = triW.y;
  float top = float(2 * s);
  float side = top + 1.0;
  float bias = -0.5 * (amp.x + amp.y);
  vec3 g = vec3(0.0);
  highp vec2 rxy = mat2(rot.x, rot.y, -rot.y, rot.x) * pos.xy;
  if (triW.z > 0.0) {
    vec4 d = texture(u_bumpGrad, vec3(rxy * scale, top), 1.0);
    vec2 gt = mat2(rot.x, -rot.y, rot.y, rot.x) *
              (amp.x * d.xy + amp.y * d.zw + bias);
    g.xy = (1.0 - w_x - w_y) * gt;
  }
  if (w_y > 0.0) {
    vec4 d = texture(u_bumpGrad, vec3(pos.xz * scale, side), 1.0);
    g.xz += w_y * (amp.x * d.xy + amp.y * d.zw + bias);
  }
  if (w_x > 0.0) {
    vec4 d = texture(u_bumpGrad, vec3(pos.yz * scale, side), 1.0);
    g.yz += w_x * (amp.x * d.xy + amp.y * d.zw + bias);
  }
  return g;
}

// Procedural normal perturbation: the baked bump gradients (see [bumpGrad])
// at three scales. [triW] is the per-plane weight triple (w_x, w_y, blend.z)
// from [sampleTriplanar]. [fpL] is the anisotropic pixel footprint computed
// once in main.
vec3 perturbNormal(vec3 geomNormal, vec3 triW, float roughness,
                   vec4 detailWeights, float dataStep, highp float fpL,
                   highp float v_dist) {
  // Near-field masks: the macro scale only beyond ~200 m, the mid scale
  // beyond ~30 m. Each band bows out once the pixel footprint passes its
  // longest wavelength: past that its mips hold only box-filter residue,
  // which drew metre-sized blocks, and its fetches are wasted.
  float mask_macro = smoothstep(175., 280., v_dist); // 500m noise
  float mask_mid = smoothstep(25., 40., v_dist);
  float fade_macro = mask_macro * lodFade(31.0, fpL);
  float fade_mid = mask_mid * lodFade(9.0, fpL);
  // The fine scale used to bow out on the steepest faces to save fetches
  // (three per plane then, one now); that left a close wall with nothing
  // under the mid scale's 0.55 m, which read blurry at 50 m.
  float fade_fine = lodFade(1.25, fpL);
  if (fade_macro + fade_mid + fade_fine <= 0.0)
    return geomNormal;

  // Determine roughness
  const vec4 roughnessWeights =
      vec4(0.6 /* rock */, 0.9 /* grass */, 0.85 /* forest */, 0.2 /* ice */);
  float targetRoughness = dot(detailWeights, roughnessWeights);
  float material_roughness = mix(roughness, targetRoughness, 0.8);

  // Water/Ice gloss fix, eased in with the ice: a threshold on the
  // height-blended weight switched the bump's strength in patches.
  material_roughness = mix(material_roughness, min(material_roughness, 0.3),
                           smoothstep(0.05, 0.2, detailWeights.a));

  float bumpStrength = 4.0 * material_roughness;

  // The land-cover classes blend their detail weights (a meadow is 0.1
  // rock, 0.85 grass, 0.05 forest), which is right for colour but would put
  // rock fractures and tree crowns on every meadow: the bump takes the
  // weights cubed, so the dominant material owns it and only a genuinely
  // mixed class (or the rock that steep slopes add) blends.
  // No floor under the sums: at fp16 one small enough to stand for nothing
  // (1e-6) is subnormal, which a Mali flushes to zero, and water, which has
  // no detail weights at all, came out as 0/0 -- a NaN that went through to
  // the normal, and through the blend with the water's own whatever its
  // weight, so that the lake mirrored the sky from every angle.
  vec4 bw = detailWeights * detailWeights * detailWeights;
  float bwSum = dot(bw, vec4(1.0));
  bw = bwSum > 1e-4 ? bw / bwSum : vec4(0.0);
  // Rock's fine and mid amplitudes are several times grass's, so even a third
  // of rock (sparse-vegetation slopes, plus what steepness adds) owned their
  // bump, and its creases read as combed hair there. Rock takes its share
  // only where it is the majority: outcrops, where the height blend lifts it,
  // and cliffs.
  bw.r *= smoothstep(0.4, 0.6, bw.r);
  bwSum = dot(bw, vec4(1.0));
  bw = bwSum > 1e-4 ? bw / bwSum : vec4(0.0);

  // Share of each bump band [4..31], [0.55..9], [0.08..1.25] m that the
  // relief data cannot hold: single log2(ds) + affine remap, scaled by
  // bumpStrength.
  //   BAND_INV_SPAN = 1 / log2(lmax / lmin)
  //   BAND_OFF      = log2(3 / lmin) / log2(lmax / lmin)
  const vec3 BAND_INV_SPAN = vec3(0.3385075, 0.2479539, 0.2521600);
  const vec3 BAND_OFF = vec3(-0.1404921, 0.6068757, 1.3185003);
  float ds = min(dataStep, 30.0) / max(geomNormal.z, 0.25);
  vec3 band =
      clamp(log2(ds) * BAND_INV_SPAN + BAND_OFF, 0.0, 1.0) * bumpStrength;

  // One fetch per open projection plane per scale (past [triplanarSideGate]
  // two planes, below it the XY plane alone), accumulated directly in world
  // space so no screen-space basis (dPdx/dPdy/r1/r2/invDet) needs to stay
  // live across the bump fetches (eliminates register spilling on Mali).
  vec3 grad = vec3(0.);
  if (fade_macro > 0.0) {
    float ampA = dot(bw, BUMP_AMP_A[0]) * (band.x * fade_macro);
    if (ampA != 0.0) {
      // Forest: two copies of the layer, alternating over zones 700 m to
      // 1.1 km across (a mask of incommensurate sinusoids): flat forest showed
      // the 500 m tile repeating, with nothing in the relief to break it up.
      float mix2 = 0.0;
      if (bw.b > 0.05) {
        highp vec2 q = v_world_pos.xy;
        highp float wn = sin(q.x * 0.0071 + 0.4) * sin(q.y * 0.0089 + 2.0) +
                         0.5 * sin((0.8 * q.x + 0.6 * q.y) * 0.0057 + 1.0);
        mix2 = smoothstep(-0.35, 0.35, float(wn)) * smoothstep(0.05, 0.3, bw.b);
      }
      grad += bumpGradMacro(triW, v_world_pos, ampA, mix2);
    }
  }
  if (fade_mid > 0.0) {
    vec2 amp = vec2(dot(bw, BUMP_AMP_A[1]), dot(bw, BUMP_AMP_B_MID)) *
               (band.y * fade_mid);
    if (any(notEqual(amp, vec2(0.0))))
      grad +=
          bumpGrad(triW, v_world_pos, BUMP_SCALE_MID, 1, amp, vec2(0.866, 0.5));
  }
  if (fade_fine > 0.0) {
    vec2 amp = vec2(dot(bw, BUMP_AMP_A[2]), dot(bw, BUMP_AMP_B_FINE)) *
               (band.z * fade_fine);
    if (any(notEqual(amp, vec2(0.0))))
      grad += bumpGrad(triW, v_world_pos, BUMP_SCALE_FINE, 2, amp,
                       vec2(0.454, 0.891));
  }

  // Project the analytic 3D gradient onto the surface's tangent plane.
  vec3 surfGrad = grad - dot(grad, geomNormal) * geomNormal;
  return normalize(geomNormal - surfGrad);
}

// Procedural water with organic shoreline.
// The implicit-LOD texture() below sits in non-uniform control flow, which
// the spec leaves undefined -- accepted here: [waterFactor] is bilinearly
// interpolated so the branch is quad-coherent except on the shoreline-edge
// quads, where the worst case is one wrongly-mipped sample of decorative
// jitter (and mipmapping drives the noise to 0.5 at range anyway). If
// shoreline sparkle is ever observed on a device, the defined-behavior fix
// is dFdx/dFdy on the uv outside the branch + textureGrad inside; it was
// not applied preemptively because textureGrad takes a slower sampling path
// on mobile GPUs. Same reasoning covers the wave fetches gated on
// [waterMask] in applyWaterEffects.
float getWaterMask(highp vec2 worldPos, float waterFactor) {
  if (waterFactor >= 0.01 && waterFactor < 0.99) {
    float noise_val = texture(u_detailMap, worldPos.xy * 0.2).r;
    float jitter = (noise_val - 0.5) * 0.5;

    // Sharp threshold with smooth transition
    float threshold = 0.5 + jitter;
    return smoothstep(threshold - 0.15, threshold + 0.15, waterFactor);
  }
  return waterFactor;
}

// [worldPos] is a world coordinate (metres) and must be highp: at fp16 the
// foam UV below would snap into blocks metres across.
vec3 applyWaterEffects(vec3 baseColor, float waterMask, highp vec2 worldPos) {
  if (waterMask < 0.01)
    return baseColor;

  // Deep water color (linear space)
  vec3 waterColor = vec3(0.01, 0.04, 0.12);

  // Shallow water reads greener: the mask's shore band doubles as a depth
  // cue (lakes are coverage-rasterized, there is no bathymetry).
  vec3 shallowColor = vec3(0.035, 0.10, 0.11);
  waterColor = mix(shallowColor, waterColor, smoothstep(0.5, 0.95, waterMask));

  // Shoreline foam zone
  float shoreZone =
      smoothstep(0.2, 0.6, waterMask) * (1.0 - smoothstep(0.6, 1.0, waterMask));
  vec3 foamColor = vec3(0.35, 0.40, 0.45);

  vec3 result = mix(baseColor, waterColor, waterMask);
  if (shoreZone > 0.01) {
    // Patchy foam from the detail noise (was an axis-aligned sine grid);
    // the fetch is gated on the continuous shore band, see the UB note
    // above getWaterMask.
    float foam =
        smoothstep(0.35, 0.75, texture(u_detailMap, worldPos * 0.08).a);
    result = mix(result, foamColor, shoreZone * foam * 0.5);
  }
  return result;
}

// ========== Shadow Functions ==========

// [coords] and [compare] must be highp. A shadow-map UV is in [0, 1] over 2048
// texels, so one texel is 4.88e-4 -- exactly the fp16 ulp there: at mediump the
// five taps below collapse onto one texel and the comparison depth snaps in
// steps worth tens of metres of world depth, which is the classic acne
// striping along the terrain's contours.
float pcf_shadow(int layer, highp vec2 coords, highp float compare,
                 highp vec2 texel_size) {
  float result = 0.0;
  // 5-tap Cross Pattern (Center + 4 neighbors) - Faster than 9-tap box
  result += texture(shadow_map, vec4(coords, float(layer), compare));
  result += texture(
      shadow_map, vec4(coords + vec2(texel_size.x, 0), float(layer), compare));
  result += texture(
      shadow_map, vec4(coords - vec2(texel_size.x, 0), float(layer), compare));
  result += texture(
      shadow_map, vec4(coords + vec2(0, texel_size.y), float(layer), compare));
  result += texture(
      shadow_map, vec4(coords - vec2(0, texel_size.y), float(layer), compare));
  return result / 5.0;
}

// ========== Output Dither ==========

// Interleaved gradient noise. Must be highp: at mediump (fp16 on mobile) the
// gl_FragCoord products lose the low bits and the hash collapses to a handful
// of values.
highp float IGN(highp vec2 p) {
  highp vec3 magic = vec3(0.06711056, 0.00583715, 52.9829189);
  return fract(magic.z * fract(dot(p, magic.xy)));
}

// ========== Main ==========

uniform vec3 u_fogColor;
uniform vec3 u_zenithColor;

// Sun against sky, on a slope facing the sun: about 7 to 1, which is what a
// clear day at altitude measures. Both terms end up multiplied by the albedo,
// so their ratio is what sets how far a shaded slope falls below a lit one, and
// it used to be 1 to 1 -- see the ambient term itself, further down, for what
// that cost.
//
// AMBIENT_LIGHT is not the lever on shaded ground that it looks like, and the
// reason lies outside this file: at 5 km the haze puts about 0.015 of airlight
// in front of a sunless slope that is emitting 0.005 of its own, so three
// quarters of what the eye lands on there is atmosphere. Taking this from 0.15
// to 0.25 moves such a slope by 0.015 of the output range and no more.
// DIRECT_LIGHT does move them, by moving everything the sun reaches instead, so
// turning it down buys open shadows with relief.
const float DIRECT_LIGHT = 1.0;
const float AMBIENT_LIGHT = 0.15;

void main() {
  // Decode normal from the relief normal textures, composited inside-out so
  // each ring fades over the coarser ones beneath it. Evaluates outer fades
  // lazily (1 fetch inside any ring, 2 on a ring border) with 6 texture sites
  // total instead of 14.
  mediump vec2 encodedN = vec2(0.0);
  float data_step = 0.0;
  mediump float rem = 1.0;
  mediump float f0 = ringFade(hd_valid[0], hdReliefCoord0);
  if (f0 > 0.0) {
    encodedN = texture(hd_relief_normal[0], hdReliefCoord0).rg * f0;
    data_step = hd_step[0] * f0;
    rem -= f0;
  }
  if (rem > 0.0) {
    mediump float f1 = ringFade(hd_valid[1], hdReliefCoord1);
    if (f1 > 0.0) {
      mediump float w = rem * f1;
      encodedN += texture(hd_relief_normal[1], hdReliefCoord1).rg * w;
      data_step += hd_step[1] * w;
      rem -= w;
    }
  }
  if (rem > 0.0) {
    mediump float f2 = ringFade(hd_valid[2], hdReliefCoord2);
    if (f2 > 0.0) {
      mediump float w = rem * f2;
      encodedN += texture(hd_relief_normal[2], hdReliefCoord2).rg * w;
      data_step += hd_step[2] * w;
      rem -= w;
    }
  }
  if (rem > 0.0) {
    mediump float f3 = ringFade(hd_valid[3], hdReliefCoord3);
    if (f3 > 0.0) {
      mediump float w = rem * f3;
      encodedN += texture(hd_relief_normal[3], hdReliefCoord3).rg * w;
      data_step += hd_step[3] * w;
      rem -= w;
    }
  }
  mediump float fb = baseFade();
  if (rem > 0.0) {
    encodedN += baseNormal(fb) * rem;
    data_step += 30.0 * rem;
  }
  // Baked AO measures sky visibility: sampled early while [fb] is live so it
  // does not stay in a register across the entire material/bump/shadow block.
  // Attenuates the ambient term and the sky reflection, not the direct sun,
  // which the shadow map already handles (multiplying the whole color darkened
  // sunlit couloirs twice). The far field has its own bake beyond the base
  // grid, faded into like the normals.
  float occlusion = 1.0;
  if (fb < 1.0 && far_valid)
    occlusion = texture(far_ao, farReliefCoord()).r;
  if (fb > 0.0)
    occlusion = mix(occlusion, texture(ao, reliefCoord).r, fb);
  vec3 normal;
  normal.xy = encodedN * 2.0 - 1.0;
  normal.z = sqrt(max(0.0, 1.0 - dot(normal.xy, normal.xy)));

  vec3 lightDir = u_lightDir; // Pre-normalized on CPU
  float cosTheta = clamp(dot(normal, lightDir), 0.0, 1.0);

  // === Material System ===
  float slope = 1.0 - normal.z;
  vec3 terrain_color;
  vec3 final_normal = normal;

  // === CLC-Based Material ===
  // The cover map's outermost level spans the far field, twice the base
  // grid's side about the same centre.
  Surface surface =
      sampleCLCBilinear((reliefCoord - u_coverShift - 0.5) * 0.5 + 0.5);

  // === Canopy (LIDAR HD) ===
  // Inside the 1 m ring's block, the canopy height says where the trees are:
  // their real edges, clearings, corridors and lone trees, in place of land
  // cover drawn to 25 ha. Where it disagrees with the class, the detail
  // weights move to forest or to the class's non-forest mix, and the albedo to
  // forest green (by the conifer share) or to meadow green. Not on water, nor
  // on built-up classes, whose buildings the canopy model reads as trees.
  {
    highp vec2 d_eye = center_offset - v_world_pos.xy;
    highp float r_eye_sq = dot(d_eye, d_eye);
    highp float r_eye = sqrt(r_eye_sq);
    highp vec2 cn = u_canopySlot >= 0 ? slotCoord(u_canopySlot) : vec2(0.0);
    highp vec2 cf =
        u_canopyFarSlot >= 0 ? slotCoord(u_canopyFarSlot) : vec2(0.0);
    float nearEdge = u_canopySlot >= 0 ? canopyEdge(cn) : 0.0;
    float farEdge = u_canopyFarSlot >= 0 ? canopyEdge(cf) : 0.0;
    vec3 ct = canopyTrees(cn, cf, nearEdge, farEdge);
    if (surface.waterFactor < 0.99)
      canopyGeometry(r_eye, cn, cf, nearEdge, farEdge);
    // Height above the sea: compensate Earth's curvature drop.
    highp float elev = v_world_pos.z + center_height + r_eye_sq * 6.8306e-8;
    // Beyond the canopies, the EEA tree cover: forest where its density is
    // over ~30-60%. Its leaf type (10 m) says conifer or broadleaf wherever it
    // has trees, inside the canopies too: finer than the land cover's classes,
    // and what tree_find.vert picks the trees' species by.
    // The finer level first, then the coarser one for what is left. The
    // cover misses sparse high forest (open mountain pine and larch: at
    // 44.58, 6.85 it reads 0-9% on 70% of the LIDAR's trees over 3 m), so
    // where it has no trees but the land cover says forest, that is sparse
    // forest, half trees, rather than meadow.
    surface.conifer =
        mix(surface.conifer, 1.0,
            smoothstep(u_coniferLine, u_coniferLine + 500.0, elev));
    if (u_treeCoverOn || u_treeCoverFarOn) {
      highp vec2 drc_dx = dFdx(reliefCoord);
      highp vec2 drc_dy = dFdy(reliefCoord);
      float coverTrust = 0.0;
      float clcForest = 0.5 * smoothstep(0.35, 0.6, surface.detailWeights.b);
      for (int level = 0; level < 2; level++) {
        if (level == 0 ? !u_treeCoverOn : !u_treeCoverFarOn)
          continue;
        highp vec4 m = level == 0 ? u_treeCoverMap : u_treeCoverFarMap;
        highp vec2 cu = reliefCoord * m.xy + m.zw;
        float ce = canopyEdge(cu);
        if (ce > 0.0 && coverTrust < 1.0) {
          // cu is affine in reliefCoord and unclamped while ce > 0.
          highp vec2 gradScale = m.xy / vec2(3.0, 2.0);
          vec4 tc = textureGrad(u_canopy, canopyCoverUV(cu, level),
                                drc_dx * gradScale, drc_dy * gradScale);
          float e = ce * tc.b * (1.0 - coverTrust);
          if (e > 0.0) {
            float density = tc.r / tc.b;
            float total = ct.y + (1.0 - ct.y) * e;
            ct.x = mix(max(smoothstep(0.3, 0.6, density), clcForest), ct.x,
                       ct.y / total);
            ct.y = total;
            // Only as far as the leaf type is known: elsewhere the conifer
            // line's share, or the class's, stands (as in tree_find.vert). With
            // G premultiplied by A, mix(conifer, G / A, w A) without the
            // division.
            surface.conifer += (tc.g - surface.conifer * tc.a) * e *
                               smoothstep(0.1, 0.4, density);
            coverTrust += e;
          }
        }
      }
    }
    // No forest on glaciers -- the canopy models read seracs and crevasses
    // as a metre or two of trees -- nor above the treeline.
    ct.x *= (1.0 - surface.detailWeights.a) *
            (1.0 - smoothstep(2500.0, 2800.0, elev));
    float trust = ct.y * (1.0 - surface.built) *
                  (1.0 - smoothstep(0.3, 0.6, surface.waterFactor));
    // Where tree.vert stands the trees, this is the ground under them: no
    // canopy, crowns or forest bump of its own, but forest floor -- the open
    // ground's detail, and needle litter and moss for colour.
    float stood = treesStood(r_eye, g_canopyStands);
    float floorShare = ct.x * stood;
    ct.x *= 1.0 - stood;
    g_canopyReal = trust * ct.x * g_canopyNear;
    if (trust > 0.0) {
      vec4 w = surface.detailWeights;
      float isForest = smoothstep(0.35, 0.6, w.b);
      vec4 ground = vec4(w.r, w.g, 0.0, w.a);
      float gs = dot(ground, vec4(1.0));
      ground = gs > 0.05 ? ground / gs : vec4(0.1, 0.9, 0.0, 0.0);
      // Treeless ground on a steep slope is mostly rock and scree, not
      // meadow: from ~32 to ~49 degrees, below where the steepness rule of
      // [applySlopeModification] takes over. Not ice, which stays on a
      // glacier's slopes until that rule thins it.
      float bare = smoothstep(0.15, 0.35, slope) * (1.0 - ground.a);
      ground = mix(ground, vec4(1.0, 0.0, 0.0, 0.0), bare);
      surface.detailWeights = mix(
          w, mix(ground, vec4(0.0, 0.0, 1.0, 0.0), ct.x) * dot(w, vec4(1.0)),
          trust);
      // Linear palette albedos of broadleaved (311) and coniferous (312)
      // forest; for open ground, natural grassland (321) muted towards
      // moorland (322) -- grassland alone made clearings lime green, brighter
      // than the meadows around -- and bare rock (as [applySlopeModification])
      // where it is steep.
      vec3 forestAlbedo = mix(vec3(0.0170, 0.1009, 0.0127),
                              vec3(0.0127, 0.0676, 0.0170), surface.conifer);
      vec3 groundAlbedo =
          mix(vec3(0.116, 0.205, 0.048), vec3(0.21, 0.19, 0.17), bare);
      vec3 target = mix(groundAlbedo, forestAlbedo, ct.x);
      surface.albedo =
          mix(surface.albedo, target, trust * abs(ct.x - isForest));
      surface.albedo =
          mix(surface.albedo, vec3(0.038, 0.055, 0.018), trust * floorShare);
    }
  }

  highp float v_dist = length(vec3(center_offset, 0.0) - v_world_pos);
  applySlopeModification(surface, slope);

  // Sample packed detail map via triplanar projection
  highp float scale = 0.002; // ~500m per texture repeat
  vec3 triW;
  vec4 texNoise = sampleTriplanar(v_world_pos, normal, scale,
                                  triplanarSideGate(normal), triW);

  // -----------------------------------------------------------------------
  // STEP A: HEIGHT-BASED BLENDING ("Clumping")
  // -----------------------------------------------------------------------
  // Solves "Ghosting" in sparse areas (e.g. 50% Rock / 50% Grass).
  // Boosts the channel that matches the texture noise (High noise = Top layer).

  // Attenuate grass noise to simulate low height variation (flatness)
  // Rock (r) stays full strength. Grass (g) is compressed towards 0.5.
  vec4 blendNoise = texNoise;
  blendNoise.g = mix(0.5, texNoise.g, 0.3);

  vec4 heightWeights = surface.detailWeights * (blendNoise + 0.2);
  // Optimize pow(x, 2.0) -> x * x
  heightWeights = heightWeights * heightWeights;

  // Re-normalize
  float weightSum = dot(heightWeights, vec4(1.0));
  vec4 finalWeights =
      (weightSum > 0.001) ? (heightWeights / weightSum) : surface.detailWeights;

  // Forest crowns at their true size (~7 m): one fetch, on forest only, once
  // a crown spans a few pixels (smaller, filtered crowns flattened the canopy
  // to a lawn, and the canopy photo reads better there). Their height sets the
  // canopy colour and occlusion -- lit tops, dark gaps, in place of the
  // canopy photo's, which at the 500 m macro tile drew 70 m "crowns" -- and
  // their gradient shades each crown, lit on its sun side.
  // Anisotropic (4x) pixel footprint in world metres, shared by crown LOD and
  // [perturbNormal].
  highp vec3 ddx = dFdx(v_world_pos);
  highp vec3 ddy = dFdy(v_world_pos);
  highp float l2x = dot(ddx, ddx);
  highp float l2y = dot(ddy, ddy);
  highp float fp = sqrt(max(0.0625 * max(l2x, l2y), min(l2x, l2y)));

  float crownFade = 0.0;
  float coniferAmt = 0.0;
  float coniferTone = 0.0;
  float crownH = 0.0;
  vec2 crownGrad = vec2(0.0);
  {
    // Conifer stands within the class's share, by a non-repeating value
    // noise over ~60 m: mixed forest comes out as stands of each, not as
    // every tree half of each. [coniferAmt] is 1 where conifers grow.
    if (finalWeights.b > 0.01) {
      float standFade = lodFade(60.0, fp);
      if (standFade > 0.0) {
        coniferAmt =
            surface.conifer <= 0.02
                ? 0.0
                : (surface.conifer >= 0.98
                       ? 1.0
                       : smoothstep(-0.06, 0.06,
                                    surface.conifer -
                                        valueNoise(v_world_pos.xy / 60.0 +
                                                   vec2(53.0, 7.0))));
        // Conifer crowns are smaller: they show from a closer range.
        crownFade = lodFade(mix(4.0, 2.2, coniferAmt), fp);
        // Stands show as tone patches until they are a few pixels across;
        // beyond, the class's average share (no shimmer from a 60 m noise).
        coniferTone = mix(surface.conifer, coniferAmt, standFade);
      } else {
        coniferTone = surface.conifer;
      }
    }
    if (crownFade > 0.0) {
      // No LOD bias (the bump's +1 blurred crowns of a few pixels into soft
      // blobs). Two copies of each species' tile, turned and offset
      // differently, one per zone of a non-repeating value noise (~250 m):
      // random clusters of big trees and gaps within one 131 m tile repeated
      // recognisably over a wide forest. Crowns cannot be averaged, so each
      // zone and each stand takes one sample, and two or more are read only
      // across the narrow transitions.
      if (g_canopyReal < 1.0) {
        float pick = smoothstep(
            0.42, 0.58, valueNoise(v_world_pos.xy / 250.0 + vec2(17.0, 31.0)));
        const highp mat2 M1 =
            mat2(0.921, 0.389, -0.389, 0.921) * (1.0 / CROWN_TILE);
        const highp mat2 M2 =
            mat2(-0.276, 0.961, -0.961, -0.276) * (1.0 / CROWN_TILE);
        highp vec2 uv1 = M1 * v_world_pos.xy;
        highp vec2 uv2 = M2 * v_world_pos.xy + vec2(0.37, 0.61);
        vec3 cs = vec3(0.0);
        if (coniferAmt < 1.0)
          cs = crownSample(6.0, pick, uv1, uv2);
        if (coniferAmt > 0.0)
          cs = coniferAmt < 1.0
                   ? mix(cs, crownSample(7.0, pick, uv1, uv2), coniferAmt)
                   : crownSample(7.0, pick, uv1, uv2);
        // Density varies over ~150 m by a non-repeating value noise: crowns
        // lowered by [thin] and rescaled, so the small and low ones drop out
        // first, leaving clearings in some stands and a closed canopy in
        // others. (Driven by the forest's 500 m colour noise, it repeated with
        // that tile, as large light and dark patches over a big forest.)
        float thin =
            0.12 * smoothstep(0.55, 0.15, valueNoise(v_world_pos.xy / 150.0));
        float invScale = 1.0 / (1.0 - thin);
        crownH = max(cs.x - thin, 0.0) * invScale;
        crownGrad = cs.x > thin
                        ? cs.yz * ((2.0 * CROWN_GMAX / CROWN_TILE) * invScale)
                        : vec2(0.0);
      }
      // Where the LIDAR canopy has trees, the crowns are the real ones: its
      // tone (tops lit, flanks and gaps dark) and their slope beyond what the
      // raised mesh already shades (see [canopyGeometry]), softened to half --
      // a 2 m grid only approximates a crown's surface, and at full slope its
      // facets showed. Divided by CROWN_RELIEF, which the tilt below applies
      // to both.
      if (g_canopyReal > 0.0) {
        crownH = mix(crownH, g_canopy.a, g_canopyReal);
        crownGrad = mix(crownGrad,
                        (g_canopy.gb * (255.0 / 32.0) - 4.0 - g_liftGrad) *
                            (0.5 / CROWN_RELIEF),
                        g_canopyReal);
      }
    }
  }

  // The canopy's tone: dark in the gaps, lit across a crown rather than only
  // at its top -- by height alone, only crown centres were bright, and at zoom
  // a forest read as scattered blobs on black. Averages ~0.42 over a closed
  // canopy, as the canopy photo does where crowns fade out: brighter, the
  // forest turned to a lawn up close and darkened with distance.
  float crownTone =
      crownFade > 0.0
          ? 0.05 + 0.55 * smoothstep(0.0, 0.12, crownH) * (0.6 + 0.4 * crownH)
          : 0.0;

  // -----------------------------------------------------------------------
  // STEP B: BIO-VARIATION (Color & Texture)
  // -----------------------------------------------------------------------

  vec3 accumulatedColor = vec3(0.0);
  vec3 baseAlbedo = surface.albedo;

  // --- ROCK (Red Channel) ---
  if (finalWeights.r > 0.001) {
    float rockNoise = texNoise.r;

    // Sedimentation: Cracks (Low) = Dark Dirt, Tips (High) = Bleached Stone
    vec3 dirtColor = baseAlbedo * vec3(0.6, 0.55, 0.5);
    vec3 stoneColor = baseAlbedo * vec3(1.1, 1.1, 1.15);

    // Use raw texture value for more visible detail
    vec3 rockCol = mix(dirtColor, stoneColor, rockNoise);
    accumulatedColor += rockCol * finalWeights.r;
  }

  // --- GRASS (Green Channel) ---
  if (finalWeights.g > 0.001) {
    float grassNoise = texNoise.g;

    // Micro: Roots (Low) = Dry/Brown, Tips (High) = Lush
    vec3 rootColor = baseAlbedo * vec3(0.8, 0.7, 0.5);
    vec3 tipColor = baseAlbedo * vec3(1.15, 1.25, 1.0);
    vec3 microCol = mix(rootColor, tipColor, grassNoise);

    // Macro: Patches of dying grass (yellowish) vs healthy grass (blue-green)
    // World coordinates, so highp: 0.02 * 70 km is 1400, where the fp16 ulp is
    // a whole unit -- the sine phases below would snap in ~1 radian steps and
    // the patch pattern would break into flat blocks over distant grass.
    highp vec2 macroPos = v_world_pos.xy * 0.02;
    float macroNoise = sin(macroPos.x) * cos(macroPos.y * 0.8) +
                       sin(macroPos.x * 0.5 + macroPos.y * 1.5) * 0.5;
    float patchFactor = smoothstep(0.3, 0.7, macroNoise * 0.5 + 0.5);

    vec3 dyingPatch = vec3(1.2, 1.1, 0.8);
    vec3 healthyPatch = vec3(0.8, 1.1, 1.1);
    vec3 macroMod = mix(dyingPatch, healthyPatch, patchFactor);

    accumulatedColor += (microCol * macroMod) * finalWeights.g;
  }

  // --- FOREST (Blue Channel) ---
  if (finalWeights.b > 0.001) {
    float forestNoise = mix(texNoise.b, crownTone, crownFade);

    // Volume: Deep Shadow (Low) vs Sunlit Canopy (High)
    // Conifers are darker and cooler than broadleaf trees: needles absorb
    // more, and their canopy holds more shadow.
    vec3 species =
        mix(vec3(1.1, 1.12, 0.92), vec3(0.78, 0.85, 0.95), coniferTone);
    vec3 deepShadow =
        baseAlbedo * species * vec3(0.2, 0.25, 0.3); // Very dark/cool
    vec3 sunlitTop = baseAlbedo * species * vec3(1.2, 1.25, 1.0);

    vec3 forestCol = mix(deepShadow, sunlitTop, forestNoise);
    accumulatedColor += forestCol * finalWeights.b;
  }

  // --- ICE (Alpha Channel) ---
  if (finalWeights.a > 0.001) {
    // Blueish crevasses
    float depth = max(0., 1. - 2. * texNoise.a);
    vec3 iceCol = mix(baseAlbedo, baseAlbedo * vec3(0.5, 0.7, 1.), depth);
    accumulatedColor += iceCol * finalWeights.a;
  }

  terrain_color = accumulatedColor;

  // Water handling
  float waterMask = getWaterMask(v_world_pos.xy, surface.waterFactor);
  terrain_color = applyWaterEffects(terrain_color, waterMask, v_world_pos.xy);

  // Procedural normal perturbation (replaces rock_normal_map)
  if (waterMask < 0.99)
    final_normal = perturbNormal(normal, triW, surface.roughness, finalWeights,
                                 data_step, fp, v_dist);
  // Crowns stand vertical whatever the slope under them: each tilts the
  // ground's normal about the vertical axis -- a horizontal offset -- rather
  // than within the slope's tangent plane. As a bump on the slope every crown
  // leaned with it and the pattern followed the slope; replacing the normal
  // outright lost the slope's own light and shade, which real forest keeps.
  // The raised canopy's own slope (the surface is ground plus canopy, so the
  // gradients add): forest edges and the sides of stands shade as walls, not
  // as the ground beneath them. The crown tilt below then adds only the
  // detail beyond this smoothed slope.
  if (dot(g_liftGrad, g_liftGrad) > 0.0) {
    final_normal = normalize(final_normal + vec3(-g_liftGrad, 0.0));
    // The flank of a stand seen from the side: shade under the crowns, broken
    // by the gaps between trunks -- not the top-down crown pattern, smeared
    // down the wall, which made groves read as moss-covered mounds. Bands
    // along the flank's horizontal direction (constant in height, so
    // vertical on screen), ~2.5 m and ~1 m, non-repeating; gone by 3 km,
    // where they would alias.
    float slLen = length(g_liftGrad);
    float wall = smoothstep(0.4, 1.2, slLen) *
                 (1.0 - smoothstep(1500.0, 3000.0, v_dist));
    if (wall > 0.0) {
      vec2 along = vec2(-g_liftGrad.y, g_liftGrad.x) / slLen;
      highp float t = dot(v_world_pos.xy, along);
      float gaps =
          0.65 * valueNoise1D(t * 0.4, 73u) + 0.35 * valueNoise1D(t, 191u);
      terrain_color *=
          mix(1.0, 0.75 * (0.45 + 0.55 * smoothstep(0.3, 0.7, gaps)), wall);
    }
  }
  if (crownFade > 0.0) {
    float onCrown = smoothstep(0.02, 0.2, crownH) * crownFade * finalWeights.b;
    final_normal = normalize(final_normal +
                             vec3(-crownGrad * (CROWN_RELIEF * onCrown), 0.0));
  }

  // Water Wave Logic
  if (waterMask > 0.01) {
    // Distance-based fade: waves visible up to 2km, fully calm by 5km
    float waveFade = 1.0 - smoothstep(2000.0, 5000.0, v_dist);

    // Skip wave calculations if too far (optimization)
    if (waveFade > 0.01) {
      // 1. Low frequency waves (Swell) - Isotropic Interference Pattern
      // Using 3 waves at 120-degree offsets to eliminate directional banding.
      // The domain is warped by the low-frequency noise so the interference
      // lattice's phase drifts from patch to patch instead of staying
      // aligned across the whole lake; the warp is near-constant at the
      // 126 m wavelength, so the analytic derivatives below stay valid.
      highp vec2 waveCoord = v_world_pos.xy * 0.05;
      waveCoord += (texNoise.rb + texNoise.ga - 2.0 * texNoise.gb) * 5.0;

      // Wave 2: 120 degrees, Freq 1.1 (Detuned)
      highp float input2 = (waveCoord.x * -0.5 + waveCoord.y * 0.866) * 1.1;
      // Wave 3: 240 degrees, Freq 1.2 (Detuned)
      highp float input3 = (waveCoord.x * -0.5 - waveCoord.y * 0.866) * 1.2;

      // Analytical Derivatives
      float dw1_dx = cos(waveCoord.x);
      float dw1_dy = 0.0;

      float dw2_base = cos(input2) * 1.1;
      float dw2_dx = dw2_base * -0.5;
      float dw2_dy = dw2_base * 0.866;

      float dw3_base = cos(input3) * 1.2;
      float dw3_dx = dw3_base * -0.5;
      float dw3_dy = dw3_base * -0.866;

      // Wind patchiness: modulate the swell amplitude with two noise fields —
      // the 500 m one already sampled for the terrain, and one at an
      // incommensurate ~1.4 km scale so their product repeats only over tens
      // of kilometres (texNoise alone tiles every 500 m). The product gives
      // real calm/active contrast, like gusts on a lake.
      vec4 gustTex =
          texture(u_detailMap, v_world_pos.xy * 0.00073 + vec2(0.37, 0.71));
      float gust = 0.25 + (2.6 * texNoise.g * gustTex.g);
      vec3 waveNormal = normalize(vec3(-(dw1_dx + dw2_dx + dw3_dx) * gust,
                                       -(dw1_dy + dw2_dy + dw3_dy) * gust,
                                       20.0 // Higher divisor = flatter waves
                                       ));

      // 2. High frequency ripples - Texture based (Rotated & Layered)
      // Use Green/Blue channels as X/Y vector components
      // Sample 1: Base Scale (Green=X, Blue=Y) - Soft Noise
      vec2 r1 = texture(u_detailMap, v_world_pos.xy * 0.15).gb * 2.0 - 1.0;

      // Sample 2: Rotated & Scaled (Green=X, Blue=Y)
      // Rotation breaks grid alignment
      float c_rot = 0.8; // cos(37 deg)
      float s_rot = 0.6; // sin(37 deg)
      highp mat2 rot = mat2(c_rot, -s_rot, s_rot, c_rot);
      highp vec2 uv2 = rot * v_world_pos.xy * 0.24 + vec2(7.1, 3.3);
      vec2 r2 = texture(u_detailMap, uv2).gb * 2.0 - 1.0;

      // Combine vectors
      vec2 rippleXY = (r1 + r2) * (0.45 + (1.2 * gustTex.b));

      // Construct Normal
      // Z-component controls flatness (Higher = Flatter)
      vec3 rippleNormal = normalize(vec3(rippleXY, 8.0));

      // Combine: Swell provides structure, Ripples provide detail
      vec3 waterNormal = normalize(waveNormal + rippleNormal);

      // Fade towards flat normal (0, 0, 1) with distance
      waterNormal = normalize(mix(vec3(0.0, 0.0, 1.0), waterNormal, waveFade));

      // Blend terrain normal with water normal
      final_normal = normalize(mix(final_normal, waterNormal, waterMask));
    } else {
      // Beyond fade distance: use flat vertical normal for water
      final_normal =
          normalize(mix(final_normal, vec3(0.0, 0.0, 1.0), waterMask));
    }
  }

  // Store roughness and reflection properties for lighting
  float material_roughness = surface.roughness;
  float iceAmount = surface.detailWeights.a;

  // Only water and ice reflect the sky the way a mirror does, which is what the
  // [mix] below models: what comes off the surface stands in place of what lies
  // under it. Land used to be given a share of that too, and it was the wrong
  // shape twice over -- substituted rather than added, so it carried none of
  // the material's own colour, and taken from the mirror direction, which on a
  // steep face is near the zenith and has almost no red in it. It ended up
  // supplying more than half the light leaving every slope in shade, more than
  // the ambient term that is meant to carry the sky, so shaded ground lost half
  // its separation between materials and came out very nearly pure blue. Land
  // now takes the sky through the ambient term, and reflects on top of it only
  // what the sheen below adds.
  // Ice mirrors a fixed share; water takes its own from the Fresnel term
  // below, starting from none. Both ease in: as thresholds, ice drew a line
  // where steepness thins it out, and water was given the ice's share over its
  // shore band -- a fifth of the sky at half a mask, where open water seen
  // from above returns a few percent -- which rimmed every lake in pale blue.
  float iceMirror = smoothstep(0.05, 0.2, iceAmount);
  float waterGloss = smoothstep(0.0, 0.1, waterMask);
  float mirror = 0.4 * 0.3 * iceMirror;

  // === Specular, Environment Reflection & Pre-Shadow Lighting ===
  // View direction computed here so it is not kept live across material/bump.
  highp vec3 view_dir_n =
      (vec3(center_offset, 0.0) - v_world_pos) / max(v_dist, 1e-4);
  vec3 view_dir_m = vec3(view_dir_n);
  float view_z = view_dir_m.z;

  // Highp, and guarded: looking straight at the sun past a ridge the two
  // directions cancel, and at fp16 they do so exactly over a ~5e-4 rad patch,
  // where normalize() would return NaN.
  highp vec3 halfSum = u_lightDir + view_dir_n;
  highp vec3 halfVec = halfSum * inversesqrt(max(dot(halfSum, halfSum), 1e-12));

  // GGX distribution with a hand-tuned strength in front of it, which is not a
  // reflectance and does hand a quarter of the light on sunlit rock and grass
  // -- and two fifths of it on snow -- to a white, view-dependent wash.
  // Completing it into a BRDF, with the Fresnel and geometry terms it is
  // missing, was tried and put back: a view down a valley towards the sun is
  // grazing and backlit at once, and single-scattering GGX answers that with
  // more sheen than this, not less. It took the specular to nearly half the
  // light on sunlit ground, and cost snow and rock a tenth of their local
  // contrast. Capping the Fresnel climb at 1 - roughness, which is the usual
  // stand-in for the multiple scattering that would damp it, brought the share
  // back but not the contrast. What is wrong with this term is its shape rather
  // than its size, and shape is not what the app is short of.
  float NdotH = max(0.0, dot(final_normal, halfVec));
  float NdotV_raw = dot(final_normal, view_dir_m);
  float NdotV = clamp(NdotV_raw, 1e-3, 1.0);
  float roughSq = material_roughness * material_roughness;
  // NdotH^2 (roughSq - 1) + 1, rewritten as (1 - NdotH^2) + NdotH^2 roughSq
  // with 1 - NdotH^2 taken as |N x H|^2: the direct form subtracts two numbers
  // near 1, which at fp16 moves in steps of ~1e-3 -- the whole width of the
  // glint's core on water (roughness 0.05-0.15), so it came out in a few hard
  // rings. Squared in highp, where the core's ~1e-5 is not subnormal. The
  // identity needs the unclamped N.H, so a half vector behind the surface
  // keeps the clamped value's denominator of 1 (the two agree at N.H = 0).
  highp vec3 NxH = cross(final_normal, halfVec);
  highp float a = NdotH * material_roughness;
  highp float denom = (NdotH > 0.0) ? dot(NxH, NxH) + a * a : 1.0;
  float D = roughSq / (3.14159 * denom * denom + 0.0001);
  float final_l = max(0.0, dot(final_normal, lightDir));
  float specBoost =
      mix(1.0, 2.5, max(waterGloss, smoothstep(0.4, 0.6, iceAmount)));
  float specTerm = D * final_l * (1.0 - material_roughness) * specBoost * 0.4;

  // Fresnel for water and grazing sheen for land.
  float oneMinusNdotV = 1.0 - NdotV;
  float om2 = oneMinusNdotV * oneMinusNdotV;
  float om5 = om2 * om2 * oneMinusNdotV;
  if (waterMask > 0.01) {
    // Schlick's approximation
    float fresnel = 0.02 + 0.98 * om5;
    mirror = mix(mirror, fresnel, waterMask);
  }
  // What land returns of the sky: a few percent face-on, rising towards
  // grazing, and added on top of the material rather than in place of it.
  // Scaled by AMBIENT_LIGHT because it is the same sky the ambient term carries
  // and has to stay in proportion to it, and by smoothness because a rough
  // surface scatters it too wide to show.
  float sheenAmt = (1.0 - iceMirror) * (1.0 - waterGloss) *
                   (1.0 - material_roughness) * (0.04 + 0.6 * om5) *
                   AMBIENT_LIGHT;

  // A canopy is not a Lambertian surface: it is crowns standing proud of the
  // ground, each with a lit and a shaded side and a shadow behind it. With
  // the sun behind the viewer the lit sides face the eye and the shadows hide
  // behind the crowns (a hotspot right at the antisolar point); looking into
  // the sun, mostly shaded sides show, with rims glowing where light comes
  // through the edges. Lambert has none of this, which is much of why forest
  // read as a flat carpet. [cosg] is the cosine of the phase angle (1 with
  // the sun straight behind the viewer); the factor is 1 side-on, so forest
  // keeps its overall brightness and only its direction changes.
  float direct_l = final_l;
  if (final_l > 0.0 && finalWeights.b > 0.01) {
    highp float cosg = dot(view_dir_n, u_lightDir);
    float back = max(-float(cosg), 0.0);
    float back3 = back * back * back;
    float phase = clamp(1.0 + 0.6 * float(cosg), 0.25, 2.0) +
                  0.4 * exp((float(cosg) - 1.0) / 0.015) +
                  0.8 * (back3 * back3) * (om2 * oneMinusNdotV);
    direct_l *= mix(1.0, phase, finalWeights.b);
  }

  // Matched to Sky Shader: Horizon -> Zenith
  vec3 sky_color = u_fogColor * 0.8 + u_zenithColor * 0.2;
  vec3 ground_color =
      vec3(0.08, 0.07, 0.05); // Deeper ground bounce for contrast
  float sky_factor = final_normal.z * 0.6 + 0.4; // Weighted more towards sky
  // Rectified Geometric Normal for Micro-AO comparison
  // Use flat normal (0,0,1) for water to avoid dark artifacts at shorelines
  vec3 geoNormal = mix(normal, vec3(0.0, 0.0, 1.0), waterMask);
  // Normal-based Micro-AO: Darken steep crevices
  float normalAO = mix(0.85, 1.0, max(0.0, dot(final_normal, geoNormal)));
  // Noise-based Micro-AO: Darken crevasses/gaps (low noise values)
  vec3 matAO = smoothstep(
      vec3(0.0), vec3(0.6, 0.5, 0.6),
      vec3(texNoise.r, mix(texNoise.b, crownTone, crownFade), texNoise.a));
  float noiseAO = mix(0.5, 1.0, dot(matAO, finalWeights.rba) + finalWeights.g);
  // The two used to arrive in equal measure, which cost the picture the whole
  // of its relief: sunlit and shaded ground came out within a factor of two of
  // each other, every slope carried the blue of the sky more strongly than the
  // colour of its own material, and a band of ridges spanned a fifth of the
  // display range -- little enough to disappear entirely under sunlight falling
  // on the screen. See AMBIENT_LIGHT against the direct term below.
  vec3 ambient = mix(ground_color, sky_color, sky_factor) *
                 (AMBIENT_LIGHT * normalAO * noiseAO * occlusion);

  // Environment reflection (sky color for glossy surfaces): z of
  // reflect(-view_dir_m, final_normal) = 2 * dot(N, V) * N.z - V.z.
  float skyReflect = max(0.0, 2.0 * NdotV_raw * final_normal.z - view_z);
  // Updated to match new sky: Horizon(u_fogColor) -> Zenith(u_zenithColor)
  // Sky Shader uses smoothstep(0.0, 0.35, cos_theta)
  // We use the exact same mixing factor for consistency
  vec3 envOcc =
      mix(u_fogColor, u_zenithColor, smoothstep(0.0, 0.35, skyReflect)) *
      occlusion;

  // Collapse unshadowed and shadowed lighting into two vec3s before the shadow
  // map fetches so none of the material, normal, noise, or view terms stay
  // live across the shadow block (keeps peak register footprint minimal).
  // Diffuse lighting scales the albedo; reflected and specular light are added
  // on top so grazing lakes do not darken by the diffuse term.
  vec3 base_lit =
      mix(ambient * terrain_color, envOcc, mirror) + envOcc * sheenAmt;
  vec3 sun_lit =
      vec3(1.0, 0.95, 0.9) * u_sunTint *
          ((direct_l * (1.0 - mirror) * DIRECT_LIGHT) * terrain_color) +
      vec3(1.0, 0.98, 0.95) * u_sunTint * specTerm;

  // === Shadow Calculation ===
  // Both consumers of shadow_val (specular and direct light, combined in
  // sun_lit above) are multiplied by final_l, so the 5-tap PCF is pure waste
  // on unlit fragments. The branch is quad-coherent (whole slopes face away).
  float shadow_val = 1.0;
  if (final_l > 0.0) {
    int cascade = 2;
    if (v_dist < shadow_splits[0])
      cascade = 0;
    else if (v_dist < shadow_splits[1])
      cascade = 1;

    // One shadow texel in world units (x) and depth span (y) for this cascade.
    highp vec2 casc = shadow_cascades[cascade];
    float normalOffset = casc.x * (1.0 - cosTheta);
    // World coordinates, and everything derived from them down to the depth
    // comparison, must stay highp: at fp16 [offset_pos] alone is quantised to
    // 2 m at 3 km and 16 m at 20 km, and [proj_coords.z] to 10/23/73 m of
    // world depth per cascade -- far more than the bias below is sized for.
    highp vec4 swp = vec4(v_world_pos + vec3(0., 0., center_height), 1.0);
    highp vec3 proj_coords =
        shadow_matrices[cascade] * (swp + vec4(normal * normalOffset, 0.0));

    // World-space depth bias, expressed in this cascade's [0,1] depth range
    // (20/48/150 km): a small constant for sun-facing surfaces plus a
    // slope-proportional term covering the depth a shadow texel
    // (2.93/11.7/36.6 m) spans on surfaces tilted toward the light's
    // direction. Too little and the texel rows band such slopes (acne); a
    // large constant (the previous 30/72/225 m) detaches every shadow from
    // its caster instead.
    float slopeTan = min(4.0, sqrt(max(0.0, 1.0 - cosTheta * cosTheta)) /
                                  max(cosTheta, 0.1));
    highp float ref_depth =
        proj_coords.z - (4.0 + 1.5 * casc.x * slopeTan) / casc.y;
    // Only inside the cascade's box: past it (the last one's ends at 37.5 km,
    // and the ground goes on to 140) the clamped fetches would stretch its
    // border texels outwards. The last cascade fades out over its outer 5%,
    // so that its shadows do not stop at a line.
    vec2 inBox = min(vec2(proj_coords.xy), 1.0 - vec2(proj_coords.xy));
    float edge = min(inBox.x, inBox.y);
    if (proj_coords.z <= 1.0 && proj_coords.z >= 0.0 && edge > 0.0) {
      shadow_val =
          pcf_shadow(cascade, proj_coords.xy, ref_depth, vec2(0.000488));
      if (cascade == 2)
        shadow_val = mix(1.0, shadow_val, smoothstep(0.0, 0.05, edge));
    }
    // The trees' shadows, at ~0.5 m per texel: the layer holds nothing else,
    // so a small bias does (a trunk's buried foot is below the ground).
    if (u_treeShadowOn && v_dist < 600.0) {
      highp vec3 t_coords = u_treeShadowMatrix * swp;
      if (all(greaterThan(t_coords.xy, vec2(0.0))) &&
          all(lessThan(t_coords.xy, vec2(1.0))) && t_coords.z >= 0.0 &&
          t_coords.z <= 1.0)
        shadow_val *= pcf_shadow(3, t_coords.xy, t_coords.z - 0.3 / 4000.0,
                                 vec2(0.000488));
    }
  }

  vec3 lit = base_lit + sun_lit * shadow_val;

  // === Fog & Haze ===
  // Blend toward the zenith colour above the eye line exactly as sky.frag
  // does (-view.z is the sky's cos_theta), so fogged ridges meet the sky
  // without a tonal step. At or below the eye line the smoothstep is 0 and
  // this is exactly u_fogColor.
  vec3 fog_color =
      mix(u_fogColor, u_zenithColor, smoothstep(0.0, 0.35, -view_z));
  // Height-based haze, exp(-h/H) with H~1667m, taken at the mean altitude of
  // the camera-to-fragment path rather than the fragment's own: with the
  // fragment altitude alone, a low valley seen from a summit was over-hazed
  // by the dense air it does not actually lie under.
  float haze_density = exp((-center_height - (0.5 * v_world_pos.z)) * 0.0006);
  // Divide by -25000 in highp: -0.4e-4 is subnormal in fp16 (< 6.1e-5) and
  // could flush to zero if reassociated with mediump haze_density.
  highp float fog_arg = (v_dist / -25000.0) * haze_density;
  float fog_coeff = exp(fog_arg);

  vec3 final_color = mix(fog_color, lit, fog_coeff);

  // Tone curve and gamma correction, then a +/-0.5/255 dither in output space
  // to break up banding
  highp float noise = IGN(gl_FragCoord.xy);
  color = vec4(
      pow(tone_map(final_color), vec3(1.0 / 2.2)) + (noise - 0.5) / 255.0, 1.0);
}
