precision mediump float; // Default mediump for mobile performance
precision highp sampler2DArray;

// Encoded normal only (RG8). The heights live in a separate RG8 texture that
// the vertex stage declares as `relief`; this stage never reads them.
uniform highp sampler2D relief_normal;
// Encoded normals of the near-field refinement rings (see [Hd_dem]), innermost
// first. Selected by the same extent tests as the vertex stage; [hd_valid] is
// shared with it, and bool needs no precision qualifier, so the two
// declarations cannot disagree.
#define HD_SLOTS 3
uniform bool hd_valid[HD_SLOTS];
uniform highp sampler2D hd_relief_normal[HD_SLOTS];
// Each ring's sample pitch in metres (the coarser axis). The bump weighs
// itself against it per pixel in [perturbNormal]: footprints far below the
// pitch show scales no grid sample can express, footprints at the pitch and
// above show exactly the band the data owns (see [Render_state]).
uniform mediump float hd_step[HD_SLOTS];
uniform mediump sampler2D ao;
uniform mediump sampler2D
    u_detailMap; // Packed RGBA: R=Rock, G=Grass, B=Forest, A=Ice
uniform highp sampler2DArrayShadow shadow_map; // Hardware shadow comparison

// CLC Material System Uniforms
uniform mediump usampler2DArray u_coverMap; // CLC ID clipmap (layers)
uniform mediump sampler2D u_paletteTex;     // 128x1 RGBA palette
uniform int u_numLevels;                    // Number of clipmap levels
uniform highp mat4 shadow_matrices[3];      // Must be highp for projection
uniform mediump float shadow_splits[3];
uniform vec3 u_lightDir; // Pre-normalized on CPU
uniform highp float center_height;

in highp vec2 reliefCoord; // Highp for texture coords
in highp vec2 hdReliefCoord0;
in highp vec2 hdReliefCoord1;
in highp vec2 hdReliefCoord2;
in highp float v_dist;
in highp vec3 v_view_dir;
in highp vec3 v_world_pos; // Highp for world coords

out mediump vec4 color;

// ========== CLC Material System ==========

struct Surface {
  vec3 albedo;
  float roughness;
  vec4 detailWeights; // RGBA = Rock, Grass, Forest, Ice
  float waterFactor;
};

// Lookup surface properties from palette by material ID
Surface getSurfaceFromID(uint id) {
  Surface s;
  // Use texelFetch for direct integer addressing (no filtering overhead)
  ivec2 coord = ivec2(int(id) * 2, 0);
  vec4 pixelA = texelFetch(u_paletteTex, coord, 0);
  vec4 pixelB = texelFetch(u_paletteTex, coord + ivec2(1, 0), 0);

  // Palette is pre-baked with linear values (gamma decoded on CPU)
  s.albedo = pixelA.rgb;
  s.roughness = pixelA.a;
  s.detailWeights = vec4(pixelB.rgb, 0.0); // RGB weights, ice computed below
  s.waterFactor = pixelB.a;

  // ICE LOGIC: If RGB weights are ~0 and not water, force ice (alpha channel)
  float weightSum = dot(pixelB.rgb, vec3(1.0));
  if (weightSum < 0.05 && s.waterFactor < 0.5) {
    s.detailWeights.a = 1.0; // Force ice/snow weight
  }

  return s;
}

// Manual bilinear filtering of CLC IDs (blend Surface properties)
Surface sampleCLCBilinear(highp vec2 uv) {
  // Distance-based LOD (Coverage Constraint)
  // The concentric clipmaps have limited extent. Level L covers radius 0.5 /
  // 2^(6-L). We must choose a level large enough to cover the current UV.
  highp vec2 dist_from_center = abs(uv - 0.5);
  highp float d_center = max(dist_from_center.x, dist_from_center.y);
  // Avoid log2(0)
  d_center = max(d_center, 0.000001);

  // Formula: Level > 7 + log2(d)   (Assumes u_numLevels=7)
  float level_pos = ceil(float(u_numLevels) + log2(d_center));

  int level = clamp(int(level_pos), 0, u_numLevels - 1);

  // Calculate texture coordinates for this concentric level
  // Level 6 covers the whole tile (scale 1x)
  // Level 0 covers 1/64th (scale 64x)
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
  highp vec2 frac = fract(texelPos);

  // Clamp to valid range
  ivec2 maxCoord = ivec2(texSize) - 1;
  ivec2 p00 = clamp(p00_raw, ivec2(0), maxCoord);
  ivec2 p10 = clamp(p00_raw + ivec2(1, 0), ivec2(0), maxCoord);
  ivec2 p01 = clamp(p00_raw + ivec2(0, 1), ivec2(0), maxCoord);
  ivec2 p11 = clamp(p00_raw + ivec2(1, 1), ivec2(0), maxCoord);

  // Sample 4 neighbors from selected array layer
  // usampler2DArray fetch returns uvec4, we take .r component
  uint id00 = texelFetch(u_coverMap, ivec3(p00, level), 0).r;
  uint id10 = texelFetch(u_coverMap, ivec3(p10, level), 0).r;
  uint id01 = texelFetch(u_coverMap, ivec3(p01, level), 0).r;
  uint id11 = texelFetch(u_coverMap, ivec3(p11, level), 0).r;

  // Fast path: skip bilinear blend when all 4 texels have the same ID
  if (id00 == id10 && id00 == id01 && id00 == id11) {
    return getSurfaceFromID(id00);
  }

  // Get surfaces for each neighbor
  Surface s00 = getSurfaceFromID(id00);
  Surface s10 = getSurfaceFromID(id10);
  Surface s01 = getSurfaceFromID(id01);
  Surface s11 = getSurfaceFromID(id11);

  // Bilinear blend of surface properties
  Surface result;
  vec3 a0 = mix(s00.albedo, s10.albedo, frac.x);
  vec3 a1 = mix(s01.albedo, s11.albedo, frac.x);
  result.albedo = mix(a0, a1, frac.y);

  float r0 = mix(s00.roughness, s10.roughness, frac.x);
  float r1 = mix(s01.roughness, s11.roughness, frac.x);
  result.roughness = mix(r0, r1, frac.y);

  vec4 w0 = mix(s00.detailWeights, s10.detailWeights, frac.x);
  vec4 w1 = mix(s01.detailWeights, s11.detailWeights, frac.x);
  result.detailWeights = mix(w0, w1, frac.y);

  float wf0 = mix(s00.waterFactor, s10.waterFactor, frac.x);
  float wf1 = mix(s01.waterFactor, s11.waterFactor, frac.x);
  result.waterFactor = mix(wf0, wf1, frac.y);

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

// Triplanar sample of the packed RGBA detail map, kept per-plane: the raw
// samples, the orientation blend and the side-projection fades survive so
// that perturbNormal can reuse the fetches as finite-difference centers.
// Everything else consumes the blend via [triplanarCombine].
struct Triplanar {
  vec4 d_xy, d_xz, d_yz; // raw plane samples (0.5 where a plane is faded out)
  vec3 blend;
  float fade_x, fade_y; // side-projection fade-ins
};

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

Triplanar sampleTriplanar(highp vec3 worldPos, vec3 normal, float scale,
                          float sideGate) {
  Triplanar t;
  t.blend = abs(normal);
  t.blend /= (t.blend.x + t.blend.y + t.blend.z + 0.0001);

  // Sample each projection plane. The side projections are faded by their
  // blend weight and by [sideGate] (see [triplanarSideGate]), and default to
  // the neutral 0.5. What the material blend loses under the gate is a
  // statistically identical share of the same noise, sampled less obliquely.
  float bias = 1.;
  t.d_xy = texture(u_detailMap, worldPos.xy * scale, bias);
  t.d_xz = vec4(0.5);
  t.fade_y = smoothstep(0.003, 0.01, t.blend.y) * sideGate;
  if (t.fade_y > 0.0)
    t.d_xz = texture(u_detailMap, worldPos.xz * scale, bias);
  t.d_yz = vec4(0.5);
  t.fade_x = smoothstep(0.003, 0.01, t.blend.x) * sideGate;
  if (t.fade_x > 0.0)
    t.d_yz = texture(u_detailMap, worldPos.yz * scale, bias);
  return t;
}

// Blend based on surface orientation (Corrected for Z-up)
// Top/Bottom (blend.z) -> XY projection
// Side-Y (blend.y)     -> XZ projection
// Side-X (blend.x)     -> YZ projection
vec4 triplanarCombine(Triplanar t) {
  return (t.d_xy * t.blend.z) + (mix(vec4(0.5), t.d_xz, t.fade_y) * t.blend.y) +
         (mix(vec4(0.5), t.d_yz, t.fade_x) * t.blend.x);
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

float computeHeight(vec4 noise, vec4 weights) {
  const vec4 heightWeight =
      vec4(3. /* rock */, 0.3 /* grass */, 5. /* forest */, 3. /* ice */);
  return dot((noise - 0.5) * heightWeight, weights);
}

// Forward-difference gradient of the detail height in one projection plane,
// around an already-fetched (and already height-converted) center sample:
// continuous and per-pixel where dFdx/dFdy of a sample is constant across
// each 2x2 quad (visibly blocky up close). Holding [weights] fixed across
// the taps also keeps material-boundary weight gradients from reading as
// bumps. [eps] is in world meters; the constant offset leaves the
// implicit-LOD derivatives unchanged, so the taps hit the center's mip.
vec2 planeHeightGrad(highp vec2 pos, float scale, vec4 weights, float h0,
                     highp float eps) {
  float hu = computeHeight(
      texture(u_detailMap, (pos + vec2(eps, 0.)) * scale, 1.), weights);
  float hv = computeHeight(
      texture(u_detailMap, (pos + vec2(0., eps)) * scale, 1.), weights);
  return vec2(hu - h0, hv - h0) / eps;
}

// Triplanar world-space gradient of the detail height at [scale]: the
// per-plane forward differences, lifted onto each projection plane's axes
// and combined with [t]'s orientation blend and fades held fixed. [t]'s raw
// samples are the finite-difference centers -- for the macro scale they are
// already paid for by the material blend; the mid/fine scales fetch their
// own. The XY weight is the side planes' complement rather than blend.z, so
// under [triplanarSideGate] the gradient collapses to the plain XY gradient
// (weight one, the pre-triplanar behavior) instead of a blend.z-weakened
// one. Sub-gate terrain: 3 taps per scale, what the XY-only version cost; a
// wall opens 2-3 planes (6-9 taps) within the same distance gates, which
// used to skip walls entirely.
highp vec3 triplanarHeightGrad(Triplanar t, highp vec3 pos, float scale,
                               vec4 weights, highp float eps) {
  float w_y = t.blend.y * t.fade_y;
  float w_x = t.blend.x * t.fade_x;
  highp vec3 g = vec3(0.);
  g.xy =
      (1.0 - w_x - w_y) * planeHeightGrad(pos.xy, scale, weights,
                                          computeHeight(t.d_xy, weights), eps);
  if (w_y > 0.0)
    g.xz += w_y * planeHeightGrad(pos.xz, scale, weights,
                                  computeHeight(t.d_xz, weights), eps);
  if (w_x > 0.0)
    g.yz += w_x * planeHeightGrad(pos.yz, scale, weights,
                                  computeHeight(t.d_yz, weights), eps);
  return g;
}

// Procedural normal perturbation based on detail noise. [tri] is the macro
// material-blend sample from main, reused as finite-difference centers.
vec3 perturbNormal(vec3 geomNormal, Triplanar tri, float roughness,
                   vec4 detailWeights, float dataStep) {
  // Determine roughness
  const vec4 roughnessWeights =
      vec4(0.6 /* rock */, 0.9 /* grass */, 0.85 /* forest */, 0.2 /* ice */);
  float targetRoughness = dot(detailWeights, roughnessWeights);
  float material_roughness = mix(roughness, targetRoughness, 0.8);

  // Water/Ice gloss fix
  if (detailWeights.a > 0.1) {
    material_roughness = min(material_roughness, 0.3);
  }

  float bumpStrength = 4.0 * material_roughness;

  // Distance fades and masks for the three noise scales (skipping fetches
  // where a scale contributes nothing)
  float fade_fine = 1.0 - smoothstep(1000., 2000., v_dist); // 10m noise
  float fade_mid = 1.0 - smoothstep(4000., 8000., v_dist);  // 70m noise
  float mask_macro = smoothstep(175., 280., v_dist);        // 500m noise
  float mask_mid = smoothstep(25., 40., v_dist);

  // The fine scale bows out on the steepest faces: they are the most
  // expensive fragments (2-3 planes at every scale once past
  // [triplanarSideGate]), and the 10 m noise contributes least to how a wall
  // reads -- its character is macro and mid structure. Starts where the side
  // gate ends, so the fine bump is never simultaneously triplanar.
  fade_fine *= 1.0 - smoothstep(0.55, 0.7, 1.0 - geomNormal.z);

  // Compute projection basis once (before the gated fetches: no dFdx inside
  // non-uniform control flow)
  highp vec3 dPdx = dFdx(v_world_pos);
  highp vec3 dPdy = dFdy(v_world_pos);
  highp vec3 r1 = cross(dPdy, geomNormal);
  highp vec3 r2 = cross(geomNormal, dPdx);
  highp float det = dot(dPdx, r1);
  float signDet = (det > 0.0) ? 1.0 : -1.0;
  highp float invDet = signDet / (abs(det) + 1e-12);

  // Finite-difference step: the 3D pixel footprint, so the gradients have
  // the same frequency response as the dFdx they replace, floored per scale
  // at one mip-0 texel (gen_textures.sh bakes 1024x1024) so 8-bit
  // quantization noise is not amplified by a tiny divisor.
  highp float fp3 = max(length(dPdx), length(dPdy));

  // What a derivative bump paints at a pixel has the wavelength of the
  // pixel's own footprint, so whether it is detail the relief data cannot
  // hold (keep it) or a duplicate of the band the data owns (it would bury
  // the real thing) is a per-pixel ratio: data samples per pixel, along the
  // *better-resolved* screen axis -- at grazing view the long axis stretches
  // over many samples while the transverse one is what actually shows
  // detail. The pitch is per metre of horizontal grid, so it stretches by
  // 1/n_z on a steep face (multiplied through to keep every term small in
  // mediump). Below a quarter-sample per pixel the bump is untouched; from
  // one sample per pixel on it keeps a tenth, enough texture to read as
  // material rather than relief.
  highp float fpMin = min(length(dPdx), length(dPdy));
  bumpStrength *=
      1.0 -
      0.9 * smoothstep(0.25, 1.0, fpMin * max(geomNormal.z, 0.125) / dataStep);

  // All three scales take a triplanar world-space gradient, gated by their
  // distance fades: steep faces get texture in proper proportion at every
  // scale (an XY-only gradient smears down the fall line there, and a slope
  // fade instead of it stripped cliffs down to the bare relief normals --
  // both tried and rejected). The macro centers are [tri]'s, already paid
  // for by the material blend; mid and fine fetch their own. Below
  // [triplanarSideGate] all of it collapses to the XY plane: 3 taps per
  // scale, what the XY-only version cost.
  float sideGate = triplanarSideGate(geomNormal);
  highp vec3 grad1 = vec3(0.);
  if (mask_macro > 0.0)
    grad1 = triplanarHeightGrad(tri, v_world_pos, 0.002, detailWeights,
                                max(fp3, 1. / (0.002 * 1024.)));
  highp vec3 grad2 = vec3(0.);
  if (fade_mid > 0.0)
    grad2 = triplanarHeightGrad(
        sampleTriplanar(v_world_pos, geomNormal, 0.014, sideGate), v_world_pos,
        0.014, detailWeights, max(fp3, 1. / (0.014 * 1024.)));
  highp vec3 grad3 = vec3(0.);
  if (fade_fine > 0.0)
    grad3 = triplanarHeightGrad(
        sampleTriplanar(v_world_pos, geomNormal, 0.1, sideGate), v_world_pos,
        0.1, detailWeights, max(fp3, 1. / (0.1 * 1024.)));

  // Per-scale: scalar height derivatives along the screen axes via the chain
  // rule, yielding the same screen-space quantity dFdx used to measure --
  // the masks and divisors below apply unchanged. dFdx itself was retired at
  // every scale: differentiating the mip-filtered sample gave per-quad,
  // per-mip-texel-plateau values, whose grazing amplification rendered as
  // shading rectangles on walls at the macro scale.
  highp float dHdx1 = dot(grad1, dPdx);
  highp float dHdy1 = dot(grad1, dPdy);
  highp float dHdx2 = dot(grad2, dPdx);
  highp float dHdy2 = dot(grad2, dPdy);
  highp float dHdx3 = dot(grad3, dPdx);
  highp float dHdy3 = dot(grad3, dPdy);

  // Combine in scalar space, then project once
  highp float combinedDHdx = dHdx1 * mask_macro +
                             dHdx2 * mask_mid * fade_mid / 8. +
                             dHdx3 * fade_fine / 60.;
  highp float combinedDHdy = dHdy1 * mask_macro +
                             dHdy2 * mask_mid * fade_mid / 8. +
                             dHdy3 * fade_fine / 60.;

  return normalize(geomNormal - invDet *
                                    (combinedDHdx * r1 + combinedDHdy * r2) *
                                    bumpStrength);
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
  // Define fog color early for use in water reflection.
  // Blend toward the zenith colour above the eye line exactly as sky.frag
  // does (-view.z is the sky's cos_theta), so fogged ridges meet the sky
  // without a tonal step. At or below the eye line the smoothstep is 0 and
  // this is exactly u_fogColor.
  highp vec3 view_dir_n = normalize(v_view_dir);
  vec3 fog_color =
      mix(u_fogColor, u_zenithColor, smoothstep(0.0, 0.35, -view_dir_n.z));

  // Decode normal from the relief normal texture, preferring the
  // high-resolution one inside its extent (quad-coherent branch: neighbouring
  // fragments are on the same side of a boundary kilometres away, and both
  // sources derive from the same heights there).
  mediump float f0 = ringFade(hd_valid[0], hdReliefCoord0);
  mediump float f1 = ringFade(hd_valid[1], hdReliefCoord1);
  mediump float f2 = ringFade(hd_valid[2], hdReliefCoord2);
  mediump vec2 encodedN;
  mediump float data_step;
  if (f0 >= 1.0) {
    encodedN = texture(hd_relief_normal[0], hdReliefCoord0).rg;
    data_step = hd_step[0];
  } else if (f1 >= 1.0 && f0 <= 0.0) {
    encodedN = texture(hd_relief_normal[1], hdReliefCoord1).rg;
    data_step = hd_step[1];
  } else if (f2 >= 1.0 && f1 <= 0.0 && f0 <= 0.0) {
    encodedN = texture(hd_relief_normal[2], hdReliefCoord2).rg;
    data_step = hd_step[2];
  } else {
    // A ring's border band (rare: the bands are ~6% of each extent), or
    // outside every ring. Composite outside-in so each ring fades over the
    // one beneath it; the fetch count is bounded by the fades that are
    // actually open, and the branches stay quad-coherent away from the
    // bands' own edges (the same acceptance as the selection above).
    encodedN = texture(relief_normal, reliefCoord).rg;
    // The base gets the same sentinel [Render_state] uploads for empty
    // slots: far above any footprint, i.e. the full bump, exactly as the
    // base always had -- and mediump-safe, unlike an infinity.
    data_step = 60000.0;
    if (f2 > 0.0) {
      encodedN =
          mix(encodedN, texture(hd_relief_normal[2], hdReliefCoord2).rg, f2);
      data_step = mix(data_step, hd_step[2], f2);
    }
    if (f1 > 0.0) {
      encodedN =
          mix(encodedN, texture(hd_relief_normal[1], hdReliefCoord1).rg, f1);
      data_step = mix(data_step, hd_step[1], f1);
    }
    if (f0 > 0.0) {
      encodedN =
          mix(encodedN, texture(hd_relief_normal[0], hdReliefCoord0).rg, f0);
      data_step = mix(data_step, hd_step[0], f0);
    }
  }
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
  Surface surface = sampleCLCBilinear(reliefCoord);

  applySlopeModification(surface, slope);

  // Sample packed detail map via triplanar projection
  highp float scale = 0.002; // ~500m per texture repeat
  Triplanar tri =
      sampleTriplanar(v_world_pos, normal, scale, triplanarSideGate(normal));
  vec4 texNoise = triplanarCombine(tri);

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
    float forestNoise = texNoise.b;

    // Volume: Deep Shadow (Low) vs Sunlit Canopy (High)
    vec3 deepShadow = baseAlbedo * vec3(0.2, 0.25, 0.3); // Very dark/cool
    vec3 sunlitTop = baseAlbedo * vec3(1.2, 1.25, 1.0);

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
  final_normal =
      perturbNormal(normal, tri, surface.roughness, finalWeights, data_step);

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

      // Wave 1: 0 degrees, Freq 1.0
      float w1 = sin(waveCoord.x);
      // Wave 2: 120 degrees, Freq 1.1 (Detuned)
      highp float input2 = (waveCoord.x * -0.5 + waveCoord.y * 0.866) * 1.1;
      float w2 = sin(input2);
      // Wave 3: 240 degrees, Freq 1.2 (Detuned)
      highp float input3 = (waveCoord.x * -0.5 - waveCoord.y * 0.866) * 1.2;
      float w3 = sin(input3);

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
  float mirror = (iceAmount > 0.1 || waterMask > 0.1) ? 0.4 : 0.0;
  bool is_mirror = mirror > 0.0;

  // === Specular & Environment Reflection ===
  vec3 halfVec = normalize(lightDir + view_dir_n);

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
  float NdotV = max(dot(final_normal, view_dir_n), 1e-3);
  float roughSq = material_roughness * material_roughness;
  float denom = NdotH * NdotH * (roughSq - 1.0) + 1.0;
  float D = roughSq / (3.14159 * denom * denom + 0.0001);
  float final_l = max(0.0, dot(final_normal, lightDir));
  float specular = D * final_l;

  // === Shadow Calculation ===
  // Both consumers of shadow_val (specular below, direct light further down)
  // are multiplied by final_l, so the 5-tap PCF is pure waste on unlit
  // fragments. The branch is quad-coherent (whole slopes face away).
  float shadow_val = 1.0;
  if (final_l > 0.0) {
    int cascade = 2;
    if (v_dist < shadow_splits[0])
      cascade = 0;
    else if (v_dist < shadow_splits[1])
      cascade = 1;

    float slopeScale = 1.0 - cosTheta;
    // One shadow texel in world units per cascade (ortho width / 2048).
    float texelSz = (cascade == 0) ? 3.0 : ((cascade == 1) ? 12.0 : 37.0);
    float normalOffset = texelSz * slopeScale;
    // World coordinates, and everything derived from them down to the depth
    // comparison, must stay highp: at fp16 [offset_pos] alone is quantised to
    // 2 m at 3 km and 16 m at 20 km, and [proj_coords.z] to 10/23/73 m of
    // world depth per cascade -- far more than the bias below is sized for.
    highp vec3 offset_pos =
        v_world_pos + normal * normalOffset + vec3(0., 0., center_height);

    highp vec4 s_pos = shadow_matrices[cascade] * vec4(offset_pos, 1.0);
    highp vec3 proj_coords = s_pos.xyz / s_pos.w;
    proj_coords = proj_coords * 0.5 + 0.5;

    highp float current_depth = proj_coords.z;
    // World-space depth bias, expressed in this cascade's [0,1] depth range
    // (20/48/150 km): a small constant for sun-facing surfaces plus a
    // slope-proportional term covering the depth a shadow texel
    // (2.93/11.7/36.6 m) spans on surfaces tilted toward the light's
    // direction. Too little and the texel rows band such slopes (acne); a
    // large constant (the previous 30/72/225 m) detaches every shadow from
    // its caster instead.
    highp float texelWorld =
        (cascade == 0) ? 2.93 : ((cascade == 1) ? 11.7 : 36.6);
    highp float depthSpan =
        (cascade == 0) ? 20000.0 : ((cascade == 1) ? 48000.0 : 150000.0);
    highp float slopeTan = min(4.0, sqrt(max(0.0, 1.0 - cosTheta * cosTheta)) /
                                        max(cosTheta, 0.1));
    highp float bias = (4.0 + 1.5 * texelWorld * slopeTan) / depthSpan;
    shadow_val = pcf_shadow(cascade, proj_coords.xy, current_depth - bias,
                            vec2(0.000488));
    if (proj_coords.z > 1.0)
      shadow_val = 1.0;
  }

  // Environment reflection (sky color for glossy surfaces)
  vec3 reflectDir = reflect(-view_dir_n, final_normal);
  float skyReflect = max(0.0, reflectDir.z);
  // Updated to match new sky: Horizon(u_fogColor) -> Zenith(u_zenithColor)
  // Sky Shader uses smoothstep(0.0, 0.35, cos_theta)
  // We use the exact same mixing factor for consistency
  float sky_mix = smoothstep(0.0, 0.35, skyReflect);
  vec3 envColor = mix(u_fogColor, u_zenithColor, sky_mix);

  // Specular and reflection are incident light: they are added after the
  // diffuse lighting multiply at the end, not folded into the albedo.
  float specBoost = (waterMask > 0.01 || iceAmount > 0.5) ? 2.5 : 1.0;
  vec3 specColor = vec3(1.0, 0.98, 0.95) * specular *
                   (1.0 - material_roughness) * shadow_val * specBoost;

  // Fresnel for water
  if (waterMask > 0.01) {
    // Schlick's approximation
    float fresnel = 0.02 + 0.98 * pow(1.0 - NdotV, 5.0);
    mirror = mix(mirror, fresnel, waterMask);
    // Removed explicit fog mixing here; let distance fog handle it
    // envColor = mix(envColor, fog_color, waterMask);
  }

  // Reflection strength (remove damping for water); applied at the end
  float reflectDamp = (waterMask > 0.01) ? 1.0 : 0.3;

  // === Lighting ===

  // Matched to Sky Shader: Horizon -> Zenith
  vec3 sky_color = u_fogColor * 0.8 + u_zenithColor * 0.2;
  vec3 ground_color =
      vec3(0.08, 0.07, 0.05); // Deeper ground bounce for contrast
  float sky_factor = final_normal.z * 0.6 + 0.4; // Weighted more towards sky
  // The two used to arrive in equal measure, which cost the picture the whole
  // of its relief: sunlit and shaded ground came out within a factor of two of
  // each other, every slope carried the blue of the sky more strongly than the
  // colour of its own material, and a band of ridges spanned a fifth of the
  // display range -- little enough to disappear entirely under sunlight falling
  // on the screen. See AMBIENT_LIGHT against the direct term below.
  vec3 ambient = mix(ground_color, sky_color, sky_factor) * AMBIENT_LIGHT;

  // Rectified Geometric Normal for Micro-AO comparison
  // Use flat normal (0,0,1) for water to avoid dark artifacts at shorelines
  vec3 geoNormal = mix(normal, vec3(0.0, 0.0, 1.0), waterMask);

  // Normal-based Micro-AO: Darken steep crevices
  // Compare final normal (with detail) against the underlying geometric shape
  float normalAO = mix(0.85, 1.0, max(0.0, dot(final_normal, geoNormal)));

  // Noise-based Micro-AO: Darken crevasses/gaps (low noise values)
  // 1. Compute per-material AO factors (Low noise = Darker)
  float rockAO = smoothstep(0.0, 0.6, texNoise.r);
  float forestAO = smoothstep(0.0, 0.5, texNoise.b);
  float iceAO = smoothstep(0.0, 0.6, texNoise.a);

  // 2. Combine based on weights (Grass/Green channel gets 1.0 = no darkening)
  float combinedNoiseAO = rockAO * finalWeights.r + 1.0 * finalWeights.g +
                          forestAO * finalWeights.b + iceAO * finalWeights.a;

  // 3. Remap to valid range (0.5 to 1.0) so it doesn't get too dark
  float noiseAO = mix(0.5, 1.0, combinedNoiseAO);

  // Combine both AO terms
  float finalMicroAO = normalAO * noiseAO;
  ambient *= finalMicroAO;

  vec3 sun_color = vec3(1.0, 0.95, 0.9);
  vec3 direct = sun_color * final_l * shadow_val * DIRECT_LIGHT;
  // Baked AO measures sky visibility: it attenuates the ambient term and the
  // sky reflection, not the direct sun, which the shadow map already handles
  // (multiplying the whole color darkened sunlit couloirs twice).
  float occlusion = texture(ao, reliefCoord).r;
  vec3 lighting = (ambient * occlusion) + direct;

  // Diffuse lighting scales the albedo; reflected light is added on top.
  // Applying the reflection before the lighting multiply darkened the sky
  // reflection by the diffuse term, turning grazing lakes muddy, and applied
  // the (already shadowed) specular through the shadowed direct term twice.
  vec3 lit = lighting * terrain_color;
  lit = mix(lit, envColor * occlusion, mirror * reflectDamp);
  // What land returns of the sky: a few percent face-on, rising towards
  // grazing, and added on top of the material rather than in place of it.
  // Scaled by AMBIENT_LIGHT because it is the same sky the ambient term carries
  // and has to stay in proportion to it, and by smoothness because a rough
  // surface scatters it too wide to show.
  if (!is_mirror) {
    float sheen = 0.04 + 0.6 * pow(1.0 - NdotV, 5.0);
    lit += envColor * occlusion * AMBIENT_LIGHT * sheen *
           (1.0 - material_roughness);
  }
  lit += specColor * 0.4;

  // === Fog & Haze ===
  // Height-based haze, exp(-h/H) with H~1667m, taken at the mean altitude of
  // the camera-to-fragment path rather than the fragment's own: with the
  // fragment altitude alone, a low valley seen from a summit was over-hazed
  // by the dense air it does not actually lie under.
  float haze_density = exp((-center_height - (0.5 * v_world_pos.z)) * 0.0006);
  float fog_coeff = exp(v_dist * -0.4e-4 * haze_density);

  vec3 final_color = mix(fog_color, lit, fog_coeff);

  // Tone curve and gamma correction, then a +/-0.5/255 dither in output space
  // to break up banding
  highp float noise = IGN(gl_FragCoord.xy);
  color = vec4(
      pow(tone_map(final_color), vec3(1.0 / 2.2)) + (noise - 0.5) / 255.0, 1.0);
}
