precision mediump float; // Default mediump for mobile performance
precision highp sampler2DArray;

// Encoded normal only (RG8). The heights live in a separate RG8 texture that
// the vertex stage declares as `relief`; this stage never reads them.
uniform highp sampler2D relief_normal;
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
  vec2 dist_from_center = abs(uv - 0.5);
  float d_center = max(dist_from_center.x, dist_from_center.y);
  // Avoid log2(0)
  d_center = max(d_center, 0.000001);

  // Formula: Level > 7 + log2(d)   (Assumes u_numLevels=7)
  float level_pos = ceil(float(u_numLevels) + log2(d_center));

  int level = clamp(int(level_pos), 0, u_numLevels - 1);

  // Calculate texture coordinates for this concentric level
  // Level 6 covers the whole tile (scale 1x)
  // Level 0 covers 1/64th (scale 64x)
  float scale = exp2(float((u_numLevels - 1) - level));
  vec2 levelUV = (uv - 0.5) * scale + 0.5;

  vec2 texSize = vec2(textureSize(u_coverMap, 0).xy);
  highp vec2 texelPos = levelUV * texSize - 0.5;

  ivec2 p00_raw = ivec2(floor(texelPos));
  vec2 frac = fract(texelPos);

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

// Triplanar sampling for packed RGBA detail map
// Returns blended detail weights from all projection planes
vec4 sampleTriplanarCombined(highp vec3 worldPos, vec3 normal, float scale) {
  highp vec2 uv_xz = worldPos.xz * scale;
  highp vec2 uv_xy = worldPos.xy * scale;
  highp vec2 uv_yz = worldPos.yz * scale;

  vec3 blend = abs(normal);
  blend /= (blend.x + blend.y + blend.z + 0.0001);

  // Sample packed RGBA detail map from each projection plane. The side
  // projections contribute nothing on near-horizontal terrain: fade them in
  // with their blend weight (quad-coherent, like the distance gates in
  // perturbNormal) and default to the neutral 0.5.
  float bias = 1.;
  vec4 d_xy = texture(u_detailMap, uv_xy, bias);
  vec4 d_xz = vec4(0.5);
  float fade_y = smoothstep(0.003, 0.01, blend.y);
  if (fade_y > 0.0)
    d_xz = mix(vec4(0.5), texture(u_detailMap, uv_xz, bias), fade_y);
  vec4 d_yz = vec4(0.5);
  float fade_x = smoothstep(0.003, 0.01, blend.x);
  if (fade_x > 0.0)
    d_yz = mix(vec4(0.5), texture(u_detailMap, uv_yz, bias), fade_x);

  // Blend based on surface orientation (Corrected for Z-up)
  // Top/Bottom (blend.z) -> XY projection
  // Side-Y (blend.y)     -> XZ projection
  // Side-X (blend.x)     -> YZ projection
  return d_xy * blend.z + d_xz * blend.y + d_yz * blend.x;
}

float computeHeight(vec4 noise, vec4 weights) {
  const vec4 heightWeight =
      vec4(3. /* rock */, 0.3 /* grass */, 5. /* forest */, 3. /* ice */);
  return dot((noise - 0.5) * heightWeight, weights);
}

// Procedural normal perturbation based on detail noise
// Uses screen-space derivatives for directional bump mapping
vec3 perturbNormal(vec3 geomNormal, vec4 texNoise, float roughness,
                   vec4 detailWeights) {
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

  // Distance fade-outs for detail noise (invisible at range, saves texture
  // fetches)
  float fade_fine = 1.0 - smoothstep(1000., 2000., v_dist); // 10m noise
  float fade_mid = 1.0 - smoothstep(4000., 8000., v_dist);  // 70m noise

  // Only sample when contribution is visible at this distance
  vec4 texNoise2 = vec4(0.5);
  vec4 texNoise3 = vec4(0.5);
  if (fade_mid > 0.0)
    texNoise2 = texture(u_detailMap, v_world_pos.xy * 0.014, 1.);
  if (fade_fine > 0.0)
    texNoise3 = texture(u_detailMap, v_world_pos.xy * 0.1, 1.);

  // Compute projection basis once
  highp vec3 dPdx = dFdx(v_world_pos);
  highp vec3 dPdy = dFdy(v_world_pos);
  highp vec3 r1 = cross(dPdy, geomNormal);
  highp vec3 r2 = cross(geomNormal, dPdx);
  highp float det = dot(dPdx, r1);
  float signDet = (det > 0.0) ? 1.0 : -1.0;
  highp float invDet = signDet / (abs(det) + 1e-12);

  // Per-scale: compute scalar height derivatives
  highp float dHdx1 = dFdx(computeHeight(texNoise, detailWeights));
  highp float dHdy1 = dFdy(computeHeight(texNoise, detailWeights));
  highp float dHdx2 = dFdx(computeHeight(texNoise2, detailWeights));
  highp float dHdy2 = dFdy(computeHeight(texNoise2, detailWeights));
  highp float dHdx3 = dFdx(computeHeight(texNoise3, detailWeights));
  highp float dHdy3 = dFdy(computeHeight(texNoise3, detailWeights));

  // Distance-based attenuation masks
  float mask_macro = smoothstep(175., 280., v_dist);
  float mask_mid = smoothstep(25., 40., v_dist);

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

vec3 applyWaterEffects(vec3 baseColor, float waterMask, vec2 worldPos) {
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

float pcf_shadow(int layer, vec2 coords, float compare, vec2 texel_size) {
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

void main() {
  // Define fog color early for use in water reflection.
  // Blend toward the zenith colour above the eye line exactly as sky.frag
  // does (-view.z is the sky's cos_theta), so fogged ridges meet the sky
  // without a tonal step. At or below the eye line the smoothstep is 0 and
  // this is exactly u_fogColor.
  highp vec3 view_dir_n = normalize(v_view_dir);
  vec3 fog_color =
      mix(u_fogColor, u_zenithColor, smoothstep(0.0, 0.35, -view_dir_n.z));

  // Decode normal from the relief normal texture
  mediump vec2 encodedN = texture(relief_normal, reliefCoord).rg;
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
  vec4 texNoise = sampleTriplanarCombined(v_world_pos, normal, scale);

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
    vec2 macroPos = v_world_pos.xy * 0.02;
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
      perturbNormal(normal, texNoise, surface.roughness, finalWeights);

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
  float reflectivity = (1.0 - material_roughness) * 0.6; // Smooth = reflective

  // Ice and water get extra reflection
  float iceAmount = surface.detailWeights.a;
  if (iceAmount > 0.1 || waterMask > 0.1) {
    reflectivity = max(reflectivity, 0.4);
  }

  // === Specular & Environment Reflection ===
  vec3 halfVec = normalize(lightDir + view_dir_n);

  // GGX-inspired specular (simplified)
  float NdotH = max(0.0, dot(final_normal, halfVec));
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
    vec3 offset_pos =
        v_world_pos + normal * normalOffset + vec3(0., 0., center_height);

    vec4 s_pos = shadow_matrices[cascade] * vec4(offset_pos, 1.0);
    vec3 proj_coords = s_pos.xyz / s_pos.w;
    proj_coords = proj_coords * 0.5 + 0.5;

    float current_depth = proj_coords.z;
    // World-space depth bias, expressed in this cascade's [0,1] depth range
    // (20/48/150 km): a small constant for sun-facing surfaces plus a
    // slope-proportional term covering the depth a shadow texel
    // (2.93/11.7/36.6 m) spans on surfaces tilted toward the light's
    // direction. Too little and the texel rows band such slopes (acne); a
    // large constant (the previous 30/72/225 m) detaches every shadow from
    // its caster instead.
    float texelWorld = (cascade == 0) ? 2.93 : ((cascade == 1) ? 11.7 : 36.6);
    float depthSpan =
        (cascade == 0) ? 20000.0 : ((cascade == 1) ? 48000.0 : 150000.0);
    float slopeTan = min(4.0, sqrt(max(0.0, 1.0 - cosTheta * cosTheta)) /
                                  max(cosTheta, 0.1));
    float bias = (4.0 + 1.5 * texelWorld * slopeTan) / depthSpan;
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
    float n_dot_v = max(0.0, dot(final_normal, view_dir_n));
    // Schlick's approximation
    float fresnel = 0.02 + 0.98 * pow(1.0 - n_dot_v, 5.0);
    reflectivity = mix(reflectivity, fresnel, waterMask);
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
  vec3 ambient = mix(ground_color, sky_color, sky_factor) * 0.5;

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
  vec3 direct = sun_color * final_l * shadow_val * 0.5;
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
  lit = mix(lit, envColor * occlusion, reflectivity * reflectDamp);
  lit += specColor * 0.4;

  // === Fog & Haze ===
  // Height-based haze, exp(-h/H) with H~1667m, taken at the mean altitude of
  // the camera-to-fragment path rather than the fragment's own: with the
  // fragment altitude alone, a low valley seen from a summit was over-hazed
  // by the dense air it does not actually lie under.
  float haze_density = exp((-center_height - (0.5 * v_world_pos.z)) * 0.0006);
  float fog_coeff = exp(v_dist * -0.4e-4 * haze_density);

  vec3 final_color = mix(fog_color, lit, fog_coeff);

  // Gamma correction, then a +/-0.5/255 dither in output space to break up
  // banding
  highp float noise = IGN(gl_FragCoord.xy);
  color = vec4(pow(final_color, vec3(1.0 / 2.2)) + (noise - 0.5) / 255.0, 1.0);
}
