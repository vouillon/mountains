// Trees near the eye (tree.vert): lit by the sun and the sky like the
// terrain (terrain.frag's DIRECT_LIGHT, AMBIENT_LIGHT), darker towards the
// base of the crown, with the terrain's haze and tone curve. The sun is
// shadowed by the relief (the shadow map's first cascade) and by the trees
// (its layer 3, Viewer.tree_shadow_box); against the sun, light comes through
// the edges of the crowns.

// The foliage pattern's hashes need 32-bit ints (mediump ints are 16-bit on
// Mali).
precision highp int;

uniform vec3 u_lightDir;
uniform vec3 u_fogColor;
uniform vec3 u_zenithColor;
uniform highp float u_eyeHeight;
uniform bool u_broadleaf;
uniform highp sampler2DArrayShadow shadow_map;
uniform highp mat4 u_shadowMatrix; // the first cascade's
// That cascade's world metres per shadow texel and the depth its [0,1] spans
// (terrain.frag's [shadow_cascades]).
uniform highp vec2 u_shadowCascade;
uniform highp mat4 u_treeShadowMatrix; // layer 3's, 4 km deep

in vec3 v_normal;
in highp float v_dist;
in highp float v_z;
in float v_up;
in highp vec3 v_world;
in vec3 v_fromEye;
flat in float v_tint;
flat in float v_bark;
in highp vec3 v_local;
flat in highp float v_seed;
flat in vec2 v_size;

out mediump vec4 color;

// 5-tap PCF of shadow layer [layer] at [p] seen through [m], [bias] metres
// over a depth span of [span]; lit outside the layer.
float shadowAt(int layer, highp mat4 m, highp vec3 p, highp float bias,
               highp float span) {
  highp vec4 s = m * vec4(p, 1.0);
  highp vec3 c = s.xyz / s.w * 0.5 + 0.5;
  if (c.z > 1.0 || any(lessThan(c.xy, vec2(0.0))) ||
      any(greaterThan(c.xy, vec2(1.0))))
    return 1.0;
  highp float compare = c.z - bias / span;
  highp float t = 0.000488;
  float l = float(layer);
  float v = texture(shadow_map, vec4(c.xy, l, compare));
  v += texture(shadow_map, vec4(c.xy + vec2(t, 0.0), l, compare));
  v += texture(shadow_map, vec4(c.xy - vec2(t, 0.0), l, compare));
  v += texture(shadow_map, vec4(c.xy + vec2(0.0, t), l, compare));
  v += texture(shadow_map, vec4(c.xy - vec2(0.0, t), l, compare));
  return v / 5.0;
}

// The relief's shadow, away from the surface by a coarse shadow texel
// (~3 m): towards the sun and out along the normal.
float reliefShadow(vec3 n) {
  highp float texel = u_shadowCascade.x;
  return shadowAt(0, u_shadowMatrix,
                  v_world + u_lightDir * (1.5 * texel) + n * (0.5 * texel), 2.0,
                  u_shadowCascade.y);
}

// The trees' shadow, this crown's own included: lifted off the surface by a
// texel (~0.5 m) along the normal.
float treeShadow(vec3 n) {
  return shadowAt(3, u_treeShadowMatrix, v_world + n * 0.6, 0.3, 4000.0);
}

// ========== Foliage pattern ==========
// Per tree (seeded), in the tree's own frame, so nothing repeats or swims.

highp uint hash3(highp ivec3 c) {
  highp uvec3 v = uvec3(c) * 1664525u + 1013904223u;
  v.x += v.y * v.z;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  v ^= v >> 16u;
  v.x += v.y * v.z;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  return v.x ^ v.y ^ v.z;
}

float rnd3(highp ivec3 c) {
  return float(hash3(c + ivec3(int(v_seed))) & 0xffffu) / 65535.0;
}

float valueNoise3(highp vec3 p) {
  highp vec3 i = floor(p);
  vec3 f = vec3(p - i);
  highp ivec3 c = ivec3(i);
  vec3 u = f * f * (3.0 - 2.0 * f);
  return mix(
      mix(mix(rnd3(c), rnd3(c + ivec3(1, 0, 0)), u.x),
          mix(rnd3(c + ivec3(0, 1, 0)), rnd3(c + ivec3(1, 1, 0)), u.x), u.y),
      mix(mix(rnd3(c + ivec3(0, 0, 1)), rnd3(c + ivec3(1, 0, 1)), u.x),
          mix(rnd3(c + ivec3(0, 1, 1)), rnd3(c + ivec3(1, 1, 1)), u.x), u.y),
      u.z);
}

// Leaf clumps: the nearest of one jittered point per unit cell among the 8
// cells around [p] (jitter kept to the cell's middle, so 8 are enough);
// distance in x, the direction from that point to [p] in yzw.
vec4 clumps(highp vec3 p) {
  highp vec3 b = floor(p - 0.5);
  float best = 10.0;
  vec3 dir = vec3(0.0, 0.0, 1.0);
  for (int k = 0; k < 8; k++) {
    highp vec3 c = b + vec3(float(k & 1), float((k >> 1) & 1), float(k >> 2));
    highp ivec3 ic = ivec3(c);
    highp vec3 q = c + 0.25 +
                   0.5 * vec3(rnd3(ic), rnd3(ic + ivec3(17, 0, 0)),
                              rnd3(ic + ivec3(0, 29, 0)));
    vec3 d = vec3(p - q);
    float l = dot(d, d);
    if (l < best) {
      best = l;
      dir = d;
    }
  }
  return vec4(sqrt(best), normalize(dir));
}

// The foliage's tone (scaling the albedo) at this point, tilting [n] to
// match. Faded out as the pattern drops below a few pixels of [fp] metres.
float foliage(inout vec3 n, highp float fp) {
  // Fine detail: needle or leaf clumps of ~0.25 m.
  float fine = 1.0 - smoothstep(0.06, 0.15, fp);
  float tone = 1.0;
  if (fine > 0.0) {
    // Two octaves of tone, and a tilt of the normal over ~0.5 m: each clump
    // of needles or leaves catches the light its own way.
    float finer = 1.0 - smoothstep(0.025, 0.06, fp);
    tone =
        mix(1.0,
            (0.6 + 0.8 * valueNoise3(v_local * 4.0)) *
                mix(1.0, 0.75 + 0.5 * valueNoise3(v_local * 9.0 + 5.0), finer),
            fine);
    highp vec3 q = v_local * 2.0;
    vec3 tilt = vec3(valueNoise3(q + 41.0), valueNoise3(q + 53.0),
                     valueNoise3(q + 67.0)) -
                0.5;
    n = normalize(n + tilt * (1.2 * fine));
  }
  if (!u_broadleaf) {
    // Branch whorls: drooping layers, lit on top, dark in the gap beneath
    // the next one up, staggered between 7-10 sectors around the trunk.
    float period = max(0.45, v_size.x / 22.0);
    float fade = 1.0 - smoothstep(0.2, 0.45, fp / period);
    if (fade > 0.0) {
      float sectors = 7.0 + floor(4.0 * rnd3(ivec3(3, 5, 7)));
      highp float a = atan(v_local.y, v_local.x) * (1.0 / 6.2831853) + 0.5;
      highp float sector = floor(a * sectors);
      float phase = rnd3(ivec3(int(sector), 11, 13));
      // Branch tips droop: the layer bends down away from the trunk.
      highp float rho = length(v_local.xy);
      // Whorls neither level nor evenly spaced: a noise over ~0.7 m
      // shifts them by up to half a period.
      float f = fract(float(v_local.z / period) + phase + 0.5 * rho / period +
                      0.5 * valueNoise3(v_local * 1.4 + 31.0));
      float layer = smoothstep(0.0, 0.35, f);
      tone *= mix(1.0, mix(0.55, 1.08, layer), fade);
      // Top faces of the branches face up, their undersides down.
      n = normalize(n + vec3(0.0, 0.0, (layer - 0.45) * 0.9 * fade));
    }
  } else {
    // Leaf clumps of ~1.6 m: soft domes, darker between, their sizes and
    // depths varied by a noise over ~3 m.
    float size = 1.6;
    float fade = 1.0 - smoothstep(0.15, 0.35, fp / size);
    if (fade > 0.0) {
      highp vec3 q = v_local / size;
      vec4 c = clumps(q + 0.35 * (valueNoise3(q * 0.5 + 7.0) - 0.5));
      float depth = 0.5 + 0.5 * valueNoise3(q * 0.5 + 19.0);
      float dome = 1.0 - smoothstep(0.1, 0.85, c.x);
      tone *= mix(1.0, mix(1.0 - 0.4 * depth, 1.06, dome), fade);
      n = normalize(n + c.yzw * (0.45 * depth * fade));
    }
  }
  return tone;
}

void main() {
  vec3 n = normalize(v_normal);
  // The pixel's footprint, outside any branch (bark is per triangle).
  highp float fp = max(length(dFdx(v_local)), length(dFdy(v_local)));
  float tone = v_bark > 0.5 ? 1.0 : foliage(n, fp);
  // Linear palette albedos of broadleaved (311) and coniferous (312) forest,
  // and bark.
  vec3 albedo = v_bark > 0.5  ? vec3(0.05, 0.035, 0.025)
                : u_broadleaf ? vec3(0.0170, 0.1009, 0.0127) * v_tint
                              : vec3(0.0127, 0.0676, 0.0170) * v_tint;
  albedo *= tone;
  vec3 sky = u_fogColor * 0.8 + u_zenithColor * 0.2;
  vec3 ambient = mix(vec3(0.08, 0.07, 0.05), sky, n.z * 0.6 + 0.4) * 0.15;
  float within = mix(0.35, 1.0, v_up);
  float lit_side = max(dot(n, u_lightDir), 0.0);
  float relief = reliefShadow(n);
  float sun = lit_side > 0.0 && relief > 0.0 ? relief * treeShadow(n) : 0.0;
  vec3 sunColor = vec3(1.0, 0.95, 0.9);
  vec3 direct = sunColor * lit_side * sun * mix(0.6, 1.0, v_up);
  // Against the sun, the edges of a crown glow: light through the foliage,
  // strongest towards the sun's direction and where the crown is seen edge on
  // (thin there), yellower than the reflected green. Not through bark, and
  // only if the relief lets the sun reach the tree.
  vec3 view = normalize(v_fromEye);
  float against = max(dot(view, u_lightDir), 0.0);
  float edge = 1.0 - abs(dot(n, view));
  float against2 = against * against;
  float through =
      v_bark > 0.5 ? 0.0
                   : against2 * against2 * (0.15 + 0.85 * edge * edge) * relief;
  // A third of the direct light at most, and patchy with the foliage's tone
  // (in [albedo]): light comes through the gaps of a crown, not a lamp shade.
  vec3 transmitted =
      albedo * tone * vec3(1.05, 1.05, 0.6) * sunColor * through * 0.35;
  vec3 lit = albedo * (ambient * within + direct) + transmitted;
  float haze_density = exp((-u_eyeHeight - 0.5 * v_z) * 0.0006);
  float fog = exp(v_dist * -0.4e-4 * haze_density);
  vec3 final_color = mix(u_fogColor, lit, fog);
  color = vec4(pow(tone_map(final_color), vec3(1.0 / 2.2)), 1.0);
}
