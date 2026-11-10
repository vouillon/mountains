// Trees near the eye (tree.vert): lit by the sun and the sky like the
// terrain (terrain.frag's DIRECT_LIGHT, AMBIENT_LIGHT), darker towards the
// base of the crown, with the terrain's haze and tone curve. The sun is
// shadowed by the relief (the shadow map's first cascade) and by the trees
// (its layer 3, Viewer.tree_shadow_box); against the sun, light comes through
// the edges of the crowns.

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

void main() {
  vec3 n = normalize(v_normal);
  // Linear palette albedos of broadleaved (311) and coniferous (312) forest,
  // and bark.
  vec3 albedo = v_bark > 0.5  ? vec3(0.05, 0.035, 0.025)
                : u_broadleaf ? vec3(0.0170, 0.1009, 0.0127) * v_tint
                              : vec3(0.0127, 0.0676, 0.0170) * v_tint;
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
  float through = v_bark > 0.5 ? 0.0
                               : against * against * against *
                                     (0.25 + 0.75 * edge * edge) * relief;
  vec3 transmitted = albedo * vec3(1.3, 1.25, 0.5) * sunColor * through * 0.9;
  vec3 lit = albedo * (ambient * within + direct) + transmitted;
  float haze_density = exp((-u_eyeHeight - 0.5 * v_z) * 0.0006);
  float fog = exp(v_dist * -0.4e-4 * haze_density);
  vec3 final_color = mix(u_fogColor, lit, fog);
  color = vec4(pow(tone_map(final_color), vec3(1.0 / 2.2)), 1.0);
}
