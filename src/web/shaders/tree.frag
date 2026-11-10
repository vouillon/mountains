// Trees near the eye (tree.vert): lit by the sun and the sky like the
// terrain (terrain.frag's DIRECT_LIGHT, AMBIENT_LIGHT), darker towards the
// base of the crown, shadowed from the sun's shadow map's first cascade
// (where all of them are), with the terrain's haze and tone curve.

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

in vec3 v_normal;
in highp float v_dist;
in highp float v_z;
in float v_up;
in highp vec3 v_world;
flat in float v_tint;
flat in float v_bark;

out mediump vec4 color;

float sunVisible(vec3 n) {
  // Away from the crown's own surface in the map, which is a shadow texel
  // (~3 m) coarse: towards the sun and out along the normal.
  highp vec3 p = v_world + u_lightDir * (1.5 * u_shadowCascade.x) +
                 n * (0.5 * u_shadowCascade.x);
  highp vec4 s = u_shadowMatrix * vec4(p, 1.0);
  highp vec3 c = s.xyz / s.w * 0.5 + 0.5;
  if (c.z > 1.0)
    return 1.0;
  highp float compare = c.z - 2.0 / u_shadowCascade.y;
  highp float t = 0.000488;
  float v = texture(shadow_map, vec4(c.xy, 0.0, compare));
  v += texture(shadow_map, vec4(c.xy + vec2(t, 0.0), 0.0, compare));
  v += texture(shadow_map, vec4(c.xy - vec2(t, 0.0), 0.0, compare));
  v += texture(shadow_map, vec4(c.xy + vec2(0.0, t), 0.0, compare));
  v += texture(shadow_map, vec4(c.xy - vec2(0.0, t), 0.0, compare));
  return v / 5.0;
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
  float sun = lit_side > 0.0 ? sunVisible(n) : 0.0;
  vec3 direct = vec3(1.0, 0.95, 0.9) * lit_side * sun * mix(0.6, 1.0, v_up);
  vec3 lit = albedo * (ambient * within + direct);
  float haze_density = exp((-u_eyeHeight - 0.5 * v_z) * 0.0006);
  float fog = exp(v_dist * -0.4e-4 * haze_density);
  vec3 final_color = mix(u_fogColor, lit, fog);
  color = vec4(pow(tone_map(final_color), vec3(1.0 / 2.2)), 1.0);
}
