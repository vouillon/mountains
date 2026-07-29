uniform sampler2D relief;
uniform int width;
uniform mediump float scale;
in vec2 uv;
out mediump float occlusion;

float IGN(vec2 p) {
  vec3 magic = vec3(0.06711056, 0.00583715, 52.9829189);
  return fract(magic.z * fract(dot(p, magic.xy)));
}

void main() {
  const mediump float DIRECTIONS = 8.0;
  const mediump float STEPS = 10.0;

  mediump float R_uv = 50.0 / float(width);
  float R_world = 50.0 * scale;

  float h_center = decode_height(texture(relief, uv).rg);

  float noise = IGN(gl_FragCoord.xy);
  mediump float totalOcclusion = 0.0;

  for (mediump float d = 0.0; d < DIRECTIONS; d++) {
    mediump float angle = (d + noise) * (2.0 * PI / DIRECTIONS);
    mediump vec2 dir = vec2(cos(angle), sin(angle));

    mediump float max_tan = -100.0;

    for (mediump float s = 1.0; s <= STEPS; s++) {
      float t_linear = (s + 1.0 + noise) / (STEPS + 2.0);
      float sample_t = t_linear * t_linear;
      vec2 sample_uv = uv + dir * sample_t * R_uv;

      float h_sample = decode_height(texture(relief, sample_uv).rg);
      float h_diff = h_sample - h_center;
      float dist = sample_t * R_world;

      float tan_s = h_diff / dist;
      max_tan = max(max_tan, tan_s);
    }

    mediump float sin_horizon = max_tan / sqrt(1.0 + max_tan * max_tan);
    totalOcclusion += max(0.0, sin_horizon);
  }

  occlusion = 1.0 - totalOcclusion / DIRECTIONS;
}
