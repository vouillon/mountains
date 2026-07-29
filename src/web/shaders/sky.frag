#version 300 es
precision mediump float;
uniform highp mat4 inv_view;
uniform highp vec3 u_lightDir;
uniform vec3 u_fogColor;
uniform vec3 u_zenithColor;
uniform vec2 sky_params; // x_scale, y_scale
in highp vec2 v_uv;
out vec4 color;

void main() {
  // Reconstruct View Ray in View Space
  // Clip space: (x, y, -1.0) for forward direction (RH)
  // View Ray = (clip.x / x_scale, clip.y / y_scale, -1.0)

  highp float x = (v_uv.x * 2.0 - 1.0) / sky_params.x;
  highp float y = (v_uv.y * 2.0 - 1.0) / sky_params.y;
  highp vec3 view_ray = normalize(vec3(x, y, -1.0));

  // Transform to World Space (Rotation only)
  highp vec3 view_dir = mat3(inv_view) * view_ray;
  view_dir = normalize(view_dir);

  float cos_theta = view_dir.z;
  highp float cos_gamma = dot(view_dir, u_lightDir);

  // Deep blue zenith, lighter horizon (Linear Space)
  // New Lighter Blue/White Horizon (Matches fog)
  vec3 horizon = u_fogColor;

  float horizon_factor = smoothstep(0.0, 0.35, cos_theta);
  vec3 sky_base = mix(horizon, u_zenithColor, horizon_factor);

  float mie = pow(max(0.0, cos_gamma), 400.0) * 0.8;
  float halo = pow(max(0.0, cos_gamma), 20.0) * 0.2;

  vec3 sun_color = vec3(1.0, 0.9, 0.7);
  vec3 sky = sky_base + sun_color * (mie + halo);

  if (cos_gamma > 0.9995) {
    sky = vec3(1.0, 0.95, 0.8) * 20.0;
  }

  // Dither to prevent banding
  float noise =
      fract(sin(dot(gl_FragCoord.xy, vec2(12.9898, 78.233))) * 43758.5453);
  sky += (noise - 0.5) / 255.0;

  // Gamma Correction (Linear -> sRGB)
  color = vec4(pow(sky, vec3(1.0 / 2.2)), 1.0);
}
