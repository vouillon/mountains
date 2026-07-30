#version 300 es
precision mediump float;
uniform highp mat4 inv_view;
uniform highp vec3 u_lightDir;
uniform vec3 u_fogColor;
uniform vec3 u_zenithColor;
uniform vec2 sky_params; // x_scale, y_scale
in highp vec2 v_uv;
out vec4 color;

// Interleaved gradient noise. Must be highp: at mediump (fp16 on mobile) the
// gl_FragCoord products lose the low bits and the hash collapses to a handful
// of values.
highp float IGN(highp vec2 p) {
  highp vec3 magic = vec3(0.06711056, 0.00583715, 52.9829189);
  return fract(magic.z * fract(dot(p, magic.xy)));
}

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

  // Gated: pow(0.0, y) is 0.0 for y > 0, so this is bit-identical while
  // skipping two pows over the (large) half of the sky away from the sun.
  float mie = 0.0;
  float halo = 0.0;
  if (cos_gamma > 0.0) {
    mie = pow(cos_gamma, 400.0) * 0.8;
    halo = pow(cos_gamma, 20.0) * 0.2;
  }

  vec3 sun_color = vec3(1.0, 0.9, 0.7);
  vec3 sky = sky_base + sun_color * (mie + halo);

  // Sun disc, ~0.53 degrees across with a soft limb (the real angular size;
  // the previous hard cutoff at 0.9995 made a jagged disc 3.6 degrees wide).
  // The thresholds must be highp: mediump cannot represent values this close
  // to 1 (fp16 ulp near 1 is ~5e-4).
  const highp float disc_outer = 0.9999870; // cos(0.29 deg)
  const highp float disc_inner = 0.9999910; // cos(0.24 deg)
  sky = mix(sky, vec3(1.0, 0.95, 0.8) * 20.0,
            smoothstep(disc_outer, disc_inner, cos_gamma));

  // Gamma Correction (Linear -> sRGB), then a +/-0.5/255 dither to prevent
  // banding. The dither must come after the pow: in linear space one output LSB
  // is worth 2-5x more noise in the dark blue zenith and ~2x less near the sun.
  highp float noise = IGN(gl_FragCoord.xy);
  color = vec4(pow(sky, vec3(1.0 / 2.2)) + (noise - 0.5) / 255.0, 1.0);
}
