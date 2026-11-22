
uniform vec2 size;
uniform vec2 delta;
// Takes the gradient this shader measures along the grid's own axes to one
// along east and north. Identity for a grid on the graticule; for one in a
// projected CRS its axes are turned by that CRS's grid convergence -- 2.44
// degrees at 6.36 E -- and without undoing it the ring's normals sit that far
// round from those of the surface beyond its edge, which the renderer's hard
// switch at the boundary shows as a seam.
uniform highp mat2 grad_rot;
uniform sampler2D tile;
// The grid's own quantisation (metres per u16 step); see [Hd_dem.blend].
// Height differences cancel the offset, so only the scale enters the slope.
uniform highp float height_scale;
// Single render target: heights already live in the height pyramid (uploaded
// directly at level 0 and written directly by downsample.frag at levels 1+),
// so normal.frag only writes the encoded normal (8 taps, half the writes).
layout(location = 0) out mediump vec2 normal_out;

// Raw u16/255 height at integer texel [p], clamped to this level's [size].
float get_raw(ivec2 p, ivec2 max_p) {
  return dot(texelFetch(tile, clamp(p, ivec2(0), max_p), 0).rg,
             vec2(1.0, 256.0));
}

void main() {
  ivec2 p = ivec2(gl_FragCoord.xy);
  ivec2 max_p = ivec2(size) - 1;

  // Sobel filter (8 surrounding taps; center is unused)
  float tl = get_raw(p + ivec2(-1, -1), max_p);
  float t = get_raw(p + ivec2(0, -1), max_p);
  float tr = get_raw(p + ivec2(1, -1), max_p);
  float l = get_raw(p + ivec2(-1, 0), max_p);
  float r = get_raw(p + ivec2(1, 0), max_p);
  float bl = get_raw(p + ivec2(-1, 1), max_p);
  float b = get_raw(p + ivec2(0, 1), max_p);
  float br = get_raw(p + ivec2(1, 1), max_p);

  float dX = tr + 2.0 * r + br - (tl + 2.0 * l + bl);
  float dY = bl + 2.0 * b + br - (tl + 2.0 * t + tr);

  // Normal vector: raw is u16/255, Sobel scales by 8, so multiply by
  // -(255/8) * height_scale / delta.
  vec2 slope =
      grad_rot * (vec2(dX, dY) * ((-255.0 / 8.0) * height_scale / delta));
  vec3 n = normalize(vec3(slope, 1.0));

  // Encode Normal (xy components to [0,1])
  normal_out = n.xy * 0.5 + 0.5;
}
