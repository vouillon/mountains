// Bakes the EEA tree cover (Viewer.tree_cover_texture, Tree_cover) into the
// canopy atlas's third region, RGBA8 per ~20 m sample, rows from the south:
// - R: tree cover density, 0-1;
// - G: share of conifers: 1 coniferous, 0 broadleaved, 0.5 neither;
// - B: 1 where the density has data, 0 elsewhere (outside the EEA's
//   coverage, and where the service answers stray values above 100).

// R8, rows north-up as served, [u_width] samples across of [u_size] rows
// (Tree_cover.t's [width]: the service's samples are square in degrees): the
// density (percent) from the left, the leaf type (1 broadleaved, 2
// coniferous) [u_stride] texels on.
uniform highp sampler2D u_input;
uniform int u_size;
uniform int u_width;
uniform int u_stride;
uniform ivec2 u_offset; // the target slot's first texel (Viewer.canopy_atlas)

out vec4 color;

void main() {
  ivec2 p = ivec2(gl_FragCoord.xy) - u_offset;
  // The nearest of the [u_width] samples across.
  ivec2 q = ivec2((p.x * u_width + u_width / 2) / u_size, u_size - 1 - p.y);
  float d = floor(texelFetch(u_input, q, 0).r * 255.0 + 0.5);
  float l =
      floor(texelFetch(u_input, q + ivec2(u_stride, 0), 0).r * 255.0 + 0.5);
  bool valid = d <= 100.0;
  color = vec4(valid ? d / 100.0 : 0.0, l == 2.0 ? 1.0 : (l == 1.0 ? 0.0 : 0.5),
               valid ? 1.0 : 0.0, 0.0);
}
