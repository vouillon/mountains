// Bakes the EEA tree cover (Viewer.tree_cover_texture, Tree_cover) into layers
// 2 and 3 of the canopy texture array, RGBA8 per ~20 m sample, rows from the
// south:
// - R: tree cover density, 0-1;
// - G: 1 coniferous, 0 broadleaved or unknown -- premultiplied by A, so that
//   the mipmaps average the share over the samples that know it;
// - B: 1 where the density has data, 0 elsewhere (outside the EEA's
//   coverage, and where the service answers stray values above 100);
// - A: 1 where the leaf type is known (broadleaved or coniferous), 0 where it
//   has no tree or no data. An unknown sample once read as a share of 0.5,
//   which pulled the conifers above Viewer's [conifer_line] -- the sparse
//   stands whose trees the 10 m leaf type misses -- halfway to broadleaf.

// R8, rows north-up as served, [u_width] samples across of [u_size] rows
// (Tree_cover.t's [width]: the service's samples are square in degrees): the
// density (percent) from the left, the leaf type (1 broadleaved, 2
// coniferous) [u_stride] texels on.
// The column below is a product of two indices, over a million: beyond the
// 16 bits of a mediump int (Mali's).
precision highp int;

uniform highp sampler2D u_input;
uniform int u_size;
uniform int u_width;
uniform int u_stride;

out vec4 color;

void main() {
  ivec2 p = ivec2(gl_FragCoord.xy);
  // The nearest of the [u_width] samples across.
  ivec2 q = ivec2((p.x * u_width + u_width / 2) / u_size, u_size - 1 - p.y);
  float d = floor(texelFetch(u_input, q, 0).r * 255.0 + 0.5);
  float l =
      floor(texelFetch(u_input, q + ivec2(u_stride, 0), 0).r * 255.0 + 0.5);
  bool valid = d <= 100.0;
  bool known = l == 1.0 || l == 2.0;
  color = vec4(valid ? d / 100.0 : 0.0, l == 2.0 ? 1.0 : 0.0, valid ? 1.0 : 0.0,
               known ? 1.0 : 0.0);
}
