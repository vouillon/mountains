// Bakes the EEA tree cover (Viewer.tree_cover_texture, Tree_cover) into the
// canopy atlas's third region, RGBA8 per ~20 m sample, rows from the south:
// - R: tree cover density, 0-1;
// - G: share of conifers: 1 coniferous, 0 broadleaved, 0.5 neither;
// - B: 1 where the density has data, 0 elsewhere (outside the EEA's
//   coverage, and where the service answers stray values above 100).

// R8, rows north-up as served: the density (percent) in the left half, the
// leaf type (1 broadleaved, 2 coniferous) in the right.
uniform highp sampler2D u_input;
uniform int u_size;
uniform int u_offset; // the target's first column

out vec4 color;

void main() {
  ivec2 p = ivec2(gl_FragCoord.xy) - ivec2(u_offset, 0);
  ivec2 q = ivec2(p.x, u_size - 1 - p.y);
  float d = floor(texelFetch(u_input, q, 0).r * 255.0 + 0.5);
  float l = floor(texelFetch(u_input, q + ivec2(u_size, 0), 0).r * 255.0 + 0.5);
  bool valid = d <= 100.0;
  color = vec4(valid ? d / 100.0 : 0.0, l == 2.0 ? 1.0 : (l == 1.0 ? 0.0 : 0.5),
               valid ? 1.0 : 0.0, 0.0);
}
