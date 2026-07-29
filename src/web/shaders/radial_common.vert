
// Radial grid uniforms (shared between terrain and shadow shaders)
uniform highp int w_mask;
uniform highp int w_shift;
uniform highp vec2 center_offset;
uniform highp float snapped_alpha;
uniform highp float inv_sectors_div;
uniform highp float grid_k;
uniform highp float grid_scale;
uniform highp vec2 inv_delta;
uniform highp float inv_w;
uniform highp float inv_avg_delta;
uniform highp int max_lod;
uniform highp sampler2D relief;

// Output structure for radial vertex computation
struct RadialVertex {
  highp vec2 pos_plane;    // Position relative to camera in meters
  highp vec2 coord_meters; // Absolute world position in meters
  highp vec2 norm_coord;   // Normalized texture coordinate (0..1)
  highp float height;      // Terrain height at this position
};

// Compute radial grid vertex position and sample terrain height
RadialVertex computeRadialVertex() {
  const float PI = 3.14159265359;
  RadialVertex v;

  int sector = gl_VertexID & w_mask;
  int ring = gl_VertexID >> w_shift;
  float theta = (float(sector) * inv_sectors_div) * (PI / 2.0) - (PI / 4.0);
  float angle = theta + snapped_alpha + (PI / 2.0);

  // Exponential radial distance: r = A(e^(k*ring) - 1)
  highp float r = grid_scale * (exp(grid_k * float(ring)) - 1.0);
  v.pos_plane = vec2(cos(angle), sin(angle)) * r;
  v.coord_meters = center_offset + v.pos_plane;
  highp vec2 coord = v.coord_meters * inv_delta;

  // Grid spacing for LOD: dr = k(r + A)
  highp float grid_spacing = grid_k * (r + grid_scale);

  // LOD level based on grid spacing
  highp float lod_f = max(0.0, log2(grid_spacing * inv_avg_delta));
  int lod = min(int(lod_f), max_lod);

  // Texture size at this LOD
  ivec2 tex_size = textureSize(relief, lod);

  // Normalized coordinate (no flip: row 0 is south, see [Dem_loader.load])
  v.norm_coord = vec2(coord.x, coord.y) * inv_w + 0.5;

  // Texel position for manual bilinear interpolation
  highp vec2 lod_pos = v.norm_coord * vec2(tex_size);
  highp vec2 lod_tex_pos = clamp(lod_pos, vec2(0.0), vec2(tex_size - 1));
  highp ivec2 base = ivec2(lod_tex_pos);
  highp vec2 f = fract(lod_tex_pos);

  // Fetch 4 samples
  highp vec2 s00 = texelFetch(relief, base, lod).rg;
  highp vec2 s10 =
      texelFetch(relief, min(base + ivec2(1, 0), tex_size - 1), lod).rg;
  highp vec2 s01 =
      texelFetch(relief, min(base + ivec2(0, 1), tex_size - 1), lod).rg;
  highp vec2 s11 =
      texelFetch(relief, min(base + ivec2(1, 1), tex_size - 1), lod).rg;

  // Decode heights: high*256 + low, scaled to [-500, 9000]
  const highp float HEIGHT_SCALE = (1.0 / 257.0) * 9500.0;
  highp vec4 H = vec4(dot(s00, vec2(1.0, 256.0)), dot(s10, vec2(1.0, 256.0)),
                      dot(s01, vec2(1.0, 256.0)), dot(s11, vec2(1.0, 256.0))) *
                     HEIGHT_SCALE -
                 500.0;

  v.height = mix(mix(H.x, H.y, f.x), mix(H.z, H.w, f.x), f.y);

  return v;
}
