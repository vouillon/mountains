
// Radial grid uniforms (shared between terrain and shadow shaders)
uniform highp int w_mask;
uniform highp int w_shift;
uniform highp vec2 center_offset;
uniform highp float snapped_alpha;
uniform highp float inv_sectors_div;
uniform highp float grid_k;
uniform highp float grid_scale;
uniform highp vec2 inv_delta;
uniform highp float meridian_conv; // tan(center latitude) / earth radius
uniform highp float inv_w;
uniform highp float inv_avg_delta;
uniform highp int max_lod;
uniform highp sampler2D relief;

// Near-field high-resolution relief (IGN RGE ALTI, fetched per location; see
// [Hd_dem]). A second height pyramid over a square that is neither centred on
// the anchor arcsecond nor a power-of-two refinement of the base grid: its
// placement is [hd_bias] and its sampling exp2(hd_lod_bias) times finer, both
// derived from the actual sample spacing. [Hd_dem.blend] has already faded the
// high-resolution data back into the base at the edge of that square, so the
// two agree there and the selection below can be a hard switch. When no data is
// available [hd_valid] is false and this file behaves exactly as it did before
// the HD layer existed.
uniform bool hd_valid;
uniform highp sampler2D hd_relief;
uniform highp float hd_scale;    // 1 / extent of the HD square, in arcseconds
uniform highp vec2 hd_bias;      // normalized position of the anchor arcsecond
uniform highp float hd_lod_bias; // log2 of the refinement over the base grid
uniform highp int hd_max_lod;

// Output structure for radial vertex computation
struct RadialVertex {
  highp vec2 pos_plane;    // Position relative to camera in meters
  highp vec2 coord_meters; // Absolute world position in meters
  highp vec2 norm_coord;   // Normalized texture coordinate (0..1)
  highp vec2 hd_coord;     // Same, over the high-resolution square
  highp float height;      // Terrain height at this position
};

// Bilinear fetch over a height pyramid, in the corner convention (the level-l
// texel j sits at level-0 index j << l) that [rendered_height] replicates on
// the CPU. Shared by the base and high-resolution paths so that they cannot
// drift apart.
highp float sampleReliefHeight(highp sampler2D tex, highp vec2 norm_coord,
                               int lod) {
  ivec2 tex_size = textureSize(tex, lod);

  // Texel position for manual bilinear interpolation
  highp vec2 lod_pos = norm_coord * vec2(tex_size);
  highp vec2 lod_tex_pos = clamp(lod_pos, vec2(0.0), vec2(tex_size - 1));
  highp ivec2 base = ivec2(lod_tex_pos);
  highp vec2 f = fract(lod_tex_pos);

  // Fetch 4 samples
  highp vec2 s00 = texelFetch(tex, base, lod).rg;
  highp vec2 s10 =
      texelFetch(tex, min(base + ivec2(1, 0), tex_size - 1), lod).rg;
  highp vec2 s01 =
      texelFetch(tex, min(base + ivec2(0, 1), tex_size - 1), lod).rg;
  highp vec2 s11 =
      texelFetch(tex, min(base + ivec2(1, 1), tex_size - 1), lod).rg;

  // Decode heights: high*256 + low, scaled to [-500, 9000]
  const highp float HEIGHT_SCALE = (1.0 / 257.0) * 9500.0;
  highp vec4 H = vec4(dot(s00, vec2(1.0, 256.0)), dot(s10, vec2(1.0, 256.0)),
                      dot(s01, vec2(1.0, 256.0)), dot(s11, vec2(1.0, 256.0))) *
                     HEIGHT_SCALE -
                 500.0;

  return mix(mix(H.x, H.y, f.x), mix(H.z, H.w, f.x), f.y);
}

// True where the high-resolution pyramid must be used instead of the base one.
bool insideHd(highp vec2 hd_coord) {
  return hd_valid && all(greaterThanEqual(hd_coord, vec2(0.0))) &&
         all(lessThanEqual(hd_coord, vec2(1.0)));
}

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

  // Observer-centred azimuthal frame -> DEM lat/lon grid, to second order:
  // meridians converge (the east-west scale shrinks with northing) and
  // parallels curve relative to great circles. Validated against exact
  // geodesics: residual <= 6 m at the 89 km corner where the linear mapping
  // was off by 660 m (0.42 deg of azimuth). Must match the inverse used for
  // POI placement in viewer.ml.
  highp vec2 proj =
      vec2(v.pos_plane.x * (1.0 + v.pos_plane.y * meridian_conv),
           v.pos_plane.y - 0.5 * v.pos_plane.x * v.pos_plane.x * meridian_conv);
  highp vec2 coord = (center_offset + proj) * inv_delta;

  // Grid spacing for LOD: dr = k(r + A)
  highp float grid_spacing = grid_k * (r + grid_scale);

  // LOD level based on grid spacing
  highp float lod_raw = log2(grid_spacing * inv_avg_delta);

  // Normalized coordinate (no flip: row 0 is south, see [Dem_loader.load])
  v.norm_coord = vec2(coord.x, coord.y) * inv_w + 0.5;
  v.hd_coord = vec2(coord.x, coord.y) * hd_scale + hd_bias;

  if (insideHd(v.hd_coord)) {
    // The bias is relative to the base spacing, and applies before the clamp
    // to level 0: near the camera the mesh is finer than the base grid, which
    // is precisely where the extra levels pay off.
    v.height = sampleReliefHeight(
        hd_relief, v.hd_coord,
        min(int(max(0.0, lod_raw + hd_lod_bias)), hd_max_lod));
  } else {
    v.height = sampleReliefHeight(relief, v.norm_coord,
                                  min(int(max(0.0, lod_raw)), max_lod));
  }

  // Earth curvature with standard atmospheric refraction folded in
  // (effective radius R / (1 - 0.13) ~ 7320 km). The grid is centred on the
  // observer, so the drop is a pure function of the ring radius; without it
  // the skyline at the 70 km edge renders ~335 m too high. Must match
  // [Visibility.curvature_drop].
  v.height -= r * r * 6.8306e-8;

  return v;
}
