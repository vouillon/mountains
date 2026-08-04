
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

// Near-field refinement rings (IGN RGE ALTI over WMTS, LIDAR HD over WMS;
// see [Hd_dem]). Extra height pyramids over squares that are neither centred on
// the anchor arcsecond nor power-of-two refinements of the base grid: each
// one's placement is [hd_bias] and its sampling exp2(hd_lod_bias) times finer,
// both derived from the actual sample spacing. [Hd_dem.blend] has already faded
// each ring back into the surface beneath it at the edge of its square, so they
// agree there and the selection below can be a hard switch. A ring with no data
// has [hd_valid] false and is skipped; with none valid this file behaves
// exactly as it did before the refinement layers existed. Slot 0 is the
// innermost ring. The scalars are arrays, which GLSL ES 3.0 lets us index
// dynamically; the sampler arrays it only lets us index by literal, hence the
// unrolled chain in [sampleTerrainHeight].
#define HD_SLOTS 2
uniform bool hd_valid[HD_SLOTS];
uniform highp sampler2D hd_relief[HD_SLOTS];
uniform highp float hd_scale[HD_SLOTS]; // 1 / extent of the ring, in arcseconds
uniform highp vec2 hd_bias[HD_SLOTS];   // normalized position of the anchor
uniform highp float hd_lod_bias[HD_SLOTS]; // log2 of refinement over the base
uniform highp int hd_max_lod[HD_SLOTS];

// Output structure for radial vertex computation
struct RadialVertex {
  highp vec2 pos_plane;          // Position relative to camera in meters
  highp vec2 coord_meters;       // Absolute world position in meters
  highp vec2 norm_coord;         // Normalized texture coordinate (0..1)
  highp vec2 hd_coord[HD_SLOTS]; // Same, over each refinement ring
  highp float height;            // Terrain height at this position
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

// True where ring [i] covers this coordinate and must be used in preference to
// anything coarser.
bool insideHd(int i, highp vec2 c) {
  return hd_valid[i] && all(greaterThanEqual(c, vec2(0.0))) &&
         all(lessThanEqual(c, vec2(1.0)));
}

// Height at a base-grid coordinate, taken from the innermost ring that covers
// it and falling back to the base pyramid. Shared by the terrain, shadow and
// path programs so the three cannot disagree about which surface is drawn --
// a GPX trace resolving its height differently from the mesh under it would
// float or sink.
highp float sampleTerrainHeight(highp vec2 coord, highp vec2 norm_coord,
                                highp float lod_raw) {
  // The bias is relative to the base spacing, and applies before the clamp to
  // level 0: near the camera the mesh is finer than the base grid, which is
  // precisely where the extra levels pay off.
  highp vec2 c0 = coord * hd_scale[0] + hd_bias[0];
  if (insideHd(0, c0))
    return sampleReliefHeight(
        hd_relief[0], c0,
        min(int(max(0.0, lod_raw + hd_lod_bias[0])), hd_max_lod[0]));
  highp vec2 c1 = coord * hd_scale[1] + hd_bias[1];
  if (insideHd(1, c1))
    return sampleReliefHeight(
        hd_relief[1], c1,
        min(int(max(0.0, lod_raw + hd_lod_bias[1])), hd_max_lod[1]));
  return sampleReliefHeight(relief, norm_coord,
                            min(int(max(0.0, lod_raw)), max_lod));
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
  v.hd_coord[0] = vec2(coord.x, coord.y) * hd_scale[0] + hd_bias[0];
  v.hd_coord[1] = vec2(coord.x, coord.y) * hd_scale[1] + hd_bias[1];
  v.height = sampleTerrainHeight(vec2(coord.x, coord.y), v.norm_coord, lod_raw);

  // Earth curvature with standard atmospheric refraction folded in
  // (effective radius R / (1 - 0.13) ~ 7320 km). The grid is centred on the
  // observer, so the drop is a pure function of the ring radius; without it
  // the skyline at the 70 km edge renders ~335 m too high. Must match
  // [Visibility.curvature_drop].
  v.height -= r * r * 6.8306e-8;

  return v;
}
