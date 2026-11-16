
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

// The far field: a second pyramid over a square twice the base grid's side,
// on a grid four times coarser (compress_dem --coarse) and the base's
// quantisation. A base-grid coordinate [c], in arcseconds from the anchor, is
// at (c + far_map.xy) * far_map.z of it. It has the ground beyond the base
// grid, which fades into it over its outer band ([baseWeight]); without it
// ([far_valid] false: its tiles did not come) the ground sinks out of sight
// there instead.
uniform bool far_valid;
uniform highp sampler2D far_relief;
uniform highp vec3 far_map;
uniform highp int far_max_lod;
// log2 of the far grid's spacing over the base's.
const highp float FAR_LOD_BIAS = -2.0;

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
#define HD_SLOTS 3
uniform bool hd_valid[HD_SLOTS];
uniform highp sampler2D hd_relief[HD_SLOTS];
// Arcseconds from the anchor to a position normalized over the ring. A ring on
// a projected grid has axes turned from north by its CRS's grid convergence, so
// this is a 2x2 rather than one scale per axis; the translation is separate
// because GLSL ES 3.0 has no mat3x2.
uniform highp mat2 hd_mat[HD_SLOTS];
uniform highp vec2 hd_bias[HD_SLOTS];
// And curving: over a block of kilometres a projected grid is not an affine
// image of the arcsecond one -- a second of longitude shortens with latitude,
// the grid convergence changes across the block -- and the best affine map
// misplaces a 5 km ring's corners by a metre. Each output's coefficients of
// (u^2, u v, v^2), from [Hd_dem.frame]'s fitted [Warp]; zero on the graticule.
uniform highp vec3 hd_quad_x[HD_SLOTS];
uniform highp vec3 hd_quad_y[HD_SLOTS];
uniform highp float hd_lod_bias[HD_SLOTS]; // log2 of refinement over the base
uniform highp int hd_max_lod[HD_SLOTS];
uniform highp float hd_height_scale_n[HD_SLOTS];
uniform highp float hd_height_offset[HD_SLOTS];

// The base pyramid's quantisation: 9500 m over 65536 steps, the .dem
// pipeline's.
const highp float BASE_SCALE_N = (1.0 / 257.0) * 9500.0;
const highp float BASE_OFFSET = -500.0;

// Output structure for radial vertex computation
struct RadialVertex {
  highp vec2 pos_plane;          // Position relative to camera in meters
  highp vec2 coord_meters;       // Absolute world position in meters
  highp vec2 norm_coord;         // Normalized texture coordinate (0..1)
  highp vec2 hd_coord[HD_SLOTS]; // Same, over each refinement ring
  highp vec2 far_coord;          // Same, over the far field
  highp float height;            // Terrain height at this position
};

// Bilinear fetch over a height pyramid, in the corner convention (the level-l
// texel j sits at level-0 index j << l) that [rendered_height] replicates on
// the CPU. Shared by the base and high-resolution paths so that they cannot
// drift apart.
// [scale_n] is metres per normalised unit (the pyramid's metres-per-u16-step
// times 255) and [offset] the metres at zero: each refinement pyramid carries
// its own, the base pyramid the pipeline's.
highp float sampleReliefHeight(highp sampler2D tex, highp vec2 norm_coord,
                               int lod, highp float scale_n,
                               highp float offset) {
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

  // Decode heights: high*256 + low, through this pyramid's own quantisation.
  highp vec4 H = vec4(dot(s00, vec2(1.0, 256.0)), dot(s10, vec2(1.0, 256.0)),
                      dot(s01, vec2(1.0, 256.0)), dot(s11, vec2(1.0, 256.0))) *
                     scale_n +
                 offset;

  return mix(mix(H.x, H.y, f.x), mix(H.z, H.w, f.x), f.y);
}

// The base grid's share of the surface at its normalised coordinate [c]: all
// of it well inside, none from its edge outwards, where the far field has the
// ground. The two grids are not aligned -- the far one is absolute, the base
// one stands on its anchor -- so they are faded into each other, heights here
// and normals in terrain.frag, which must use the same band.
highp float baseWeight(highp vec2 c) {
  highp float d = min(min(c.x, 1.0 - c.x), min(c.y, 1.0 - c.y));
  return smoothstep(0.0, 0.03, d);
}

// Ring [i]'s normalized coordinate of a base-grid coordinate [c], in arcseconds
// from the anchor.
highp vec2 hdCoord(int i, highp vec2 c) {
  highp vec3 q = vec3(c.x * c.x, c.x * c.y, c.y * c.y);
  return hd_mat[i] * c + hd_bias[i] +
         vec2(dot(hd_quad_x[i], q), dot(hd_quad_y[i], q));
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
  highp vec2 c0 = hdCoord(0, coord);
  if (insideHd(0, c0))
    return sampleReliefHeight(
        hd_relief[0], c0,
        min(int(max(0.0, lod_raw + hd_lod_bias[0])), hd_max_lod[0]),
        hd_height_scale_n[0], hd_height_offset[0]);
  highp vec2 c1 = hdCoord(1, coord);
  if (insideHd(1, c1))
    return sampleReliefHeight(
        hd_relief[1], c1,
        min(int(max(0.0, lod_raw + hd_lod_bias[1])), hd_max_lod[1]),
        hd_height_scale_n[1], hd_height_offset[1]);
  highp vec2 c2 = hdCoord(2, coord);
  if (insideHd(2, c2))
    return sampleReliefHeight(
        hd_relief[2], c2,
        min(int(max(0.0, lod_raw + hd_lod_bias[2])), hd_max_lod[2]),
        hd_height_scale_n[2], hd_height_offset[2]);
  highp float w = baseWeight(norm_coord);
  highp float h = BASE_OFFSET;
  if (w < 1.0 && far_valid)
    h = sampleReliefHeight(
        far_relief, (coord + far_map.xy) * far_map.z,
        min(int(max(0.0, lod_raw + FAR_LOD_BIAS)), far_max_lod), BASE_SCALE_N,
        BASE_OFFSET);
  if (w > 0.0)
    h = mix(h,
            sampleReliefHeight(relief, norm_coord,
                               min(int(max(0.0, lod_raw)), max_lod),
                               BASE_SCALE_N, BASE_OFFSET),
            w);
  return h;
}

// The LIDAR HD canopy over the 1 m ring's block (see terrain.frag and
// Viewer.canopy_texture): R is the height, 0.2 m per step; A is 0 for nodata.
// Shared by the terrain, shadow and path programs, so the forest the mesh
// stands up, the forest that casts shadows and the forest a GPX trace is
// draped over are one surface. Precisions explicit: terrain.frag declares the
// same uniforms, and they must match across stages.
uniform mediump sampler2D u_canopy;
uniform mediump int u_canopySlot;
// The far canopy (Hd_dem.canopy_far, 5 m over the 5 m ring's block), read
// through slot [u_canopyFarSlot], -1 when there is none. Both are in
// [u_canopy] (Viewer.canopy_atlas), with the EEA tree cover and the 20 m
// canopy that terrain.frag reads.
uniform mediump int u_canopyFarSlot;
// Whether tree.vert stands trees over the near canopy (Viewer.apply_canopy).
uniform bool u_treesStand;
// Metres per sample of the near and far canopies, as served (4 m rather than
// 5 for the far one over La Reunion: Hd_dem.served_metres).
uniform mediump vec2 u_canopyStep;

// Slot [slot] of the atlas (Viewer.canopy_atlas: three columns, two rows):
// 0 the near canopy, 1 the far one, 2 and 3 the EEA tree cover's two levels,
// 4 the 20 m canopy.
highp vec2 atlasUV(highp vec2 c, int slot) {
  return (vec2(float(slot % 3), float(slot / 3)) + clamp(c, 0.0, 1.0)) /
         vec2(3.0, 2.0);
}
highp vec2 canopyNearUV(highp vec2 c) { return atlasUV(c, 0); }
highp vec2 canopyFarUV(highp vec2 c) { return atlasUV(c, 1); }

// How far a canopy is trusted at its block coordinate [c]: not over the
// block's last 3% (~60 m near, ~150 m far), which also keeps the two halves
// of the atlas from bleeding into each other's reads.
float canopyEdge(highp vec2 c) {
  highp vec2 e = min(c, 1.0 - c);
  return smoothstep(0.0, 0.03, min(e.x, e.y));
}

// The share of the ground [r] m from the eye under tree.vert's trees: within
// ~900 m (thinning out over the last 120 m of Viewer.tree_radius) and inside
// the near block, whose edge [nearEdge] tree_find.vert thins them over too.
float treesStood(highp float r, float nearEdge) {
  return u_treesStand ? (1.0 - smoothstep(840.0, 960.0, r)) * nearEdge : 0.0;
}

// The share of the canopy the mesh is raised by [r] m from the eye: none
// where trees stand instead, and none around the eye itself, which stands on
// the ground -- with no trees there (the near canopy missing or still on its
// way, or trees turned off) the mesh would otherwise close over the viewer.
// The clearing is the trees' own (tree_find.vert); without the near canopy it
// is wider, the far one alone being a blanket that reads as hills from close
// by, and raising buildings, which only the near one's trees leave out.
float canopyRest(highp float r, float nearEdge) {
  float clearing = u_canopySlot >= 0 ? smoothstep(40.0, 80.0, r)
                                     : smoothstep(300.0, 600.0, r);
  return (1.0 - treesStood(r, nearEdge)) * clearing;
}

// The canopy mip level the mesh reads at [grid_spacing] m between rings, for
// a canopy of [step] m: two levels coarser than the spacing (~8 m averages
// near the eye), so the mesh carries a stand's shape and its crowns stay in
// the shading -- at the spacing itself every crown became a spike.
// terrain.frag shades with the slope of this same level.
float canopyLod(highp float grid_spacing, float step) {
  return log2(max(grid_spacing / step, 1.0)) + 2.0;
}

// Only stands are stood up: averaged at [canopyLod], a lone tree became a
// grassy mound three times its width with a green crown on top (the colour
// reads the canopy at full resolution). The averaged crown tone measures the
// cover -- about 0.5 in a closed canopy, 0.1-0.2 around a lone tree -- so
// lone trees and small groups stay flat, drawn by their crown shading alone.
// terrain.frag weighs the slope it shades with by the same factor.
float canopyCover(float tone) { return smoothstep(0.2, 0.4, tone); }

// The land cover (terrain.frag's, on the same units, hence the precisions):
// the canopy model has crowns over the water along a wooded shore, and what
// it has on open water is noise, neither of which may raise the water.
uniform mediump usampler2DArray u_coverMap;
uniform mediump sampler2D u_paletteTex;

// Whether the land cover is water at the base grid's normalised coordinate
// [c]: the nearest texel of the level terrain.frag's [sampleCLCBilinear]
// reads there.
bool overWater(highp vec2 c) {
  highp vec2 uv = (c + 0.5 * inv_w - 0.5) * 0.5 + 0.5;
  highp float d = max(max(abs(uv.x - 0.5), abs(uv.y - 0.5)), 0.000001);
  int level = clamp(int(ceil(8.0 + log2(d))), 0, 7);
  highp vec2 luv = (uv - 0.5) * exp2(float(7 - level)) + 0.5;
  ivec2 size = textureSize(u_coverMap, 0).xy;
  ivec2 p = clamp(ivec2(floor(luv * vec2(size))), ivec2(0), size - 1);
  uint id = texelFetch(u_coverMap, ivec3(p, level), 0).r;
  return texelFetch(u_paletteTex, ivec2(int(id) * 3 + 1, 0), 0).a > 0.5;
}

// Whether there is water within a mesh interval, [s] m, of [c]: a vertex
// raised beside one on the water would draw the water between them up the
// side of the canopy.
bool nearWater(highp vec2 c, highp float s) {
  highp vec2 e = s * inv_delta * inv_w;
  for (int k = 0; k < 9; k++)
    if (overWater(c + e * vec2(float(k % 3 - 1), float(k / 3 - 1))))
      return true;
  return false;
}

// The lift from one canopy's sample [s]: 0 under 1.5 m of canopy, without
// data, and above 40 m (cliffs, where surface and ground models disagree).
highp float liftOf(vec4 s) {
  if (s.a < 0.5 / 255.0)
    return 0.0;
  float h = s.r * (255.0 / 5.0);
  return h * smoothstep(1.0, 3.0, h) * (1.0 - smoothstep(40.0, 48.0, h)) *
         canopyCover(s.a);
}

// Metres to raise the mesh by for the canopy, at the near and far rings'
// normalised coordinates [cn], [cf], for a vertex [r] m from the eye with
// [grid_spacing] m between rings: the near canopy inside its block, the far
// one beyond, read at [canopyLod] so far vertices take the canopy averaged
// into a smooth blanket rather than aliasing crowns. 0 where tree.vert stands
// trees instead, around the eye ([canopyRest]), and on the water or next to
// it, [c] being the base grid's normalised coordinate.
highp float canopyLift(highp vec2 c, highp vec2 cn, highp vec2 cf,
                       highp float r, highp float grid_spacing) {
  float nearEdge = u_canopySlot >= 0 ? canopyEdge(cn) : 0.0;
  float farEdge = u_canopyFarSlot >= 0 ? canopyEdge(cf) : 0.0;
  // Where the near canopy has no data (across a border, a tile that did not
  // come) it is not there: no trees stand on it (tree_find.vert), and the
  // far canopy has the ground, which would otherwise be bare.
  vec4 sn = vec4(0.0);
  if (nearEdge > 0.0) {
    sn = textureLod(u_canopy, canopyNearUV(cn),
                    canopyLod(grid_spacing, u_canopyStep.x));
    if (sn.a < 0.5 / 255.0)
      nearEdge = 0.0;
  }
  float rest = canopyRest(r, nearEdge);
  if (rest <= 0.0)
    return 0.0;
  highp float lift = nearEdge * liftOf(sn);
  if (nearEdge < 1.0 && farEdge > 0.0)
    lift += (1.0 - nearEdge) * farEdge *
            liftOf(textureLod(u_canopy, canopyFarUV(cf),
                              canopyLod(grid_spacing, u_canopyStep.y)));
  if (lift <= 0.0 ||
      nearWater(c, max(grid_spacing, r * inv_sectors_div * 1.5708)))
    return 0.0;
  return lift * rest;
}

// A ring slot's normalised coordinate.
highp vec2 slotCoord(highp vec2 c0, highp vec2 c1, highp vec2 c2, int slot) {
  return slot == 0 ? c0 : (slot == 1 ? c1 : c2);
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
  // was off by 660 m (0.42 deg of azimuth). At the 140 km the far field
  // reaches, computed on a sphere: at most 37 m (a third of a far sample, 55
  // arcseconds of azimuth), against 1900 m. Must match the inverse used for
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
  v.hd_coord[0] = hdCoord(0, coord);
  v.hd_coord[1] = hdCoord(1, coord);
  v.hd_coord[2] = hdCoord(2, coord);
  v.far_coord = (coord + far_map.xy) * far_map.z;
  v.height = sampleTerrainHeight(vec2(coord.x, coord.y), v.norm_coord, lod_raw);
  v.height += canopyLift(
      v.norm_coord,
      slotCoord(v.hd_coord[0], v.hd_coord[1], v.hd_coord[2], u_canopySlot),
      slotCoord(v.hd_coord[0], v.hd_coord[1], v.hd_coord[2], u_canopyFarSlot),
      r, grid_spacing);

  // Earth curvature with standard atmospheric refraction folded in
  // (effective radius R / (1 - 0.13) ~ 7320 km). The grid is centred on the
  // observer, so the drop is a pure function of the ring radius; without it
  // the skyline at the 70 km edge renders ~335 m too high. Must match
  // [Visibility.curvature_drop].
  v.height -= r * r * 6.8306e-8;

  return v;
}
