
// Half a texel of the high-resolution grid: the corner convention above ->
// the texture-centre convention of the filtered fetch in terrain.frag, exactly
// as [inv_w] does for the base grid.
uniform highp float hd_half_texel[HD_SLOTS];

// The still water's mirror. x: the height of its plane, from the eye; y: one
// over the tangent of the highest elevation baked; z: 1 while baking; w: 1
// when terrain.frag has a bake to read. See [Viewer.bake_reflection].
uniform highp vec4 u_refl;
// Where this rotation of the wedge stands across the bake, and how much of
// its width it takes: the bake is of an arc of the horizon, the one that has
// water ([Viewer.mirror]).
uniform highp vec2 u_reflWedge;
// What stands this little above the plane is under it to the bake: the
// water itself, which the mesh draws at the plane's level
// (radial_common.vert's [u_waterLevel]).
const highp float REFL_MARGIN = 0.05;

out highp float v_dist;
out highp vec2 reliefCoord;
out highp vec2 hdReliefCoord0;
out highp vec2 hdReliefCoord1;
out highp vec2 hdReliefCoord2;
out highp vec2 farReliefCoord;
out highp vec3 v_world_pos;
out highp vec3 v_view_dir;

void main() {
  RadialVertex rv = computeRadialVertex();

  reliefCoord = rv.norm_coord + (0.5 * inv_w);
  hdReliefCoord0 = rv.hd_coord[0] + hd_half_texel[0];
  hdReliefCoord1 = rv.hd_coord[1] + hd_half_texel[1];
  hdReliefCoord2 = rv.hd_coord[2] + hd_half_texel[2];
  // Half a texel again: a far sample is four base steps wide.
  farReliefCoord = rv.far_coord + 2.0 * far_map.z;
  v_world_pos = vec3(rv.coord_meters, rv.height - center_height);

  if (u_refl.z > 0.5) {
    // The bake: the ground as the eye's image in the mirror sees it -- which
    // is what the eye sees in the mirror -- over an arc of the horizon, a
    // quarter turn of it per rotation of the wedge. Across is the azimuth,
    // exact at every vertex, the mesh being radial; up is the tangent of the
    // elevation, which keeps straight what is straight along a radius. The
    // depth is spent on the clipping the mirror needs instead, of everything
    // under its plane: the far plane is that plane. What hides what is left to
    // the order of drawing, from the eye outwards (see
    // [Viewer.bake_reflection]).
    highp vec3 eye = vec3(center_offset, 2.0 * u_refl.x);
    v_dist = length(eye - v_world_pos);
    v_view_dir = normalize(eye - v_world_pos);
    highp float r = max(length(rv.pos_plane), 1e-3);
    highp float across = (u_reflWedge.x + float(gl_VertexID & w_mask) *
                                              inv_sectors_div * u_reflWedge.y) *
                             2.0 -
                         1.0;
    highp float up = v_world_pos.z - 2.0 * u_refl.x;
    highp float above = v_world_pos.z - u_refl.x - REFL_MARGIN;
    // above / r is under up / r, itself under 1 / u_refl.y in the picture:
    // the depth stays within range of everything shown.
    gl_Position = vec4(across * r, 2.0 * up * u_refl.y - r,
                       r - 1.9 * u_refl.y * above, r);
    return;
  }

  highp vec4 pos = transform * vec4(rv.pos_plane, rv.height, 1.0);
  v_dist = length(vec3(center_offset, 0) - v_world_pos);
  v_view_dir = normalize(vec3(center_offset, 0) - v_world_pos);
  gl_Position = proj * pos;
}
