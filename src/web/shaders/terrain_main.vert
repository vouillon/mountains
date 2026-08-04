
// Half a texel of the high-resolution grid: the corner convention above ->
// the texture-centre convention of the filtered fetch in terrain.frag, exactly
// as [inv_w] does for the base grid.
uniform highp float hd_half_texel[HD_SLOTS];

out highp float v_dist;
out highp vec2 reliefCoord;
out highp vec2 hdReliefCoord0;
out highp vec2 hdReliefCoord1;
out highp vec2 hdReliefCoord2;
out highp vec3 v_world_pos;
out highp vec3 v_view_dir;

void main() {
  RadialVertex rv = computeRadialVertex();

  reliefCoord = rv.norm_coord + (0.5 * inv_w);
  hdReliefCoord0 = rv.hd_coord[0] + hd_half_texel[0];
  hdReliefCoord1 = rv.hd_coord[1] + hd_half_texel[1];
  hdReliefCoord2 = rv.hd_coord[2] + hd_half_texel[2];
  v_world_pos = vec3(rv.coord_meters, rv.height - center_height);

  highp vec4 pos = transform * vec4(rv.pos_plane, rv.height, 1.0);
  v_dist = length(vec3(center_offset, 0) - v_world_pos);
  v_view_dir = normalize(vec3(center_offset, 0) - v_world_pos);
  gl_Position = proj * pos;
}
