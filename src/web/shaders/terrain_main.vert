
// Pre-multiplied projection * view matrix (computed once per pass on CPU).
uniform highp mat4 view_proj;

out highp vec2 reliefCoord;
out highp vec2 hdReliefCoord0;
out highp vec2 hdReliefCoord1;
out highp vec2 hdReliefCoord2;
out highp vec2 hdReliefCoord3;
out highp vec3 v_world_pos;

void main() {
  RadialVertex rv = computeRadialVertex();

  reliefCoord = rv.norm_coord + (0.5 * inv_w);
  // Half a texel of each high-resolution grid (hd_bias4[i].z): corner
  // convention above -> texture-centre convention of the filtered fetch in
  // terrain.frag, exactly as [inv_w] does for the base grid.
  hdReliefCoord0 = rv.hd_coord[0] + hd_bias4[0].z;
  hdReliefCoord1 = rv.hd_coord[1] + hd_bias4[1].z;
  hdReliefCoord2 = rv.hd_coord[2] + hd_bias4[2].z;
  hdReliefCoord3 = rv.hd_coord[3] + hd_bias4[3].z;
  v_world_pos = vec3(rv.coord_meters, rv.height - center_height);
  gl_Position = view_proj * vec4(rv.pos_plane, rv.height, 1.0);
}
