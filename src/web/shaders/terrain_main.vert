
out highp float v_dist;
out mediump float v_h;
out highp vec2 reliefCoord;
out highp vec3 v_world_pos;
out highp vec3 v_view_dir;

void main() {
  RadialVertex rv = computeRadialVertex();

  reliefCoord = rv.norm_coord + (0.5 * inv_w);
  v_world_pos = vec3(rv.coord_meters, rv.height - center_height);

  highp vec4 pos = transform * vec4(rv.pos_plane, rv.height, 1.0);
  v_dist = length(vec3(center_offset, 0) - v_world_pos);
  v_view_dir = normalize(vec3(center_offset, 0) - v_world_pos);
  v_h = rv.height;
  gl_Position = proj * pos;
}
