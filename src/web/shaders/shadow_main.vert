
void main() {
  RadialVertex rv = computeRadialVertex();
  gl_Position = shadow_view_proj * vec4(rv.coord_meters, rv.height, 1.0);
}
