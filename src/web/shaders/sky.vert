#version 300 es
out mediump vec2 v_uv;
void main() {
  float x = float(gl_VertexID & 1);
  float y = float(gl_VertexID >> 1);
  v_uv = vec2(x, y);
  // Draw at Far Plane (Z=1.0)
  gl_Position = vec4(2.0 * x - 1.0, 2.0 * y - 1.0, 1.0, 1.0);
}
