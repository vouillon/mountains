#version 300 es
out vec2 uv;
void main() {
  float x = float(gl_VertexID & 1);
  float y = float(gl_VertexID >> 1);
  uv = vec2(x, y);
  gl_Position = vec4(2.0 * x - 1.0, 2.0 * y - 1.0, 0.0, 1.0);
}
