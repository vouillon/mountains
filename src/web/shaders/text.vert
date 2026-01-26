#version 300 es
uniform mat4 transform;
out vec2 texture_coord;
void main() {
  float x = float(gl_VertexID & 1);
  float y = float(gl_VertexID >> 1);
  texture_coord = vec2(x, 1. - y);
  gl_Position = transform * vec4(x, y, 0, 1.);
}
