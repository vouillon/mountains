#version 300 es
uniform mat4 transform;
uniform vec4 color;
out vec4 fragment_color;
void main() {
  float x = float(gl_VertexID - 1) / 2.;
  float y = float(gl_VertexID != 1) * (sqrt(3.) / 2.);
  fragment_color = color;
  gl_Position = transform * vec4(x, y, 0, 1.);
}
