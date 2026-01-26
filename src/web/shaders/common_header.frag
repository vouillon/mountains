#version 300 es
precision highp float;
const float PI = 3.14159265359;
const highp float HEIGHT_SCALE = (1.0 / 257.0) * 9500.0;
highp float decode_height(highp vec2 c) {
  return (c.r + c.g * 256.0) * HEIGHT_SCALE - 500.0;
}
