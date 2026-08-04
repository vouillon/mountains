in vec3 prev_pos;
in vec3 position;
in vec3 next_pos;
in float side;

uniform vec4 u_color;
uniform vec2 u_viewport;
uniform float u_linewidth;

out vec4 v_color;

const float PI = 3.14159265359;
const float SECTOR_ANGLE = PI / 1024.0;
const int LAST_RING = 1023;

struct TerrainSurface {
  vec3 position;
  vec3 normal;
};

float radialDistance(int ring) {
  return grid_scale * (exp(grid_k * float(ring)) - 1.0);
}

float radialVertexHeight(vec2 pos_plane) {
  float r = length(pos_plane);
  vec2 proj =
      vec2(pos_plane.x * (1.0 + pos_plane.y * meridian_conv),
           pos_plane.y - 0.5 * pos_plane.x * pos_plane.x * meridian_conv);
  vec2 coord = (center_offset + proj) * inv_delta;
  vec2 norm_coord = coord * inv_w + 0.5;
  float grid_spacing = grid_k * (r + grid_scale);
  float lod_raw = log2(grid_spacing * inv_avg_delta);
  // Same selector as the mesh: a trace resolving its height from a different
  // ring than the ground under it would float or sink.
  float height = sampleTerrainHeight(coord, norm_coord, lod_raw);
  return height - r * r * 6.8306e-8;
}

vec3 radialVertex(float radius, float angle) {
  vec2 pos = vec2(cos(angle), sin(angle)) * radius;
  return vec3(pos, radialVertexHeight(pos));
}

vec3 barycentric(vec2 p, vec2 a, vec2 b, vec2 c) {
  float denom = (b.y - c.y) * (a.x - c.x) + (c.x - b.x) * (a.y - c.y);
  float u = ((b.y - c.y) * (p.x - c.x) + (c.x - b.x) * (p.y - c.y)) / denom;
  float v = ((c.y - a.y) * (p.x - c.x) + (a.x - c.x) * (p.y - c.y)) / denom;
  return vec3(u, v, 1.0 - u - v);
}

TerrainSurface terrainSurface(vec2 pos_plane) {
  float radius = length(pos_plane);
  float ring_float = log((radius / grid_scale) + 1.0) / grid_k;
  int ring = clamp(int(floor(ring_float)), 0, LAST_RING - 1);
  float angle = atan(pos_plane.y, pos_plane.x);
  float local_angle = mod(angle - PI / 4.0, PI / 2.0);
  if (local_angle < 0.0)
    local_angle += PI / 2.0;
  float sector_fraction = fract(local_angle / SECTOR_ANGLE);
  float angle0 = angle - sector_fraction * SECTOR_ANGLE;
  float angle1 = angle0 + SECTOR_ANGLE;
  float radius0 = radialDistance(ring);
  float radius1 = radialDistance(ring + 1);

  vec3 a0 = radialVertex(radius0, angle0);
  vec3 b0 = radialVertex(radius1, angle0);
  vec3 a1 = radialVertex(radius0, angle1);
  vec3 b1 = radialVertex(radius1, angle1);
  // Matches the terrain strip's A0, B0, A1, B1 ordering and diagonal.
  vec3 weights;
  vec3 p0;
  vec3 p1;
  vec3 p2;
  if (ring == 0) {
    weights = barycentric(pos_plane, a1.xy, b0.xy, b1.xy);
    p0 = a1;
    p1 = b0;
    p2 = b1;
  } else {
    weights = barycentric(pos_plane, a0.xy, b0.xy, a1.xy);
    if (min(weights.x, min(weights.y, weights.z)) < -0.00001) {
      weights = barycentric(pos_plane, a1.xy, b0.xy, b1.xy);
      p0 = a1;
      p1 = b0;
      p2 = b1;
    } else {
      p0 = a0;
      p1 = b0;
      p2 = a1;
    }
  }

  TerrainSurface surface;
  surface.position = weights.x * p0 + weights.y * p1 + weights.z * p2;
  surface.normal = normalize(cross(p1 - p0, p2 - p0));
  if (surface.normal.z < 0.0)
    surface.normal = -surface.normal;
  return surface;
}

vec3 raisedTerrainPosition(vec2 pos_plane) {
  TerrainSurface surface = terrainSurface(pos_plane);
  float distance = length(pos_plane);
  float offset = 0.002 + 0.10 * min(1.0, distance / 1000.0);
  return surface.position + surface.normal * offset;
}

void main() {
  v_color = u_color;

  vec4 viewA = transform * vec4(raisedTerrainPosition(position.xy), 1.0);
  vec4 viewP = transform * vec4(raisedTerrainPosition(prev_pos.xy), 1.0);
  vec4 viewN = transform * vec4(raisedTerrainPosition(next_pos.xy), 1.0);

  const float NEAR = 0.5;

  if (viewA.z > -NEAR) {
    bool p_vis = viewP.z < -NEAR;
    bool n_vis = viewN.z < -NEAR;
    if (!p_vis && !n_vis) {
      gl_Position = vec4(2.0, 2.0, 2.0, 1.0);
      return;
    }
    vec4 ref = p_vis ? viewP : viewN;
    float t = (-NEAR - viewA.z) / (ref.z - viewA.z);
    viewA = mix(viewA, ref, t);
  }

  bool use_n2 = abs(side) > 1.5;
  float s = (side > 0.0) ? 1.0 : -1.0;

  vec3 D = use_n2 ? (viewN.xyz - viewA.xyz) : (viewA.xyz - viewP.xyz);
  if (length(D) < 0.001) {
    D = use_n2 ? (viewA.xyz - viewP.xyz) : (viewN.xyz - viewA.xyz);
  }

  float depthA = -viewA.z;
  vec2 dir = vec2((D.x * depthA + viewA.x * D.z) * proj[0][0],
                  (D.y * depthA + viewA.y * D.z) * proj[1][1]);

  float len = length(dir);
  vec2 n = (len > 0.001) ? vec2(-dir.y, dir.x) / len : vec2(0.0, 1.0);

  vec4 clipA = proj * viewA;
  vec2 offset_ndc = (n * (s * u_linewidth) / u_viewport) * clipA.w;

  // Keep the near-field ribbon ahead of its source triangle without pulling
  // distant paths forward by a large world-space distance.
  float depth_bias =
      1e-5 * (1.0 - smoothstep(50.0, 300.0, length(position.xy)));
  gl_Position =
      vec4(clipA.xy + offset_ndc, clipA.z - (depth_bias * clipA.w), clipA.w);
}
