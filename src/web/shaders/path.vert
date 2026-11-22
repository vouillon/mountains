uniform mat4 proj;
uniform mat4 transform;

// Bound where Viewer.build_gpx_path points them: unbound, the linker may
// number them in any order.
layout(location = 0) in vec3 prev_pos;
layout(location = 1) in vec3 position;
layout(location = 2) in vec3 next_pos;
layout(location = 3) in float side;

uniform vec4 u_color;
uniform vec2 u_viewport;
uniform float u_linewidth;

out vec4 v_color;

// Shape of the radial mesh this trace is draped over, from the same
// [radial_params] the terrain program is fed.
uniform float sector_angle; // angle between two adjacent sectors
uniform int last_ring;      // index of the outermost ring

const float PI = 3.14159265359;

// Never set, so always 0, but the compiler cannot know it: added to a loop's
// count, it keeps the loop a loop. Unrolled, the three points of a trace
// segment, each on the three corners of its mesh triangle, inlined the whole
// height lookup (rings, canopy) nine times, and the program took 2.2 s to link
// on a first visit; the shadow program, which has it once, took 0.1.
uniform int u_zero;

float radialDistance(int ring) {
  return grid_scale * (exp(grid_k * float(ring)) - 1.0);
}

float radialVertexHeight(vec2 pos_plane, float r) {
  vec2 proj_xy =
      vec2(pos_plane.x * (1.0 + pos_plane.y * meridian_conv),
           pos_plane.y - 0.5 * pos_plane.x * pos_plane.x * meridian_conv);
  vec2 coord = (center_offset + proj_xy) * inv_delta;
  vec2 norm_coord = coord * inv_w + 0.5;
  float grid_spacing = grid_k * (r + grid_scale);
  float lod_raw = log2(grid_spacing * inv_avg_delta);
  highp vec2 c0 = hd_bias4[0].w > 0.5 ? hdCoord(0, coord) : vec2(-1.0);
  highp vec2 c1 = hd_bias4[1].w > 0.5 ? hdCoord(1, coord) : vec2(-1.0);
  highp vec2 c2 = hd_bias4[2].w > 0.5 ? hdCoord(2, coord) : vec2(-1.0);
  highp vec2 c3 = hd_bias4[3].w > 0.5 ? hdCoord(3, coord) : vec2(-1.0);
  highp vec2 cn = slotCoord(c0, c1, c2, c3, u_canopySlot);
  highp vec2 cf = slotCoord(c0, c1, c2, c3, u_canopyFarSlot);
  // Same selector as the mesh: a trace resolving its height from a different
  // ring than the ground under it would float or sink.
  float height =
      sampleTerrainHeight(coord, norm_coord, lod_raw, c0, c1, c2, c3);
  // And the canopy the mesh is raised by, as [computeRadialVertex].
  height += canopyLift(norm_coord, cn, cf, r, grid_spacing);
  return height - r * r * 6.8306e-8;
}

vec3 barycentric(vec2 p, vec2 a, vec2 b, vec2 c) {
  float denom = (b.y - c.y) * (a.x - c.x) + (c.x - b.x) * (a.y - c.y);
  float u = ((b.y - c.y) * (p.x - c.x) + (c.x - b.x) * (p.y - c.y)) / denom;
  float v = ((c.y - a.y) * (p.x - c.x) + (a.x - c.x) * (p.y - c.y)) / denom;
  return vec3(u, v, 1.0 - u - v);
}

vec3 raisedTerrainPosition(vec2 pos_plane) {
  float radius = length(pos_plane);
  float ring_float = log((radius / grid_scale) + 1.0) / grid_k;
  int ring = clamp(int(floor(ring_float)), 0, last_ring - 1);
  float angle =
      dot(pos_plane, pos_plane) > 0.0 ? atan(pos_plane.y, pos_plane.x) : 0.0;
  float local_angle = mod(angle - PI / 4.0, PI / 2.0);
  if (local_angle < 0.0)
    local_angle += PI / 2.0;
  float sector_fraction = fract(local_angle / sector_angle);
  float angle0 = angle - sector_fraction * sector_angle;
  float radius0 = radialDistance(ring);
  float radius1 = radialDistance(ring + 1);

  // Pick which of the cell's two triangles holds [pos_plane] in 2D first so we
  // only sample heights at its 3 corners instead of all 4. Matches the terrain
  // strip's A0, B0, A1, B1 ordering and diagonal.
  bool upper = true;
  if (ring != 0) {
    vec2 d0 = vec2(cos(angle0), sin(angle0));
    vec2 d1 = vec2(cos(angle0 + sector_angle), sin(angle0 + sector_angle));
    vec3 w0 = barycentric(pos_plane, d0 * radius0, d0 * radius1, d1 * radius0);
    upper = min(w0.x, min(w0.y, w0.z)) < -0.00001;
  }

  // Scalar accumulators instead of a dynamically indexed array eliminate local
  // memory stack allocations on Mali while keeping the loop rolled via u_zero.
  float h0 = 0.0, h1 = 0.0, h = 0.0;
  for (int i = 0; i < 3 + u_zero; i++) {
    float a = angle0 + ((i == 2 || (upper && i == 0)) ? sector_angle : 0.0);
    float r = (i == 1 || (upper && i == 2)) ? radius1 : radius0;
    h = radialVertexHeight(vec2(cos(a), sin(a)) * r, r);
    if (i == 0)
      h0 = h;
    else if (i == 1)
      h1 = h;
  }
  float h2 = h;

  vec2 d0 = vec2(cos(angle0), sin(angle0));
  vec2 d1 = vec2(cos(angle0 + sector_angle), sin(angle0 + sector_angle));
  vec2 p0_xy = (upper ? d1 : d0) * radius0;
  vec2 p1_xy = d0 * radius1;
  vec2 p2_xy = d1 * (upper ? radius1 : radius0);
  vec3 weights = barycentric(pos_plane, p0_xy, p1_xy, p2_xy);
  vec3 p0 = vec3(p0_xy, h0);
  vec3 p1 = vec3(p1_xy, h1);
  vec3 p2 = vec3(p2_xy, h2);
  vec3 pos = vec3(pos_plane, weights.x * h0 + weights.y * h1 + weights.z * h2);
  vec3 normal = normalize(cross(p1 - p0, p2 - p0));
  if (normal.z < 0.0)
    normal = -normal;
  float offset = 0.002 + 0.10 * min(1.0, length(pos_plane) / 1000.0);
  return pos + normal * offset;
}

void main() {
  v_color = u_color;

  vec3 viewA = vec3(0.0);
  vec3 viewP = vec3(0.0);
  vec3 v = vec3(0.0);
  for (int i = 0; i < 3 + u_zero; i++) {
    vec2 pt = i == 0 ? position.xy : (i == 1 ? prev_pos.xy : next_pos.xy);
    v = (transform * vec4(raisedTerrainPosition(pt), 1.0)).xyz;
    if (i == 0)
      viewA = v;
    else if (i == 1)
      viewP = v;
  }
  vec3 viewN = v;

  bool use_n2 = abs(side) > 1.5;
  const float NEAR = 0.5;

  if (viewA.z > -NEAR) {
    bool p_vis = viewP.z < -NEAR;
    bool n_vis = viewN.z < -NEAR;
    if (!p_vis && !n_vis) {
      gl_Position = vec4(2.0, 2.0, 2.0, 1.0);
      return;
    }
    vec3 ref = use_n2 ? (n_vis ? viewN : viewP) : (p_vis ? viewP : viewN);
    float t = (-NEAR - viewA.z) / (ref.z - viewA.z);
    viewA = mix(viewA, ref, t);
  }

  float s = (side > 0.0) ? 1.0 : -1.0;

  vec3 D = use_n2 ? (viewN - viewA) : (viewA - viewP);
  if (length(D) < 0.001) {
    D = use_n2 ? (viewA - viewP) : (viewN - viewA);
  }

  float depthA = -viewA.z;
  // Screen direction, in pixels: the perpendicular below is only one in pixel
  // space if [dir] is measured there, and NDC is stretched by the viewport's
  // aspect (a ribbon taken perpendicular in NDC came out up to 7% thin and
  // sheared by 22 degrees on a portrait phone).
  vec2 dir = vec2((D.x * depthA + viewA.x * D.z) * proj[0][0],
                  (D.y * depthA + viewA.y * D.z) * proj[1][1]) *
             u_viewport;

  float len = length(dir);
  vec2 n = (len > 0.001) ? vec2(-dir.y, dir.x) / len : vec2(0.0, 1.0);

  vec4 clipA = proj * vec4(viewA, 1.0);
  vec2 offset_ndc = (n * (s * u_linewidth) / u_viewport) * clipA.w;

  // Keep the near-field ribbon ahead of its source triangle without pulling
  // distant paths forward by a large world-space distance.
  float depth_bias =
      1e-5 * (1.0 - smoothstep(50.0, 300.0, length(position.xy)));
  gl_Position =
      vec4(clipA.xy + offset_ndc, clipA.z - (depth_bias * clipA.w), clipA.w);
}
