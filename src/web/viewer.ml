let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with
    | Ok v -> Lwt.wakeup u v
    | Error err -> Lwt.wakeup_exn u (Jv.Error err) );
  t

let _ =
  Printexc.register_printer (function
    | Jv.Error e -> Some (Jstr.to_string (Jv.Error.message e))
    | _ -> None)

let ( let* ) = Lwt.bind
let now () = Jv.to_float (Jv.call (Jv.get Jv.global "performance") "now" [||])
let now_ms = now

(* GPU Timer using EXT_disjoint_timer_query_webgl2 with fallback to glFinish *)
module Gpu_timer = struct
  type ext = { time_elapsed_ext : int }

  let extension : ext option ref = ref None

  let init ctx =
    let ext =
      Brr_canvas.Gl.get_extension ctx (Jstr.v "EXT_disjoint_timer_query_webgl2")
    in
    if Jv.is_some ext then begin
      let time_elapsed_ext = Jv.to_int (Jv.get ext "TIME_ELAPSED_EXT") in
      extension := Some { time_elapsed_ext };
      Brr.Console.(
        log [ Jstr.v "GPU Timer: Using EXT_disjoint_timer_query_webgl2" ])
    end
    else
      Brr.Console.(
        log
          [ Jstr.v "GPU Timer: Extension unavailable, using glFinish fallback" ])

  (* Pending queries waiting for results *)
  type pending_query = { name : string; query : Jv.t; t0 : float }

  let pending_queries : pending_query list ref = ref []

  (* Poll for completed queries and log results *)
  let poll_results ctx =
    let module Gl = Brr_canvas.Gl in
    let gl = (Obj.magic ctx : Jv.t) in
    let query_result_available = 0x8867 in
    (* GL_QUERY_RESULT_AVAILABLE *)
    let query_result = 0x8866 in
    (* GL_QUERY_RESULT *)
    let gpu_disjoint = 0x8FBB in
    (* GPU_DISJOINT_EXT *)

    match !extension with
    | None -> ()
    | Some _ ->
        (* Check if GPU was disjoint (results may be invalid) *)
        let disjoint =
          Jv.to_bool (Jv.call gl "getParameter" [| Jv.of_int gpu_disjoint |])
        in
        (if disjoint then
           Brr.Console.(
             log
               [
                 Jstr.v "GPU Timer: Disjoint detected - results may be invalid";
               ]));

        let still_pending = ref [] in
        List.iter
          (fun pq ->
            let available =
              Jv.to_bool
                (Jv.call gl "getQueryParameter"
                   [| pq.query; Jv.of_int query_result_available |])
            in
            if available then begin
              let time_ns =
                Jv.to_float
                  (Jv.call gl "getQueryParameter"
                     [| pq.query; Jv.of_int query_result |])
              in
              let time_ms = time_ns /. 1_000_000. in
              let wall_ms = now () -. pq.t0 in
              let prefix = if disjoint then "[DISJOINT] " else "" in
              Brr.Console.(
                log
                  [
                    Jstr.v
                      (Printf.sprintf "%s%s: GPU %.2fms (wall %.1fms)" prefix
                         pq.name time_ms wall_ms);
                  ]);
              ignore (Jv.call gl "deleteQuery" [| pq.query |])
            end
            else still_pending := pq :: !still_pending)
          !pending_queries;
        pending_queries := List.rev !still_pending

  (* Start timing a GPU operation *)
  let begin_query ctx name =
    let module Gl = Brr_canvas.Gl in
    match !extension with
    | None -> None
    | Some ext ->
        let gl = (Obj.magic ctx : Jv.t) in
        let query = Jv.call gl "createQuery" [||] in
        ignore
          (Jv.call gl "beginQuery" [| Jv.of_int ext.time_elapsed_ext; query |]);
        Some { name; query; t0 = now () }

  (* End timing and queue for result polling *)
  let end_query ctx pq_opt =
    match (!extension, pq_opt) with
    | Some ext, Some pq ->
        let gl = (Obj.magic ctx : Jv.t) in
        ignore (Jv.call gl "endQuery" [| Jv.of_int ext.time_elapsed_ext |]);
        pending_queries := pq :: !pending_queries
    | _ -> ()
end

(** Time a GPU operation. Uses extension if available, otherwise glFinish. *)
let time_gpu ctx name f =
  let module Gl = Brr_canvas.Gl in
  match !Gpu_timer.extension with
  | Some _ ->
      let pq = Gpu_timer.begin_query ctx name in
      let result = f () in
      Gpu_timer.end_query ctx pq;
      result
  | None ->
      let t0 = now () in
      let result = f () in
      Gl.finish ctx;
      let t1 = now () in
      Brr.Console.(log [ Jstr.v (Printf.sprintf "%s: %.1fms" name (t1 -. t0)) ]);
      result

let request_animation_frame () =
  let t, u = Lwt.task () in
  ignore (Brr.G.request_animation_frame (fun _ -> Lwt.wakeup u ()));
  t

let sleep s =
  let t, u = Lwt.task () in
  ignore
    (Brr.G.set_timeout ~ms:(truncate (s *. 1000.)) (fun () -> Lwt.wakeup u ()));
  t

let message = ref None

let remove_message () =
  match !message with
  | Some msg ->
      Brr.El.remove msg;
      message := None
  | None -> ()

let display_message msg =
  remove_message ();
  let msg =
    Brr.El.(
      v (Jstr.v "div")
        ~at:[ Brr.At.class' (Jstr.v "message") ]
        [ txt (Jstr.v msg) ])
  in
  Brr.El.append_children (Brr.Document.body Brr.G.document) [ msg ];
  message := Some msg

let display_temporary_message msg =
  display_message msg;
  ignore (Brr.G.set_timeout ~ms:10000 remove_message)

let update_startup_status msg loading =
  let doc = Brr.G.document in
  let status = Brr.Document.find_el_by_id doc (Jstr.v "status-text") in
  Option.iter
    (fun el ->
      Brr.El.set_children el [ Brr.El.txt (Jstr.v msg) ];
      Brr.El.set_class (Jstr.v "loading") loading el)
    status

let hide_startup_overlay () =
  let doc = Brr.G.document in
  let overlay = Brr.Document.find_el_by_id doc (Jstr.v "startup-overlay") in
  Option.iter
    (fun el ->
      Brr.El.set_class (Jstr.v "hidden") true el;
      ignore
        (Brr.G.set_timeout ~ms:1100 (fun () ->
             Brr.El.set_inline_style (Jstr.v "display") (Jstr.v "none") el)))
    overlay

let show_startup_overlay msg loading =
  let doc = Brr.G.document in
  update_startup_status msg loading;
  let overlay = Brr.Document.find_el_by_id doc (Jstr.v "startup-overlay") in
  match overlay with
  | Some el ->
      Brr.El.set_inline_style (Jstr.v "display") (Jstr.v "flex") el;
      (* We need to wait for the browser to process the 'display' change
         before we can trigger the 'opacity' transition. *)
      let* () = request_animation_frame () in
      let* () = request_animation_frame () in
      Brr.El.set_class (Jstr.v "hidden") false el;
      (* Wait for the fade-in transition to complete *)
      sleep 0.5
  | None -> Lwt.return_unit

let navigate_to uri =
  Lwt.async (fun () ->
      let* () = show_startup_overlay "Navigating..." false in
      Brr.Window.set_location Brr.G.window uri;
      Lwt.return_unit)

(* Sky colors *)
let fog_linear = (0.17, 0.38, 0.79)
let zenith_linear = (0.02, 0.12, 0.55)

(* Web Utils Aliases *)
let pi = Web_utils.pi
let next_power_of_two = Web_utils.next_power_of_two

(* Types *)

type program = Web_utils.program_spec = {
  vertex_shader : string;
  fragment_shader : string;
  attributes : string list;
}

let n_sectors = 512
let n_rings = 1024

(* type orientation = Quaternion.t *)

(* Input mode: Sensor (device orientation) vs Manual (touch/mouse drag) *)
type input_mode = Sensor | Manual

let input_mode = ref Sensor
let zoom = ref 1.0
let min_zoom = 0.5
let max_zoom = 3.0

(* Math Helpers *)

let rotation_matrix orientation = Quaternion.to_matrix orientation

let compute_azimuth m =
  let v_up = Matrix.(m *> { x = 0.; y = 1.; z = 0.; w = 0. }) in
  let v_fwd = Matrix.(m *> { x = 0.; y = 0.; z = -1.; w = 0. }) in
  let len_up = (v_up.x ** 2.) +. (v_up.y ** 2.) in
  let len_fwd = (v_fwd.x ** 2.) +. (v_fwd.y ** 2.) in
  let azimuth =
    if len_up > len_fwd then atan2 v_up.y v_up.x else atan2 v_fwd.y v_fwd.x
  in
  azimuth -. (pi /. 2.)

(* GLSL Common *)

let quad_vertex_shader =
  {|#version 300 es
    out vec2 uv;
    void main() {
      float x = float(gl_VertexID & 1);
      float y = float(gl_VertexID >> 1);
      uv = vec2(x, y);
      gl_Position = vec4(2.0 * x - 1.0, 2.0 * y - 1.0, 0.0, 1.0);
    }
  |}

let common_fragment_header =
  {|#version 300 es
    precision highp float;
    const float PI = 3.14159265359;
    const highp float HEIGHT_SCALE = (1.0/257.0) * 9500.0;
    highp float decode_height(highp vec2 c) {
      return (c.r * 256.0 + c.g) * HEIGHT_SCALE - 500.0;
    }
  |}

(* Shared GLSL code for radial grid vertex shaders (terrain and shadow).
   Contains common uniforms and a function to compute world position with height. *)
let radial_vertex_common =
  {|
  // Radial grid uniforms (shared between terrain and shadow shaders)
  uniform highp int w;
  uniform highp int w_mask;
  uniform highp int w_shift;
  uniform highp vec2 center_offset;
  uniform highp float snapped_alpha;
  uniform highp float inv_sectors_div;
  uniform highp float grid_k;
  uniform highp float grid_base;
  uniform highp float grid_scale;
  uniform highp vec2 inv_delta;
  uniform highp float inv_w;
  uniform highp float inv_avg_delta;
  uniform highp int max_lod;
  uniform mediump sampler2D relief;

  // Output structure for radial vertex computation
  struct RadialVertex {
    highp vec2 pos_plane;      // Position relative to camera in meters
    highp vec2 coord_meters;   // Absolute world position in meters
    highp vec2 norm_coord;     // Normalized texture coordinate (0..1)
    highp float height;        // Terrain height at this position
  };

  // Compute radial grid vertex position and sample terrain height
  RadialVertex computeRadialVertex() {
    const float PI = 3.14159265359;
    RadialVertex v;

    int sector = gl_VertexID & w_mask;
    int ring = gl_VertexID >> w_shift;
    float theta = (float(sector) * inv_sectors_div) * (PI / 2.0) - (PI / 4.0);
    float angle = theta + snapped_alpha + (PI / 2.0);

    // Exponential radial distance: r = A(e^(k*ring) - 1)
    highp float r = grid_scale * (exp(grid_k * float(ring)) - 1.0);
    v.pos_plane = vec2(cos(angle), sin(angle)) * r;
    v.coord_meters = center_offset + v.pos_plane;
    highp vec2 coord = v.coord_meters * inv_delta;

    // Grid spacing for LOD: dr = k(r + A)
    highp float grid_spacing = grid_k * (r + grid_scale);

    // LOD level based on grid spacing
    highp float lod_f = max(0.0, log2(grid_spacing * inv_avg_delta));
    int lod = min(int(lod_f), max_lod);

    // Texture size at this LOD
    ivec2 tex_size = textureSize(relief, lod);

    // Normalized coordinate (Y flipped: row 0 is north)
    v.norm_coord = vec2(coord.x, float(w) - coord.y) * inv_w;

    // Texel position for manual bilinear interpolation
    highp vec2 lod_pos = v.norm_coord * vec2(tex_size);
    highp vec2 lod_tex_pos = clamp(lod_pos, vec2(0.0), vec2(tex_size - 1));
    highp ivec2 base = ivec2(lod_tex_pos);
    highp vec2 f = fract(lod_tex_pos);

    // Fetch 4 samples
    highp vec2 s00 = texelFetch(relief, base, lod).rg;
    highp vec2 s10 = texelFetch(relief, min(base + ivec2(1,0), tex_size - 1), lod).rg;
    highp vec2 s01 = texelFetch(relief, min(base + ivec2(0,1), tex_size - 1), lod).rg;
    highp vec2 s11 = texelFetch(relief, min(base + ivec2(1,1), tex_size - 1), lod).rg;

    // Decode heights: high*256 + low, scaled to [-500, 9000]
    const highp float HEIGHT_SCALE = (1.0/257.0) * 9500.0;
    highp vec4 H = vec4(
      dot(s00, vec2(256.0, 1.0)),
      dot(s10, vec2(256.0, 1.0)),
      dot(s01, vec2(256.0, 1.0)),
      dot(s11, vec2(256.0, 1.0))
    ) * HEIGHT_SCALE - 500.0;

    v.height = mix(mix(H.x, H.y, f.x), mix(H.z, H.w, f.x), f.y);

    return v;
  }
  |}

(* Shader Programs *)

(* Terrain shader with compile-time CLC toggle for optimal code generation *)
let terrain_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        precision highp int;
        uniform mat4 proj;
        uniform mat4 transform;|}
      ^ radial_vertex_common
      ^ {|
        out mediump float v_dist;
        out mediump float v_h;
        out highp vec2 reliefCoord;
        out highp vec3 v_world_pos;

        void main() {
          RadialVertex rv = computeRadialVertex();

          reliefCoord = rv.norm_coord + (0.5 * inv_w);
          v_world_pos = vec3(rv.coord_meters, rv.height);

          highp vec4 pos = transform * vec4(rv.pos_plane, rv.height, 1.0);
          v_dist = length(pos.xyz);
          v_h = rv.height;
          gl_Position = proj * pos;
        }
      |};
    fragment_shader =
      common_fragment_header
      ^ {|
        precision mediump float;  // Default mediump for mobile performance
        precision highp sampler2DArray;

        uniform mediump sampler2D relief;
        uniform mediump sampler2D ao;
        uniform mediump sampler2D u_detailMap;  // Packed RGBA: R=Rock, G=Grass, B=Forest, A=Ice
        uniform highp sampler2DArrayShadow shadow_map;  // Hardware shadow comparison

        // CLC Material System Uniforms
        uniform mediump usampler2DArray u_coverMap;  // CLC ID clipmap (layers)
        uniform mediump sampler2D u_paletteTex;  // 128x1 RGBA palette
        uniform highp vec2 u_cameraOffset;       // Camera world position (center of clipmap)
        uniform highp float u_baseExtent;       // Extent of level 0 in meters (highp for coord math)
        uniform int u_numLevels;                 // Number of clipmap levels

        uniform highp mat4 shadow_matrices[3];   // Must be highp for projection
        uniform mediump float shadow_splits[3];
        uniform vec3 u_lightDir;                 // Pre-normalized on CPU

        in highp vec2 reliefCoord;               // Highp for texture coords
        in mediump float v_dist;
        in mediump float v_h;
        in highp vec3 v_world_pos;               // Highp for world coords

        out lowp vec4 color;

        // ========== CLC Material System ==========

        struct Surface {
          vec3 albedo;
          float roughness;
          vec4 detailWeights;  // RGBA = Rock, Grass, Forest, Ice
          float waterFactor;
        };

        // Lookup surface properties from palette by material ID
        Surface getSurfaceFromID(float id) {
          Surface s;
          // Use texelFetch for direct integer addressing (no filtering overhead)
          ivec2 coord = ivec2(int(id) * 2, 0);
          vec4 pixelA = texelFetch(u_paletteTex, coord, 0);
          vec4 pixelB = texelFetch(u_paletteTex, coord + ivec2(1, 0), 0);

          // sRGB to linear approximation (palette stored as sRGB)
          s.albedo = pixelA.rgb * pixelA.rgb;  // Simplified gamma decode
          s.roughness = pixelA.a;
          s.detailWeights = vec4(pixelB.rgb, 0.0);  // RGB weights, ice computed below
          s.waterFactor = pixelB.a;

          // ICE LOGIC: If RGB weights are ~0 and not water, force ice (alpha channel)
          float weightSum = dot(pixelB.rgb, vec3(1.0));
          if (weightSum < 0.05 && s.waterFactor < 0.5) {
            s.detailWeights.a = 1.0;  // Force ice/snow weight
          }

          return s;
        }

        // Manual bilinear filtering of CLC IDs (blend Surface properties)
        Surface sampleCLCBilinear(highp vec2 worldPos) {
          // Calculate distance from center (camera) for LOD selection
          highp vec2 relPos = worldPos - u_cameraOffset;

          // Select clipmap level based on distance
          // Level L covers extent = u_baseExtent * 2^L
          // We want the finest level that covers this point
          highp float desiredLevel = max(0.0, 1.0 + log2(v_dist / u_baseExtent));
          int level = clamp(int(ceil(desiredLevel)), 0, u_numLevels - 1);

          // Calculate texture coordinates for this level
          highp float levelExtent = u_baseExtent * pow(2.0, float(level));
          highp vec2 texCoord = (relPos / levelExtent) + 0.5;

          vec2 texSize = vec2(textureSize(u_coverMap, 0).xy);
          highp vec2 texelPos = texCoord * texSize - 0.5;

          ivec2 p00 = ivec2(floor(texelPos));
          vec2 frac = fract(texelPos);

          // Clamp to valid range
          ivec2 maxCoord = ivec2(texSize) - 1;
          p00 = clamp(p00, ivec2(0), maxCoord);
          ivec2 p10 = clamp(p00 + ivec2(1,0), ivec2(0), maxCoord);
          ivec2 p01 = clamp(p00 + ivec2(0,1), ivec2(0), maxCoord);
          ivec2 p11 = clamp(p00 + ivec2(1,1), ivec2(0), maxCoord);

          // Sample 4 neighbors from selected array layer
          // usampler2DArray fetch returns uvec4, we take .r component
          float id00 = float(texelFetch(u_coverMap, ivec3(p00, level), 0).r);
          float id10 = float(texelFetch(u_coverMap, ivec3(p10, level), 0).r);
          float id01 = float(texelFetch(u_coverMap, ivec3(p01, level), 0).r);
          float id11 = float(texelFetch(u_coverMap, ivec3(p11, level), 0).r);

          // Get surfaces for each neighbor
          Surface s00 = getSurfaceFromID(id00);
          Surface s10 = getSurfaceFromID(id10);
          Surface s01 = getSurfaceFromID(id01);
          Surface s11 = getSurfaceFromID(id11);

          // Bilinear blend of surface properties
          Surface result;
          vec3 a0 = mix(s00.albedo, s10.albedo, frac.x);
          vec3 a1 = mix(s01.albedo, s11.albedo, frac.x);
          result.albedo = mix(a0, a1, frac.y);

          float r0 = mix(s00.roughness, s10.roughness, frac.x);
          float r1 = mix(s01.roughness, s11.roughness, frac.x);
          result.roughness = mix(r0, r1, frac.y);

          vec4 w0 = mix(s00.detailWeights, s10.detailWeights, frac.x);
          vec4 w1 = mix(s01.detailWeights, s11.detailWeights, frac.x);
          result.detailWeights = mix(w0, w1, frac.y);

          float wf0 = mix(s00.waterFactor, s10.waterFactor, frac.x);
          float wf1 = mix(s01.waterFactor, s11.waterFactor, frac.x);
          result.waterFactor = mix(wf0, wf1, frac.y);

          return result;
        }

        // Modify detail weights based on slope (steep = more rock)
        void applySlopeModification(inout Surface s, float slope) {
          // Steep slopes increase rock weight, decrease others
          float rockForce = smoothstep(0.15, 0.5, slope);

          // Scale down non-rock weights
          s.detailWeights.g *= (1.0 - rockForce);  // Reduce grass
          s.detailWeights.b *= (1.0 - rockForce);  // Reduce forest
          s.detailWeights.a *= (1.0 - rockForce);  // Reduce ice

          // Increase rock weight
          s.detailWeights.r += rockForce;

          // Normalize weights
          float total = dot(s.detailWeights, vec4(1.0));
          if (total > 0.01) {
            s.detailWeights /= total;
          }
        }

        // Triplanar sampling for packed RGBA detail map
        // Returns blended detail weights from all projection planes
        vec4 sampleTriplanarCombined(highp vec3 worldPos, vec3 normal) {
          highp float scale = 0.002;  // ~500m per texture repeat (matches validated debug scale)
          highp vec2 uv_xz = worldPos.xz * scale;
          highp vec2 uv_xy = worldPos.xy * scale;
          highp vec2 uv_yz = worldPos.yz * scale;

          vec3 blend = abs(normal);
          blend /= (blend.x + blend.y + blend.z + 0.0001);

          // Sample packed RGBA detail map from each projection plane
          vec4 d_xz = texture(u_detailMap, uv_xz);
          vec4 d_xy = texture(u_detailMap, uv_xy);
          vec4 d_yz = texture(u_detailMap, uv_yz);

          // Blend based on surface orientation (Corrected for Z-up)
          // Top/Bottom (blend.z) -> XY projection
          // Side-Y (blend.y)     -> XZ projection
          // Side-X (blend.x)     -> YZ projection
          return d_xy * blend.z + d_xz * blend.y + d_yz * blend.x;
        }

        // Procedural normal perturbation based on detail noise
        // Uses screen-space derivatives for directional bump mapping
        vec3 perturbNormal(vec3 geomNormal, vec4 texNoise, float roughness, vec4 detailWeights) {
            // 1. Define the "Bump Shape" for each material
            // We subtract 0.5 to center the noise [-0.5 to +0.5]

            // ROCK (Red): Strong, sharp cracks
            float rockBump = (texNoise.r - 0.5) * 0.5;

            // GRASS (Green): Soft, rolling mounds (Multiplied by 0.4 to be gentle)
            float grassBump = (texNoise.g - 0.5) * 0.4;

            // FOREST (Blue): Medium canopy lumps
            float forestBump = (texNoise.b - 0.5) * 0.6;

            // ICE (Alpha): Sharp crystalline facets
            float iceBump = (texNoise.a - 0.5) * 1.2;

            // 2. Mix them based on the current material weights
            float compositeBump = rockBump * detailWeights.r +
                                  grassBump * detailWeights.g +
                                  forestBump * detailWeights.b +
                                  iceBump * detailWeights.a;

            // 3. Scale by Roughness
            // Very smooth surfaces (mud/puddles) fill in cracks -> Less Bump.
            // However, Ice (shiny) is an exception; it needs to stay sharp.
            float intensity = 1.5 * roughness;

            // Hack: If it's mostly Ice or Water, force high sharpness despite low roughness
            if (detailWeights.a > 0.5) intensity = 1.0;

            // 4. Apply the Perturbation
            // We modify the X and Y components of the normal (assuming Z is up).
            // This tilts the normal vector based on the noise slope.
            vec3 pNormal = geomNormal;
            pNormal.xy += compositeBump * intensity;

            // 5. Re-normalize to ensure lighting remains correct
            return normalize(pNormal);
        }

        // Procedural water with organic shoreline
        float getWaterMask(highp vec2 worldPos, float waterFactor) {
          if (waterFactor >= 0.01 && waterFactor < 0.99) {
              float noise_val = texture(u_detailMap, worldPos.xy * 0.2).r;
              float jitter = (noise_val - 0.5) * 0.5;

              // Sharp threshold with smooth transition
              float threshold = 0.5 + jitter;
              return smoothstep(threshold - 0.15, threshold + 0.15, waterFactor);
          }
          return waterFactor;
        }

        vec3 applyWaterEffects(vec3 baseColor, float waterMask, vec2 worldPos) {
          if (waterMask < 0.01) return baseColor;

          // Deep water color (linear space)
          vec3 waterColor = vec3(0.01, 0.04, 0.12);

          // Shoreline foam zone
          float shoreZone = smoothstep(0.2, 0.6, waterMask) * (1.0 - smoothstep(0.6, 1.0, waterMask));
          vec3 foamColor = vec3(0.35, 0.40, 0.45);

          // Simple ripple pattern
          float ripples = sin(worldPos.x * 0.3) * sin(worldPos.y * 0.3) * 0.5 + 0.5;

          vec3 result = mix(baseColor, waterColor, waterMask);
          result = mix(result, foamColor, shoreZone * ripples * 0.4);

          return result;
        }

        // ========== Shadow Functions ==========

        float pcf_shadow(int layer, vec2 coords, float compare, vec2 texel_size) {
            float result = 0.0;
            for(int x = -1; x <= 1; ++x) {
                for(int y = -1; y <= 1; ++y) {
                    result += texture(shadow_map, vec4(coords + vec2(x,y) * texel_size, float(layer), compare));
                }
            }
            return result / 9.0;
        }

        // ========== Main ==========

        uniform vec3 u_fogColor;
        uniform vec3 u_zenithColor;

        void main() {
          // Define fog color early for use in water reflection
          vec3 fog_color = u_fogColor;

          // Decode normal from relief texture
          mediump vec2 encodedN = texture(relief, reliefCoord).ba;
          vec3 normal;
          normal.xy = encodedN * 2.0 - 1.0;
          normal.z = sqrt(max(0.0, 1.0 - dot(normal.xy, normal.xy)));

          vec3 lightDir = u_lightDir;  // Pre-normalized on CPU
          float cosTheta = clamp(dot(normal, lightDir), 0.0, 1.0);

          // === Shadow Calculation (unchanged) ===
          int cascade = 2;
          if (v_dist < shadow_splits[0]) cascade = 0;
          else if (v_dist < shadow_splits[1]) cascade = 1;

          float slopeScale = 1.0 - cosTheta;
          float texelSz = (cascade == 0) ? 1.0 : ((cascade == 1) ? 4.0 : 12.0);
          float normalOffset = texelSz * slopeScale;
          vec3 offset_pos = v_world_pos + normal * normalOffset;

          vec4 s_pos = shadow_matrices[cascade] * vec4(offset_pos, 1.0);
          vec3 proj_coords = s_pos.xyz / s_pos.w;
          proj_coords = proj_coords * 0.5 + 0.5;

          float current_depth = proj_coords.z;
          float bias = 0.0015;
          float shadow_val = pcf_shadow(cascade, proj_coords.xy, current_depth - bias, vec2(0.000488));
          if (proj_coords.z > 1.0) shadow_val = 1.0;

          // === Material System ===
          float slope = 1.0 - normal.z;
          vec3 terrain_color;
          vec3 final_normal = normal;

            // === CLC-Based Material ===
            Surface surface = sampleCLCBilinear(v_world_pos.xy);

            applySlopeModification(surface, slope);

            // Sample packed detail map via triplanar projection
            vec4 texNoise = sampleTriplanarCombined(v_world_pos, normal);

            // -----------------------------------------------------------------------
            // STEP A: HEIGHT-BASED BLENDING ("Clumping")
            // -----------------------------------------------------------------------
            // Solves "Ghosting" in sparse areas (e.g. 50%% Rock / 50%% Grass).
            // Boosts the channel that matches the texture noise (High noise = Top layer).

            float heightSharpness = 4.0;
            vec4 heightWeights = surface.detailWeights * (texNoise + 0.2);
            heightWeights = pow(heightWeights, vec4(heightSharpness));

            // Re-normalize
            float weightSum = dot(heightWeights, vec4(1.0));
            vec4 finalWeights = (weightSum > 0.001) ? (heightWeights / weightSum) : surface.detailWeights;

            // -----------------------------------------------------------------------
            // STEP B: BIO-VARIATION (Color & Texture)
            // -----------------------------------------------------------------------

            // Calculate Procedural Macro Noise (Cheap Math, No Texture Lookup)
            // Creates large-scale patches (healthy vs dry, sediment vs clean)
            vec2 macroPos = v_world_pos.xy * 0.005;
            float macroNoise = sin(macroPos.x) * cos(macroPos.y * 0.8) +
                               sin(macroPos.x * 0.5 + macroPos.y * 1.5) * 0.5;
            float patchFactor = macroNoise * 0.3 + 0.5; // 0.0 to 1.0

            vec3 accumulatedColor = vec3(0.0);
            vec3 baseAlbedo = surface.albedo;

            // --- ROCK (Red Channel) ---
            if (finalWeights.r > 0.001) {
                float rockNoise = texNoise.r;

                // Sedimentation: Cracks (Low) = Dark Dirt, Tips (High) = Bleached Stone
                vec3 dirtColor = baseAlbedo * vec3(0.6, 0.55, 0.5);
                vec3 stoneColor = baseAlbedo * vec3(1.1, 1.1, 1.15);

                // Use raw texture value for more visible detail
                vec3 rockCol = mix(dirtColor, stoneColor, rockNoise);
                accumulatedColor += rockCol * finalWeights.r;
            }

            // --- GRASS (Green Channel) ---
            if (finalWeights.g > 0.001) {
                float grassNoise = texNoise.g;

                // Micro: Roots (Low) = Dry/Brown, Tips (High) = Lush
                vec3 rootColor = baseAlbedo * vec3(0.8, 0.7, 0.5);
                vec3 tipColor = baseAlbedo * vec3(1.15, 1.25, 1.0);
                vec3 microCol = mix(rootColor, tipColor, grassNoise);

                // Macro: Patches of dying grass (yellowish) vs healthy grass (blue-green)
                vec3 dyingPatch = vec3(1.05, 1.0, 0.8);
                vec3 healthyPatch = vec3(0.95, 1.0, 1.05);
                vec3 macroMod = mix(dyingPatch, healthyPatch, patchFactor);

                accumulatedColor += (microCol * macroMod) * finalWeights.g;
            }

            // --- FOREST (Blue Channel) ---
            if (finalWeights.b > 0.001) {
                float forestNoise = texNoise.b;

                // Volume: Deep Shadow (Low) vs Sunlit Canopy (High)
                vec3 deepShadow = baseAlbedo * vec3(0.2, 0.25, 0.3); // Very dark/cool
                vec3 sunlitTop = baseAlbedo * vec3(1.2, 1.25, 1.0);

                vec3 forestCol = mix(deepShadow, sunlitTop, forestNoise);
                accumulatedColor += forestCol * finalWeights.b;
            }

            // --- ICE (Alpha Channel) ---
            if (finalWeights.a > 0.001) {
                vec3 iceCol = mix(baseAlbedo, vec3(0.95), texNoise.a * 0.8);
                accumulatedColor += iceCol * finalWeights.a;
            }

            terrain_color = accumulatedColor;

            // Water handling
            float waterMask = getWaterMask(v_world_pos.xy, surface.waterFactor);
            terrain_color = applyWaterEffects(terrain_color, waterMask, v_world_pos.xy);

            // Procedural normal perturbation (replaces rock_normal_map)
            final_normal = perturbNormal(normal, texNoise, surface.roughness, finalWeights);

            // Water Wave Logic
            if (waterMask > 0.01) {
               // Distance-based fade: waves visible up to 2km, fully calm by 5km
               float waveFade = 1.0 - smoothstep(2000.0, 5000.0, v_dist);

               // Skip wave calculations if too far (optimization)
               if (waveFade > 0.01) {
                  // 1. Low frequency waves (Swell) - Isotropic Interference Pattern
                  // Using 3 waves at 120-degree offsets to eliminate directional banding
                  highp vec2 waveCoord = v_world_pos.xy * 0.05;

                  // Wave 1: 0 degrees, Freq 1.0
                  float w1 = sin(waveCoord.x);
                  // Wave 2: 120 degrees, Freq 1.1 (Detuned)
                  highp float input2 = (waveCoord.x * -0.5 + waveCoord.y * 0.866) * 1.1;
                  float w2 = sin(input2);
                  // Wave 3: 240 degrees, Freq 1.2 (Detuned)
                  highp float input3 = (waveCoord.x * -0.5 - waveCoord.y * 0.866) * 1.2;
                  float w3 = sin(input3);

                  // Analytical Derivatives
                  float dw1_dx = cos(waveCoord.x);
                  float dw1_dy = 0.0;

                  float dw2_base = cos(input2) * 1.1;
                  float dw2_dx = dw2_base * -0.5;
                  float dw2_dy = dw2_base * 0.866;

                  float dw3_base = cos(input3) * 1.2;
                  float dw3_dx = dw3_base * -0.5;
                  float dw3_dy = dw3_base * -0.866;

                  vec3 waveNormal = normalize(vec3(
                      -(dw1_dx + dw2_dx + dw3_dx),
                      -(dw1_dy + dw2_dy + dw3_dy),
                      20.0 // Higher divisor = flatter waves
                  ));

                  // 2. High frequency ripples - Texture based (Rotated & Layered)
                  // Use Green/Blue channels as X/Y vector components
                  // Sample 1: Base Scale (Green=X, Blue=Y) - Soft Noise
                  vec2 r1 = texture(u_detailMap, v_world_pos.xy * 0.15).gb * 2.0 - 1.0;

                  // Sample 2: Rotated & Scaled (Green=X, Blue=Y)
                  // Rotation breaks grid alignment
                  float c_rot = 0.8; // cos(37 deg)
                  float s_rot = 0.6; // sin(37 deg)
                  highp mat2 rot = mat2(c_rot, -s_rot, s_rot, c_rot);
                  highp vec2 uv2 = rot * v_world_pos.xy * 0.24 + vec2(7.1, 3.3);
                  vec2 r2 = texture(u_detailMap, uv2).gb * 2.0 - 1.0;

                  // Combine vectors
                  vec2 rippleXY = r1 + r2;

                  // Construct Normal
                  // Z-component controls flatness (Higher = Flatter)
                  vec3 rippleNormal = normalize(vec3(rippleXY, 8.0));

                  // Combine: Swell provides structure, Ripples provide detail
                  vec3 waterNormal = normalize(waveNormal + rippleNormal);

                  // Fade towards flat normal (0, 0, 1) with distance
                  waterNormal = normalize(mix(vec3(0.0, 0.0, 1.0), waterNormal, waveFade));

                  // Blend terrain normal with water normal
                  final_normal = normalize(mix(final_normal, waterNormal, waterMask));
               } else {
                  // Beyond fade distance: use flat vertical normal for water
                  final_normal = normalize(mix(final_normal, vec3(0.0, 0.0, 1.0), waterMask));
               }
            }

            // Store roughness and reflection properties for lighting
            float material_roughness = surface.roughness;
            float reflectivity = (1.0 - material_roughness) * 0.6;  // Smooth = reflective

            // Ice and water get extra reflection
            float iceAmount = surface.detailWeights.a;
            if (iceAmount > 0.1 || waterMask > 0.1) {
              reflectivity = max(reflectivity, 0.4);
            }

            // === Specular & Environment Reflection ===
            vec3 viewDir = normalize(-v_world_pos);  // Approximate view direction
            vec3 halfVec = normalize(lightDir + viewDir);

            // GGX-inspired specular (simplified)
            float NdotH = max(0.0, dot(final_normal, halfVec));
            float roughSq = material_roughness * material_roughness;
            float denom = NdotH * NdotH * (roughSq - 1.0) + 1.0;
            float D = roughSq / (3.14159 * denom * denom + 0.0001);
            float specular = D * max(0.0, dot(final_normal, lightDir));

            // Environment reflection (sky color for glossy surfaces)
            vec3 reflectDir = reflect(-viewDir, final_normal);
            float skyReflect = max(0.0, reflectDir.z);
            // Updated to match new sky: Horizon(u_fogColor) -> Zenith(u_zenithColor)
            // Sky Shader uses smoothstep(0.0, 0.35, cos_theta)
            // We use the exact same mixing factor for consistency
            float sky_mix = smoothstep(0.0, 0.35, skyReflect);
            vec3 envColor = mix(u_fogColor, u_zenithColor, sky_mix);


            // Apply specular and reflection to terrain color
            vec3 specColor = vec3(1.0, 0.98, 0.95) * specular * (1.0 - material_roughness) * shadow_val;
            terrain_color += specColor * 0.3;

            // Fresnel for water
            if (waterMask > 0.01) {
               float n_dot_v = max(0.0, dot(final_normal, viewDir));
               // Schlick's approximation
               float fresnel = 0.02 + 0.98 * pow(1.0 - n_dot_v, 5.0);
               reflectivity = mix(reflectivity, fresnel, waterMask);
               // Removed explicit fog mixing here; let distance fog handle it
               // envColor = mix(envColor, fog_color, waterMask);
            }

            // Apply reflection (remove damping for water)
            float reflectDamp = (waterMask > 0.01) ? 1.0 : 0.3;
            terrain_color = mix(terrain_color, envColor, reflectivity * reflectDamp);


          // === Lighting ===
          float final_l = max(0.0, dot(final_normal, lightDir));

          // Matched to Sky Shader: Horizon -> Zenith
          vec3 sky_color = u_fogColor * 0.8 + u_zenithColor * 0.2;
          vec3 ground_color = vec3(0.1, 0.08, 0.05); // Slightly darker ground bounce
          float sky_factor = final_normal.z * 0.5 + 0.5;
          vec3 ambient = mix(ground_color, sky_color, sky_factor) * 0.5; // Tuned intensity

          vec3 sun_color = vec3(1.0, 0.95, 0.9);
          vec3 direct = sun_color * final_l * shadow_val * 0.5; // Reduced direct intensity slightly to balance
          vec3 lighting = ambient + direct;

          // === AO (unchanged) ===
          float occlusion = texture(ao, reliefCoord).r;
          terrain_color = terrain_color * occlusion;


          // === Fog ===
          float fog_coeff = exp(v_dist * -2e-5);

          vec3 final_color = mix(fog_color, lighting * terrain_color, fog_coeff);

          // Gamma correction
          color = vec4(pow(final_color, vec3(1.0 / 2.2)), 1.0);
        }
      |};
    attributes = [];
  }

let triangle_program =
  {
    vertex_shader =
      {|#version 300 es
        uniform mat4 transform;
        uniform vec4 color;
        out vec4 fragment_color;
        void main() {
          float x = float(gl_VertexID - 1) / 2.;
          float y = float(gl_VertexID != 1) * (sqrt(3.)/ 2.);
          fragment_color = color;
          gl_Position = transform * vec4(x, y, 0, 1.);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        in vec4 fragment_color;
        out vec4 color;
        void main() {
          color = fragment_color;
        }
      |};
    attributes = [];
  }

let text_program =
  {
    vertex_shader =
      {|#version 300 es
        uniform mat4 transform;
        out vec2 texture_coord;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          texture_coord = vec2(x, 1. - y);
          gl_Position = transform * vec4(x, y, 0, 1.);
        }
      |};
    fragment_shader =
      common_fragment_header
      ^ {|
        in vec2 texture_coord;
        uniform sampler2D tex;
        out vec4 color;
        void main() {
          color = texture(tex, texture_coord);
        }
      |};
    attributes = [];
  }

let ao_bake_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader =
      common_fragment_header
      ^ {|
        uniform sampler2D relief;
        uniform int width;
        uniform float scale; // Added scale uniform
        in vec2 uv;
        out float occlusion;

        vec3 get_pos(vec2 coord) {
          float h = decode_height(texture(relief, coord).rg);
          // Reconstruct World Position
          // UV * Width * Scale = World Meters
          return vec3(coord * float(width) * scale, h);
        }

        float IGN(vec2 p) {
          vec3 magic = vec3(0.06711056, 0.00583715, 52.9829189);
          return fract(magic.z * fract(dot(p, magic.xy)));
        }

        void main() {
          const float DIRECTIONS = 8.0;
          const float STEPS = 10.0;

          float R_uv = 50.0 / float(width);
          float R_world = 50. * scale;

          // Center Height
          float h_center = decode_height(texture(relief, uv).rg);

          float noise = IGN(gl_FragCoord.xy);

          float totalOcclusion = 0.0;

          for (float d = 0.0; d < DIRECTIONS; d++) {
             float angle = (d + noise) * (2.0 * PI / DIRECTIONS);
             vec2 dir = vec2(cos(angle), sin(angle));

             // Maximize Tangent (Height Diff / Dist) instead of Angle
             // Initialize to small value (tan(-89deg))
             float max_tan = -100.0;

             for (float s = 1.0; s <= STEPS; s++) {
                float t_linear = (s + 1. + noise) / (STEPS + 2.);
                float sample_t = t_linear * t_linear;
                vec2 sample_uv = uv + dir * sample_t * R_uv;

                float h_sample = decode_height(texture(relief, sample_uv).rg);
                float h_diff = h_sample - h_center;
                float dist = sample_t * R_world;

                float tan_s = h_diff / dist;
                max_tan = max(max_tan, tan_s);
             }

             // Convert max_tan back to sin(horizon_angle)
             float sin_horizon = max_tan / sqrt(1.0 + max_tan * max_tan);

             totalOcclusion += max(0.0, sin_horizon);
          }

          totalOcclusion = totalOcclusion / DIRECTIONS;
          occlusion = 1.0 - totalOcclusion; // Output visibility
        }
      |};
    attributes = [];
  }

let ao_blur_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader =
      common_fragment_header
      ^ {|
        uniform sampler2D ao_tex;
        uniform sampler2D relief;
        uniform vec2 inv_res;
        in vec2 uv;
        out float color;

        void main() {
          float result = 0.0;
          float weight_sum = 0.0;

          // Center height for bilateral comparison
          float h_center = decode_height(texture(relief, uv).rg);

          // Bilateral blur: Gaussian spatial + height similarity
          // Height sigma: ~50m (samples with >50m difference get low weight)
          float h_sigma = 50.0;
          float h_sigma_sq2 = 2.0 * h_sigma * h_sigma;

          // 3x3 Gaussian spatial weights
          float k[9];
          k[0]=1.; k[1]=2.; k[2]=1.;
          k[3]=2.; k[4]=4.; k[5]=2.;
          k[6]=1.; k[7]=2.; k[8]=1.;

          int idx = 0;
          for (int y=-1; y<=1; y++) {
             for (int x=-1; x<=1; x++) {
                vec2 sample_uv = uv + vec2(float(x), float(y)) * inv_res;
                float ao_sample = texture(ao_tex, sample_uv).r;
                float h_sample = decode_height(texture(relief, sample_uv).rg);

                // Height difference weight (bilateral term)
                float h_diff = h_sample - h_center;
                float h_weight = exp(-(h_diff * h_diff) / h_sigma_sq2);

                // Combined weight: spatial * height similarity
                float w = k[idx] * h_weight;
                result += ao_sample * w;
                weight_sum += w;
                idx++;
             }
          }
          color = result / weight_sum;
        }
      |};
    attributes = [];
  }

let shadow_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        precision highp int;
        uniform mat4 shadow_view_proj;|}
      ^ radial_vertex_common
      ^ {|
        void main() {
          RadialVertex rv = computeRadialVertex();
          gl_Position = shadow_view_proj * vec4(rv.coord_meters, rv.height, 1.0);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        void main() { }
      |};
    attributes = [];
  }

(* CLC Rasterization Shader - renders CLC tile triangles to R8UI FBO *)
let clc_raster_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        layout(location = 0) in vec2 in_norm_pos;  // u16 normalized to 0..1
        layout(location = 1) in uint in_color_idx;  // u8 palette index (unsigned)
        uniform vec2 u_tile_range;   // tile extent in degrees (65535/scale_x, 65535/scale_y)
        uniform vec2 u_tile_min;     // tile origin (min_lon, min_lat)
        uniform vec2 u_tex_min;      // DEM min (lon, lat) in degrees
        uniform vec2 u_tex_range;    // DEM extent (lon_range, lat_range) in degrees
        flat out uint v_idx;
        void main() {
          // Map normalized u16 (0..1) to geographic coords
          vec2 geo_pos = in_norm_pos * u_tile_range + u_tile_min;
          // Map geographic coords to NDC [-1, 1] for the output texture
          vec2 ndc = ((geo_pos - u_tex_min) / u_tex_range) * 2.0 - 1.0;
          gl_Position = vec4(ndc, 0.0, 1.0);
          v_idx = in_color_idx;
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision mediump float;
        flat in uint v_idx;
        out uvec4 out_color;
        void main() {
          out_color = uvec4(v_idx, 0u, 0u, 1u);
        }
      |};
    attributes = [ "in_norm_pos"; "in_color_idx" ];
  }

let water_raster_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        layout(location = 0) in ivec2 in_pos;  // 24-bit quantized int32
        layout(location = 1) in uint in_color_idx;  // u8 palette index
        uniform vec2 u_tile_range;   // tile extent in degrees
        uniform float u_water_scale; // quantization scale (220000.0)
        uniform vec2 u_tile_min;     // tile origin
        uniform vec2 u_tex_min;      // DEM min
        uniform vec2 u_tex_range;    // DEM extent
        flat out uint v_idx;
        void main() {
          // Un-quantize and scale to degrees
          // in_pos is 0..220000 mapping to u_tile_range
          vec2 norm = vec2(in_pos) / u_water_scale;
          vec2 geo_pos = norm * u_tile_range + u_tile_min;

          vec2 ndc = ((geo_pos - u_tex_min) / u_tex_range) * 2.0 - 1.0;
          gl_Position = vec4(ndc, 0.0, 1.0);
          v_idx = in_color_idx;
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision mediump float;
        flat in uint v_idx;
        out uvec4 out_color;
        void main() {
          out_color = uvec4(v_idx, 0u, 0u, 1u);
        }
      |};
    attributes = [ "in_pos"; "in_color_idx" ];
  }

let sky_program =
  {
    vertex_shader =
      {|#version 300 es
    out mediump vec2 v_uv;
    void main() {
      float x = float(gl_VertexID & 1);
      float y = float(gl_VertexID >> 1);
      v_uv = vec2(x, y);
      // Draw at Far Plane (Z=1.0)
      gl_Position = vec4(2.0 * x - 1.0, 2.0 * y - 1.0, 1.0, 1.0);
    }
  |};
    fragment_shader =
      {|#version 300 es
        precision mediump float;
        uniform mat4 inv_view;
        uniform vec3 u_lightDir;
        uniform vec3 u_fogColor;
        uniform vec3 u_zenithColor;
        uniform vec2 sky_params; // x_scale, y_scale
        in vec2 v_uv;
        out vec4 color;

        void main() {
          // Reconstruct View Ray in View Space
          // Clip space: (x, y, -1.0) for forward direction (RH)
          // View Ray = (clip.x / x_scale, clip.y / y_scale, -1.0)

          float x = (v_uv.x * 2.0 - 1.0) / sky_params.x;
          float y = (v_uv.y * 2.0 - 1.0) / sky_params.y;
          vec3 view_ray = normalize(vec3(x, y, -1.0));

          // Transform to World Space (Rotation only)
          highp vec3 view_dir = mat3(inv_view) * view_ray;
          view_dir = normalize(view_dir);

          float cos_theta = view_dir.z;
          highp float cos_gamma = dot(view_dir, u_lightDir);

          // Deep blue zenith, lighter horizon (Linear Space)
          // New Lighter Blue/White Horizon (Matches fog)
          vec3 horizon = u_fogColor;

          float horizon_factor = smoothstep(0.0, 0.35, cos_theta);
          vec3 sky_base = mix(horizon, u_zenithColor, horizon_factor);

          float mie = pow(max(0.0, cos_gamma), 400.0) * 0.8;
          float halo = pow(max(0.0, cos_gamma), 20.0) * 0.2;

          vec3 sun_color = vec3(1.0, 0.9, 0.7);
          vec3 sky = sky_base + sun_color * (mie + halo);

          if (cos_gamma > 0.9995) {
             sky = vec3(1.0, 0.95, 0.8) * 20.0;
          }

          // Dither to prevent banding
          float noise = fract(sin(dot(gl_FragCoord.xy, vec2(12.9898, 78.233))) * 43758.5453);
          sky += (noise - 0.5) / 255.0;

          // Gamma Correction (Linear -> sRGB)
          color = vec4(pow(sky, vec3(1.0 / 2.2)), 1.0);
        }
      |};
    attributes = [];
  }

let mipmap_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform sampler2D source_texture;
        uniform vec2 source_size;
        uniform float k;
        in vec2 uv;
        out vec4 frag_color;
        void main() {
          vec2 size = source_size;
          // Map UV to Source Pixel Coordinates
          // uv points to center of the 2x2 block in source
          ivec2 p = ivec2(uv * size);

          // Force alignment to even coordinates (top-left of 2x2 block)
          ivec2 p00 = (p / 2) * 2;

          ivec2 c00 = clamp(p00, ivec2(0), ivec2(size) - 1);
          ivec2 c10 = clamp(p00 + ivec2(1, 0), ivec2(0), ivec2(size) - 1);
          ivec2 c01 = clamp(p00 + ivec2(0, 1), ivec2(0), ivec2(size) - 1);
          ivec2 c11 = clamp(p00 + ivec2(1, 1), ivec2(0), ivec2(size) - 1);

          vec4 h00_v = texelFetch(source_texture, c00, 0);
          vec4 h10_v = texelFetch(source_texture, c10, 0);
          vec4 h01_v = texelFetch(source_texture, c01, 0);
          vec4 h11_v = texelFetch(source_texture, c11, 0);

          float h00 = h00_v.r + h00_v.g / 256.0;
          float h10 = h10_v.r + h10_v.g / 256.0;
          float h01 = h01_v.r + h01_v.g / 256.0;
          float h11 = h11_v.r + h11_v.g / 256.0;

          float max_h = max(max(h00, h10), max(h01, h11));
          float h_scale = 10000.0;

          float w00 = exp(k * (h00 - max_h) * h_scale);
          float w10 = exp(k * (h10 - max_h) * h_scale);
          float w01 = exp(k * (h01 - max_h) * h_scale);
          float w11 = exp(k * (h11 - max_h) * h_scale);

          float sum_w = w00 + w10 + w01 + w11;
          float h_avg = (h00 * w00 + h10 * w10 + h01 * w01 + h11 * w11) / sum_w;
          vec2 n_avg = (h00_v.ba + h10_v.ba + h01_v.ba + h11_v.ba) * 0.25;

          float r = floor(h_avg * 255.0) / 255.0;
          float g = (h_avg - r) * 256.0;

          frag_color = vec4(r, g, n_avg);
        }|};
    attributes = [];
  }

let copy_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform sampler2D source;
        uniform int level;
        uniform vec2 source_size;
        in vec2 uv;
        out vec4 color;
        void main() {
          // Robust 1:1 Copy using UVs + Explicit Level
          // Works now that MinFilter is Mipmap-compatible
          ivec2 p = ivec2(uv * source_size);
          p = clamp(p, ivec2(0), ivec2(source_size) - 1);
          color = texelFetch(source, p, level);
        }|};
    attributes = [];
  }

let normal_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader =
      common_fragment_header
      ^ {|
        uniform vec2 size;
        uniform vec2 delta;
        uniform sampler2D tile;
        in vec2 uv;
        out mediump vec4 color;

        float get_z(vec2 offset) {
            vec2 tileCoord = uv * (size - 1.0) + 0.5;
            // Decode from RG8: R=low byte, G=high byte (little-endian)
            // Samples are in [0, 1], need to multiply by 255 to get 0..255
            vec2 rg = texture(tile, (tileCoord + offset) / size).rg * 255.0;
            float h_val = rg.g * 256.0 + rg.r;
            // Convert back to meters: u16 range maps to -500 to 9000
            return h_val * (9500.0 / 65535.0) - 500.0;
        }

        void main() {
          // Sobel filter
          float tl = get_z(vec2(-1, -1));
          float t  = get_z(vec2( 0, -1));
          float tr = get_z(vec2( 1, -1));
          float l  = get_z(vec2(-1,  0));
          float c  = get_z(vec2( 0,  0));
          float r  = get_z(vec2( 1,  0));
          float bl = get_z(vec2(-1,  1));
          float b  = get_z(vec2( 0,  1));
          float br = get_z(vec2( 1,  1));

          float dX = tr + 2.0*r + br - (tl + 2.0*l + bl);
          float dY = bl + 2.0*b + br - (tl + 2.0*t + tr);

          // Normal vector
          // Note: dX is dHeight/dPixelX * 8 (scaling of Sobel).
          // We divide by (8 * deltax) to get slope.
          vec3 n = normalize(vec3(-dX / (8.0 * delta.x), -dY / (8.0 * delta.y), 1.0));

          // Encode Normal (xy components to [0,1])
          vec2 encN = n.xy * 0.5 + 0.5;

          // Encode Height (-500 to 9000 -> 0 to 1)
          float h_norm = clamp((c - (-500.0)) / 9500.0, 0.0, 1.0);
          float h_val = floor(h_norm * 65535.0 + 0.5);
          float h_high = floor(h_val / 256.0) / 255.0;
          float h_low = floor(mod(h_val, 256.0)) / 255.0;

          color = vec4(h_high, h_low, encN.x, encN.y);
        }
      |};
    attributes = [];
  }
(* Graphics Resources & Setup *)

module Gl = Brr_canvas.Gl

let build_indices w w' h =
  let t = Unix.gettimeofday () in
  let block_size = 32 in
  let rec count_indices total jb =
    if jb >= w - 1 then total
    else
      let je = min (jb + block_size) (w - 1) in
      let num_strips = h - 1 in
      let indices_per_strip = ((je - jb + 1) * 2) + 1 in
      count_indices (total + (num_strips * indices_per_strip)) (jb + block_size)
  in
  let total_size = count_indices 0 0 in
  let is = Bigarray.(Array1.create Bigarray.int32 c_layout total_size) in
  let idx = ref 0 in
  let rec fill_indices jb =
    if jb < w - 1 then (
      let je = min (jb + block_size) (w - 1) in
      for i = 0 to h - 2 do
        for j = jb to je do
          is.{!idx} <- Int32.of_int (j + (i * w'));
          incr idx;
          is.{!idx} <- Int32.of_int (j + ((i + 1) * w'));
          incr idx
        done;
        is.{!idx} <- Int32.of_int (-1);
        incr idx
      done;
      fill_indices (jb + block_size))
  in
  fill_indices 0;
  Format.eprintf "BUILD INDICES %f (size %d)@."
    (Unix.gettimeofday () -. t)
    total_size;
  is

let make_tile_texture ctx tile =
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  (* Input is RG8 format: 2 bytes per pixel (high, low) *)
  let height = Bigarray.Array2.dim1 tile.Dem_loader.data in
  let width = Bigarray.Array2.dim2 tile.Dem_loader.data / 2 in
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rg8 width height 0 Gl.rg
    Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array2 tile.Dem_loader.data))
    0;
  Web_utils.set_texture_params_nearest_clamp ctx Gl.texture_2d;
  Gl.bind_texture ctx Gl.texture_2d None;
  tid

(* Anisotropic filtering support *)
let aniso_ext = ref None
let max_anisotropy = ref 1.0

let init_anisotropic_filtering ctx =
  let ext = Gl.get_extension ctx (Jstr.v "EXT_texture_filter_anisotropic") in
  if Jv.is_some ext then begin
    aniso_ext := Some ext;
    (* MAX_TEXTURE_MAX_ANISOTROPY_EXT = 0x84FF *)
    let max_val = Jv.to_float (Gl.get_parameter ctx 0x84FF) in
    max_anisotropy := max_val;
    Format.eprintf "Anisotropic filtering enabled: max %.0fx@." max_val
  end
  else Format.eprintf "Anisotropic filtering not supported@."

let apply_anisotropic_filtering ctx =
  if !max_anisotropy > 1.0 then
    (* TEXTURE_MAX_ANISOTROPY_EXT = 0x84FE *)
    Gl.tex_parameterf ctx Gl.texture_2d 0x84FE !max_anisotropy

(* Detect supported compressed texture format *)
type compressed_format = BC7 | ASTC | ETC2

let detect_compressed_format ctx =
  let has_ext name = Jv.is_some (Gl.get_extension ctx (Jstr.v name)) in
  (* BC7 first for best quality on desktop *)
  if has_ext "EXT_texture_compression_bptc" then Some BC7
  else if has_ext "WEBGL_compressed_texture_astc" then Some ASTC
  else if has_ext "WEBGL_compressed_texture_etc" then Some ETC2
  else None

(* GL internal format for compressed texture *)
let compressed_internal_format = function
  | BC7 -> 0x8E8C (* COMPRESSED_RGBA_BPTC_UNORM_EXT *)
  | ASTC -> 0x93B0 (* COMPRESSED_RGBA_ASTC_4x4_KHR *)
  | ETC2 -> 0x9278 (* COMPRESSED_RGBA8_ETC2_EAC *)

(* KTX2 file for each format *)
let compressed_texture_file = function
  | BC7 -> "assets/details_bc7.ktx2"
  | ASTC -> "assets/details_astc.ktx2"
  | ETC2 -> "assets/details_etc2.ktx2"

let force_redraw = ref true

(* Load compressed KTX2 texture asynchronously *)
let load_compressed_detail_map ctx tid =
  match detect_compressed_format ctx with
  | None -> Format.eprintf "No compressed texture format supported@."
  | Some fmt ->
      let file = compressed_texture_file fmt in
      let internal_fmt = compressed_internal_format fmt in
      let fetch =
        let open Fut.Result_syntax in
        let* response = Brr_io.Fetch.url (Jstr.v file) in
        let* buffer =
          Brr_io.Fetch.Body.array_buffer
            (Brr_io.Fetch.Response.as_body response)
        in
        Fut.return (Ok buffer)
      in
      Fut.await fetch (function
        | Error e ->
            Format.eprintf "Failed to load %s: %s@." file
              (Jv.Error.message e |> Jstr.to_string)
        | Ok buffer ->
            (* Parse KTX2 header using DataView *)
            let view =
              Jv.new'
                (Jv.get Jv.global "DataView")
                [| Brr.Tarray.Buffer.to_jv buffer |]
            in
            let get_u32 off =
              Jv.to_int (Jv.call view "getUint32" [| Jv.of_int off; Jv.true' |])
            in
            (* Header fields at known offsets:
               12: vkFormat, 16: typeSize, 20: pixelWidth, 24: pixelHeight,
               28: pixelDepth, 32: layerCount, 36: faceCount, 40: levelCount *)
            let pixel_width = get_u32 20 in
            let pixel_height = get_u32 24 in
            let level_count = get_u32 40 in
            (* Level index starts at offset 80, each entry is 24 bytes (3 x uint64) *)
            let get_u64_low off =
              (* Just read low 32 bits - file offsets won't exceed 4GB *)
              Jv.to_int (Jv.call view "getUint32" [| Jv.of_int off; Jv.true' |])
            in
            (* Upload each mip level - Level Index is in GL order: [0]=largest, [n-1]=smallest *)
            Gl.active_texture ctx Gl.texture5;
            Gl.bind_texture ctx Gl.texture_2d (Some tid);
            let data = Brr.Tarray.of_buffer Brr.Tarray.Uint8 buffer in
            for level = 0 to level_count - 1 do
              let idx = 80 + (level * 24) in
              let offset = get_u64_low idx in
              let length = get_u64_low (idx + 8) in
              let level_data =
                Brr.Tarray.sub data ~start:offset ~stop:(offset + length)
              in
              let w = max 1 (pixel_width lsr level) in
              let h = max 1 (pixel_height lsr level) in
              Gl.compressed_tex_image2d ctx Gl.texture_2d level internal_fmt w h
                0 level_data
            done;
            Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
              Gl.linear_mipmap_linear;
            Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
            Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.repeat;
            Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.repeat;
            apply_anisotropic_filtering ctx;
            Gl.active_texture ctx Gl.texture0;
            force_redraw := true;
            Format.eprintf "Loaded compressed texture %s (%dx%d, %d levels)@."
              file pixel_width pixel_height level_count)

(* Create 1x1 placeholder detail map texture (grey midtone in all channels) *)
let make_detail_map ctx =
  let data = Bigarray.(Array1.create int8_unsigned c_layout 4) in
  data.{0} <- 128;
  data.{1} <- 128;
  data.{2} <- 128;
  data.{3} <- 128;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rgba8 1 1 0 Gl.rgba Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 data))
    0;
  Web_utils.set_texture_params_mipmap_repeat ctx Gl.texture_2d;
  Gl.bind_texture ctx Gl.texture_2d None;
  (* Start async load of compressed texture *)
  load_compressed_detail_map ctx tid;
  tid

(* Create CLC palette texture (128x1 RGBA, 2 pixels per material) *)
let make_palette_texture ctx =
  let data = Clc_palette.generate_palette () in
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rgba8 128 1 0 Gl.rgba Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 data))
    0;
  Web_utils.set_texture_params_nearest_clamp ctx Gl.texture_2d;
  Gl.bind_texture ctx Gl.texture_2d None;
  tid

let create_shadow_map ctx width height layers =
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d_array (Some tid);
  Gl.tex_storage3d ctx Gl.texture_2d_array 1 Gl.depth_component24 width height
    layers;

  (* Linear filter for smooth shadow edges with hardware comparison *)
  Web_utils.set_texture_params_linear_clamp ctx Gl.texture_2d_array;

  (* Enable hardware shadow comparison for sampler2DArrayShadow *)
  Gl.tex_parameteri ctx Gl.texture_2d_array Gl.texture_compare_mode
    Gl.compare_ref_to_texture;
  Gl.tex_parameteri ctx Gl.texture_2d_array Gl.texture_compare_func Gl.lequal;
  Gl.bind_texture ctx Gl.texture_2d_array None;
  tid

let create_shadow_fbo ctx shadow_map =
  (* Depth Only FBO *)
  let fbo = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some fbo);
  Gl.framebuffer_texture_layer ctx Gl.framebuffer Gl.depth_attachment shadow_map
    0 0;

  (* No Color Attachment *)
  Gl.draw_buffers ctx [ Gl.none ];

  Gl.bind_framebuffer ctx Gl.framebuffer None;
  fbo

let calculate_shadow_matrices ~light_dir ~world_center =
  let matrices = Array.make 3 (Array.make 16 0.) in

  (* Simple Cascades based on Splits *)
  for i = 0 to 2 do
    (* Shadow map radius: 1.5x the split distance for margin *)
    (* Larger cascades for coarse 20-30m grid cells *)
    let split_radius =
      if i = 0 then 2000.0 else if i = 1 then 8000.0 else 25000.0
    in
    let shadow_radius = split_radius *. 1.5 in

    (* Use world_center with z=0 (terrain uses absolute heights) *)
    let center = world_center in

    (* Ortho Proj: scale depth range with shadow radius *)
    let depth_range = max 10000. (shadow_radius *. 2.) in
    let p =
      Matrix.ortho ~left:(-.shadow_radius) ~right:shadow_radius
        ~bottom:(-.shadow_radius) ~top:shadow_radius ~near:(-.depth_range)
        ~far:depth_range
    in

    (* Eye position along light direction from center *)
    let look_target =
      Matrix.
        {
          x = center.x +. (light_dir.x *. shadow_radius);
          y = center.y +. (light_dir.y *. shadow_radius);
          z = center.z +. (light_dir.z *. shadow_radius);
          w = 1.;
        }
    in

    let view =
      Matrix.look_at ~eye:look_target ~center
        ~up:Matrix.{ x = 0.; y = 1.; z = 0.; w = 0. }
    in

    matrices.(i) <- Matrix.(view * p)
  done;

  matrices

type graphics_resources = {
  terrain_geo : Gl.vertex_array_object;
  indices : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  text_geo : Gl.vertex_array_object;
  triangle_geo : Gl.vertex_array_object;
  terrain_pid : Gl.program;
  triangle_pid : Gl.program;
  text_pid : Gl.program;
  shadow_pid : Gl.program;
  sky_uniforms : Render_state.sky_uniforms;
  terrain_uniforms : Render_state.terrain_uniforms;
  triangle_uniforms : Render_state.triangle_uniforms;
  text_uniforms : Render_state.text_uniforms;
  shadow_map : Gl.texture;
  shadow_fbo : Gl.framebuffer;
  shadow_uniforms : Render_state.shadow_uniforms;
  sky_pid : Gl.program;
  normal_pid : Gl.program;
  mipmap_pid : Gl.program;
  copy_pid : Gl.program;
  ao_bake_pid : Gl.program;
  ao_blur_pid : Gl.program;
  relief_uniforms : Render_state.relief_uniforms;
  mipmap_uniforms : Render_state.mipmap_uniforms;
  copy_uniforms : Render_state.copy_uniforms;
  ao_bake_uniforms : Render_state.ao_bake_uniforms;
  ao_blur_uniforms : Render_state.ao_blur_uniforms;
  clc_raster_pid : Gl.program;
  water_raster_pid : Gl.program;
  clc_raster_uniforms : Render_state.clc_raster_uniforms;
  water_raster_uniforms : Render_state.water_raster_uniforms;
  radial_params : Render_state.radial_params;
  nearest_sampler : Gl.sampler;
}

let resize_canvas ?device_width ?device_height canvas =
  let canvas_width, canvas_height =
    match (device_width, device_height) with
    | Some w, Some h -> (w, h)
    | _ ->
        (* Fallback: use devicePixelRatio *)
        let dpr = Brr.Window.device_pixel_ratio Brr.G.window in
        ( truncate (Brr.El.inner_w canvas *. dpr),
          truncate (Brr.El.inner_h canvas *. dpr) )
  in
  let canvas = Brr_canvas.Canvas.of_el canvas in
  if Brr_canvas.Canvas.w canvas <> canvas_width then
    Brr_canvas.Canvas.set_w canvas canvas_width;
  if Brr_canvas.Canvas.h canvas <> canvas_height then
    Brr_canvas.Canvas.set_h canvas canvas_height

let init_graphics ctx =
  (* Initialize GPU timer extension *)
  Gpu_timer.init ctx;
  (* Initialize anisotropic filtering extension *)
  let triangle_pid = Web_utils.create_program ctx triangle_program in
  let text_pid = Web_utils.create_program ctx text_program in
  let shadow_pid = Web_utils.create_program ctx shadow_program in
  let sky_pid = Web_utils.create_program ctx sky_program in
  let normal_pid = Web_utils.create_program ctx normal_program in
  let mipmap_pid = Web_utils.create_program ctx mipmap_program in
  let copy_pid = Web_utils.create_program ctx copy_program in
  let ao_bake_pid = Web_utils.create_program ctx ao_bake_program in
  let ao_blur_pid = Web_utils.create_program ctx ao_blur_program in
  let terrain_pid = Web_utils.create_program ctx terrain_program in

  let shadow_map = create_shadow_map ctx 2048 2048 3 in
  let shadow_fbo = create_shadow_fbo ctx shadow_map in
  let sky_uniforms = Render_state.init_sky_uniforms ctx sky_pid in

  (* Initialize render state - cache uniform locations and pre-compute params *)
  let radial_params = Render_state.compute_radial_params ~n_sectors ~n_rings in
  let terrain_uniforms = Render_state.init_terrain_uniforms ctx terrain_pid in
  let triangle_uniforms =
    Render_state.init_triangle_uniforms ctx triangle_pid
  in
  let text_uniforms = Render_state.init_text_uniforms ctx text_pid in
  let shadow_uniforms = Render_state.init_shadow_uniforms ctx shadow_pid in

  let terrain_geo, indices =
    let sectors = n_sectors + 1 in
    let rings = n_rings in
    let w' = next_power_of_two sectors 1 in
    let indices = build_indices sectors w' rings in
    (* Add dummy buffer to ensure VAO has at least one attribute - fixes 'Index buffer not bound' on some drivers *)
    let dummy_data = Bigarray.(Array1.create float32 c_layout 4) in
    let buffers = [ (0, 1, Gl.float, Web_utils.Buffer dummy_data) ] in
    (Web_utils.create_geometry ctx ~indices ~buffers, indices)
  in
  let text_geo =
    Web_utils.create_geometry ctx
      ~indices:(Bigarray.(Array1.init int8_unsigned c_layout) 4 (fun i -> i))
      ~buffers:[]
  in
  let triangle_geo =
    let indices =
      Bigarray.(Array1.of_array int8_unsigned c_layout [| 0; 1; 2; 1; 3; 2 |])
    in
    let positions =
      let b = Bigarray.(Array1.create float32 c_layout 12) in
      b.{0} <- -1.;
      b.{1} <- 1.;
      b.{2} <- 0.;
      b.{3} <- 1.;
      b.{4} <- 1.;
      b.{5} <- 0.;
      b.{6} <- -1.;
      b.{7} <- -1.;
      b.{8} <- 0.;
      b.{9} <- 1.;
      b.{10} <- -1.;
      b.{11} <- 0.;
      Web_utils.Buffer b
    in
    let buffers = [ (0, 3, Gl.float, positions) ] in
    Web_utils.create_geometry ctx ~indices ~buffers
  in

  (* Upload static uniforms once at initialization *)
  Gl.use_program ctx terrain_pid;

  Render_state.upload_radial_static ctx terrain_uniforms radial_params;
  Render_state.upload_texture_units ctx terrain_uniforms;

  Gl.use_program ctx shadow_pid;
  Render_state.upload_radial_static_shadow ctx shadow_uniforms radial_params;
  Render_state.upload_texture_units_shadow ctx shadow_uniforms;

  let relief_uniforms = Render_state.init_relief_uniforms ctx normal_pid in
  let mipmap_uniforms = Render_state.init_mipmap_uniforms ctx mipmap_pid in
  let copy_uniforms = Render_state.init_copy_uniforms ctx copy_pid in
  let ao_bake_uniforms = Render_state.init_ao_bake_uniforms ctx ao_bake_pid in
  let ao_blur_uniforms = Render_state.init_ao_blur_uniforms ctx ao_blur_pid in
  let clc_raster_pid = Web_utils.create_program ctx clc_raster_program in
  let clc_raster_uniforms =
    Render_state.init_clc_raster_uniforms ctx clc_raster_pid
  in
  let water_raster_pid = Web_utils.create_program ctx water_raster_program in
  let water_raster_uniforms =
    Render_state.init_water_raster_uniforms ctx water_raster_pid
  in

  (* Create nearest sampler for AO passes (relief texture needs nearest filtering there) *)
  let nearest_sampler = Gl.create_sampler ctx in
  Gl.sampler_parameteri ctx nearest_sampler Gl.texture_min_filter Gl.nearest;
  Gl.sampler_parameteri ctx nearest_sampler Gl.texture_mag_filter Gl.nearest;
  Gl.sampler_parameteri ctx nearest_sampler Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.sampler_parameteri ctx nearest_sampler Gl.texture_wrap_t Gl.clamp_to_edge;

  {
    terrain_geo;
    indices;
    text_geo;
    triangle_geo;
    terrain_pid;
    triangle_pid;
    text_pid;
    shadow_pid;
    sky_pid;
    sky_uniforms;
    terrain_uniforms;
    triangle_uniforms;
    text_uniforms;
    shadow_map;
    shadow_fbo;
    shadow_uniforms;
    normal_pid;
    mipmap_pid;
    copy_pid;
    relief_uniforms;
    mipmap_uniforms;
    copy_uniforms;
    ao_bake_pid;
    ao_blur_pid;
    ao_bake_uniforms;
    ao_blur_uniforms;
    clc_raster_pid;
    water_raster_pid;
    clc_raster_uniforms;
    water_raster_uniforms;
    radial_params;
    nearest_sampler;
  }
(* Rendering Passes *)

let compute_ao ctx width height scale relief_texture nearest_sampler ao_bake_pid
    ao_blur_pid (bake_u : Render_state.ao_bake_uniforms)
    (blur_u : Render_state.ao_blur_uniforms) =
  (* Helper to create FBO and R8 Texture *)
  let create_r8_target w h =
    let tid = Gl.create_texture ctx in
    Gl.bind_texture ctx Gl.texture_2d (Some tid);
    Gl.tex_storage2d ctx Gl.texture_2d 1 Gl.r8 w h;
    Web_utils.set_texture_params_linear_clamp ctx Gl.texture_2d;
    tid
  in

  (* Use Half Resolution for AO *)
  let ao_width = width / 2 in
  let ao_height = height / 2 in

  let ao_bake_tex = create_r8_target ao_width ao_height in
  let ao_final_tex = create_r8_target ao_width ao_height in

  let fbo = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some fbo);

  (* PASS 1: Bake AO *)
  Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0 Gl.texture_2d
    ao_bake_tex 0;

  Gl.viewport ctx 0 0 ao_width ao_height;
  Gl.use_program ctx ao_bake_pid;

  Gl.uniform1i ctx bake_u.relief 0;
  (* Keep generating noise/radius based on FULL width for consistent world scale *)
  Gl.uniform1i ctx bake_u.width width;
  Gl.uniform1f ctx bake_u.scale scale;

  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);
  (* Use nearest sampler for AO - overrides texture's mipmap filter *)
  Gl.bind_sampler ctx 0 (Some nearest_sampler);

  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  (* PASS 2: Blur AO with bilateral filter *)
  Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0 Gl.texture_2d
    ao_final_tex 0;

  Gl.use_program ctx ao_blur_pid;

  Gl.uniform1i ctx blur_u.ao_tex 0;
  Gl.uniform1i ctx blur_u.relief 1;
  (* Step size relative to LOW res texture *)
  Gl.uniform2f ctx blur_u.inv_res
    (1.0 /. float ao_width)
    (1.0 /. float ao_height);

  (* Bind AO bake texture on unit 0 - no sampler needed (not mipmapped) *)
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some ao_bake_tex);
  Gl.bind_sampler ctx 0 None;

  (* Bind relief texture on unit 1 with nearest sampler for bilateral comparison *)
  Gl.active_texture ctx Gl.texture1;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);
  Gl.bind_sampler ctx 1 (Some nearest_sampler);

  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  (* Cleanup *)
  Gl.delete_framebuffer ctx fbo;
  Gl.delete_texture ctx ao_bake_tex;
  Gl.bind_sampler ctx 0 None;
  Gl.bind_sampler ctx 1 None;

  (* Restore State *)
  Gl.bind_framebuffer ctx Gl.framebuffer None;
  Gl.bind_texture ctx Gl.texture_2d None;

  ao_final_tex

let compute_relief ctx width height lat triangle_geo tile_texture normal_pid
    mipmap_pid copy_pid (u : Render_state.relief_uniforms)
    (mipmap_u : Render_state.mipmap_uniforms)
    (copy_u : Render_state.copy_uniforms) =
  assert (width = height);

  (* Not used in shader explicitly yet, using pow directly *)
  let max_level = Web_utils.log2 (max width height) in
  let levels = max_level + 1 in

  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_base_level 0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_max_level (levels - 1);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  apply_anisotropic_filtering ctx;

  (* Use RGBA8 (4 bytes per pixel) *)
  Gl.tex_storage2d ctx Gl.texture_2d levels Gl.rgba8 width height;

  let fb = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some fb);
  let attachmentPoint = Gl.color_attachment0 in
  Gl.framebuffer_texture2d ctx Gl.framebuffer attachmentPoint Gl.texture_2d tid
    0;
  Gl.viewport ctx 0 0 width height;

  (* Clear relief texture to zero first *)
  Gl.clear_color ctx 0. 0. 0. 0.;
  Gl.clear ctx Gl.color_buffer_bit;

  (* Use Scissor to leave 1-pixel border zero *)
  Gl.enable ctx Gl.scissor_test;
  Gl.scissor ctx 1 1 (width - 2) (height - 2);

  Gl.use_program ctx normal_pid;
  (* Draw normals (Level 0) *)
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some tile_texture);

  Gl.bind_vertex_array ctx (Some triangle_geo);
  Gl.uniform2f ctx u.size (float width) (float height);

  (* Use the provided latitude for normals *)
  let deltax, deltay, _ = Render_state.compute_deltas ~lat in
  Gl.uniform2f ctx u.delta deltax deltay;

  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  Gl.disable ctx Gl.scissor_test;

  Gl.bind_texture ctx Gl.texture_2d (Some tid);

  Gl.bind_vertex_array ctx (Some triangle_geo);

  (* Common Uniforms *)
  Gl.use_program ctx mipmap_pid;
  Gl.uniform1i ctx mipmap_u.source_texture 0;
  Gl.use_program ctx copy_pid;
  Gl.uniform1i ctx copy_u.source 0;

  (* Start from level 1 *)

  (* Temporary texture for ping-ponging to avoid feedback loop *)
  let temp_tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some temp_tid);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  (* Allocate initial temp storage ONCE (full size) *)
  Gl.tex_storage2d ctx Gl.texture_2d 1 Gl.rgba8 width height;
  Gl.bind_texture ctx Gl.texture_2d None;

  let rec loop level w h =
    if level > max_level || w < 1 || h < 1 then ()
    else (
      (* 1. Copy Source (Level N-1) to Temp Texture *)

      (* Bind FBO to Temp Texture *)
      Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0
        Gl.texture_2d temp_tid 0;

      (* Bind Source: tid *)
      Gl.bind_texture ctx Gl.texture_2d (Some tid);

      (* Use Copy Program *)
      Gl.use_program ctx copy_pid;
      Gl.uniform1i ctx copy_u.level (level - 1);
      (* Source size is previous level size *)
      Gl.uniform2f ctx copy_u.source_size (float (w * 2)) (float (h * 2));

      Gl.viewport ctx 0 0 (w * 2) (h * 2);
      Gl.draw_elements ctx Gl.triangles 6 Gl.unsigned_byte 0;

      (* 2. Downsample Temp(0) -> tid(N) *)

      (* Bind FBO to Dest: tid(N) *)
      Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0
        Gl.texture_2d tid level;

      (* Bind Source: Temp *)
      Gl.bind_texture ctx Gl.texture_2d (Some temp_tid);

      (* Use Mipmap Program *)
      Gl.use_program ctx mipmap_pid;
      Gl.uniform1f ctx mipmap_u.k (0.1 *. (0.2 ** float (level - 1)));

      let source_w = float (w * 2) in
      let source_h = float (h * 2) in
      Gl.uniform2f ctx mipmap_u.source_size source_w source_h;

      (* Temp is Level 0 *)
      Gl.viewport ctx 0 0 w h;

      (* Clear Mip Level to Zero *)
      Gl.clear_color ctx 0. 0. 0. 0.;
      Gl.clear ctx Gl.color_buffer_bit;

      if w > 2 && h > 2 then (
        (* Enable scissor to render only inner area, leaving 1-pixel border *)
        Gl.enable ctx Gl.scissor_test;
        Gl.scissor ctx 1 1 (w - 2) (h - 2);

        Gl.draw_elements ctx Gl.triangles 6 Gl.unsigned_byte 0;

        Gl.disable ctx Gl.scissor_test);

      loop (level + 1) (w / 2) (h / 2))
  in
  loop 1 (width / 2) (height / 2);

  Gl.delete_texture ctx temp_tid;

  (* Restore Framebuffer *)
  Gl.bind_framebuffer ctx Gl.framebuffer None;

  Gl.bind_vertex_array ctx None;

  tid

let rasterize_clc_tiles ctx ~lat ~lon ~clc_tiles ~clc_raster_pid
    ~water_raster_pid (clc_u : Render_state.clc_raster_uniforms)
    (water_u : Render_state.water_raster_uniforms) =
  (* CLC GPU Rasterization setup *)
  let cover_map_size = 1024 in

  (* Create FBO for CLC rasterization *)
  let clc_fbo = Gl.create_framebuffer ctx in

  (* Create R8UI texture array for CLC output (7 levels) *)
  let clc_levels = 7 in
  let cover_map_texture = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d_array (Some cover_map_texture);
  Web_utils.set_texture_params_nearest_clamp ctx Gl.texture_2d_array;
  (* Initialize with grass (index 26 = Natural grasslands) *)
  let init_data =
    Bigarray.(
      Array1.create int8_unsigned c_layout
        (cover_map_size * cover_map_size * clc_levels))
  in
  Bigarray.Array1.fill init_data 26;
  Gl.tex_image3d ctx Gl.texture_2d_array 0 Gl.r8ui cover_map_size cover_map_size
    clc_levels 0 Gl.red_integer Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 init_data))
    0;

  (* Create depth buffer for overdraw prevention (smaller features drawn first win) *)
  (* Separate depth buffer for each level to prevent conflicts when batching tiles *)
  let clc_depth_rbs =
    Array.init 7 (fun _ ->
        let rb = Gl.create_renderbuffer ctx in
        Gl.bind_renderbuffer ctx Gl.renderbuffer (Some rb);
        Gl.renderbuffer_storage ctx Gl.renderbuffer Gl.depth_component16
          cover_map_size cover_map_size;
        rb)
  in

  (* Attach to FBO (attach layer 0 initially) *)
  Gl.bind_framebuffer ctx Gl.framebuffer (Some clc_fbo);
  Gl.framebuffer_texture_layer ctx Gl.framebuffer Gl.color_attachment0
    cover_map_texture 0 0;
  Gl.framebuffer_renderbuffer ctx Gl.framebuffer Gl.depth_attachment
    Gl.renderbuffer clc_depth_rbs.(0);
  Gl.bind_texture ctx Gl.texture_2d None;

  (* Prepare FBO *)
  Gl.viewport ctx 0 0 cover_map_size cover_map_size;
  Gl.use_program ctx clc_raster_pid;

  (* Reusable VAO and Buffers *)
  let vao = Gl.create_vertex_array ctx in
  Gl.bind_vertex_array ctx (Some vao);

  let vbo_pos = Gl.create_buffer ctx in
  let vbo_col = Gl.create_buffer ctx in
  let ebo = Gl.create_buffer ctx in

  (* Conversion factors *)
  let deltax, deltay, _ = Render_state.compute_deltas ~lat in
  let meters_per_deg_lat = deltay *. 3600. in
  let meters_per_deg_lon = deltax *. 3600. in

  (* Pre-calculate view bounds for each level *)
  let level_bounds =
    Array.init 7 (fun level ->
        let extent_meters = 2048.0 *. (2.0 ** float level) in
        let extent_lat = extent_meters /. meters_per_deg_lat in
        let extent_lon = extent_meters /. meters_per_deg_lon in
        let min_lat = lat -. (extent_lat /. 2.) in
        let min_lon = lon -. (extent_lon /. 2.) in
        let max_lat = min_lat +. extent_lat in
        let max_lon = min_lon +. extent_lon in
        (min_lon, min_lat, extent_lon, extent_lat, max_lon, max_lat))
  in

  (* Pre-upload all tile geometry to GPU buffers *)
  let uploaded_tiles =
    List.filter_map
      (fun (tile, tile_range_lon, tile_range_lat) ->
        let header = tile.Clc_loader.header in
        let t_min_lon = header.Clc_loader.min_lon in
        let t_min_lat = header.Clc_loader.min_lat in
        let t_max_lon = t_min_lon +. tile_range_lon in
        let t_max_lat = t_min_lat +. tile_range_lat in

        (* Check if tile intersects ANY level *)
        let visible_any =
          Array.exists
            (fun level_params ->
              Web_utils.intersects
                (t_min_lon, t_min_lat, t_max_lon, t_max_lat)
                level_params)
            level_bounds
        in

        if not visible_any then None
        else
          let land_index_count = Bigarray.Array1.dim tile.Clc_loader.indices in
          let water_index_count =
            Bigarray.Array1.dim tile.Clc_loader.water_indices
          in

          (* Create and upload land buffers if needed *)
          let land_buffers =
            if land_index_count > 0 then begin
              let pos_buf = Gl.create_buffer ctx in
              Gl.bind_buffer ctx Gl.array_buffer (Some pos_buf);
              Gl.buffer_data ctx Gl.array_buffer
                (Brr.Tarray.of_bigarray1 tile.Clc_loader.positions)
                Gl.static_draw;

              let col_buf = Gl.create_buffer ctx in
              Gl.bind_buffer ctx Gl.array_buffer (Some col_buf);
              Gl.buffer_data ctx Gl.array_buffer
                (Brr.Tarray.of_bigarray1 tile.Clc_loader.colors)
                Gl.static_draw;

              let idx_buf = Gl.create_buffer ctx in
              Gl.bind_buffer ctx Gl.element_array_buffer (Some idx_buf);
              Gl.buffer_data ctx Gl.element_array_buffer
                (Brr.Tarray.of_bigarray1 tile.Clc_loader.indices)
                Gl.static_draw;

              Some (pos_buf, col_buf, idx_buf, land_index_count)
            end
            else None
          in

          (* Create and upload water buffers if needed *)
          let water_buffers =
            if water_index_count > 0 then begin
              let pos_buf = Gl.create_buffer ctx in
              Gl.bind_buffer ctx Gl.array_buffer (Some pos_buf);
              Gl.buffer_data ctx Gl.array_buffer
                (Brr.Tarray.of_bigarray1 tile.Clc_loader.water_positions)
                Gl.static_draw;

              let col_buf = Gl.create_buffer ctx in
              Gl.bind_buffer ctx Gl.array_buffer (Some col_buf);
              Gl.buffer_data ctx Gl.array_buffer
                (Brr.Tarray.of_bigarray1 tile.Clc_loader.water_colors)
                Gl.static_draw;

              let idx_buf = Gl.create_buffer ctx in
              Gl.bind_buffer ctx Gl.element_array_buffer (Some idx_buf);
              Gl.buffer_data ctx Gl.element_array_buffer
                (Brr.Tarray.of_bigarray1 tile.Clc_loader.water_indices)
                Gl.static_draw;

              let water_scale =
                tile_range_lon *. header.Clc_loader.water_scale_x
              in
              Some (pos_buf, col_buf, idx_buf, water_index_count, water_scale)
            end
            else None
          in

          Some
            ( tile_range_lon,
              tile_range_lat,
              t_min_lon,
              t_min_lat,
              t_max_lon,
              t_max_lat,
              land_buffers,
              water_buffers ))
      clc_tiles
  in

  (* Render each level in turn *)
  for level = 0 to 6 do
    let v_min_lon, v_min_lat, v_ext_lon, v_ext_lat, v_max_lon, v_max_lat =
      level_bounds.(level)
    in

    (* Switch FBO to this level ONCE *)
    Gl.framebuffer_texture_layer ctx Gl.framebuffer Gl.color_attachment0
      cover_map_texture 0 level;
    Gl.framebuffer_renderbuffer ctx Gl.framebuffer Gl.depth_attachment
      Gl.renderbuffer clc_depth_rbs.(level);

    (* Clear depth buffer *)
    Gl.clear_depth ctx 1.0;
    Gl.clear ctx Gl.depth_buffer_bit;
    Gl.depth_mask ctx true;
    Gl.enable ctx Gl.depth_test;
    Gl.depth_func ctx Gl.less;

    (* WATER PASS for this level *)
    Gl.use_program ctx water_raster_pid;

    List.iter
      (fun ( tile_range_lon,
             tile_range_lat,
             t_min_lon,
             t_min_lat,
             t_max_lon,
             t_max_lat,
             _land_buffers,
             water_buffers ) ->
        match water_buffers with
        | None -> ()
        | Some (pos_buf, col_buf, idx_buf, water_index_count, water_scale) ->
            if
              Web_utils.intersects
                (t_min_lon, t_min_lat, t_max_lon, t_max_lat)
                ( v_min_lon,
                  v_min_lat,
                  v_ext_lon,
                  v_ext_lat,
                  v_max_lon,
                  v_max_lat )
            then begin
              (* Bind pre-uploaded buffers *)
              Gl.bind_buffer ctx Gl.array_buffer (Some pos_buf);
              Gl.enable_vertex_attrib_array ctx 0;
              Gl.vertex_attrib_ipointer ctx 0 2 Gl.int 0 0;

              Gl.bind_buffer ctx Gl.array_buffer (Some col_buf);
              Gl.enable_vertex_attrib_array ctx 1;
              Gl.vertex_attrib_ipointer ctx 1 1 Gl.unsigned_byte 0 0;

              Gl.bind_buffer ctx Gl.element_array_buffer (Some idx_buf);

              (* Set Uniforms *)
              Gl.uniform1f ctx water_u.u_water_scale water_scale;
              Gl.uniform2f ctx water_u.u_tile_range tile_range_lon
                tile_range_lat;
              Gl.uniform2f ctx water_u.u_tile_min t_min_lon t_min_lat;
              Gl.uniform2f ctx water_u.u_tex_min v_min_lon v_min_lat;
              Gl.uniform2f ctx water_u.u_tex_range v_ext_lon v_ext_lat;

              Gl.draw_elements ctx Gl.triangles water_index_count
                Gl.unsigned_int 0
            end)
      uploaded_tiles;

    (* LAND PASS for this level *)
    Gl.use_program ctx clc_raster_pid;

    List.iter
      (fun ( tile_range_lon,
             tile_range_lat,
             t_min_lon,
             t_min_lat,
             t_max_lon,
             t_max_lat,
             land_buffers,
             _water_buffers ) ->
        match land_buffers with
        | None -> ()
        | Some (pos_buf, col_buf, idx_buf, index_count) ->
            if
              Web_utils.intersects
                (t_min_lon, t_min_lat, t_max_lon, t_max_lat)
                ( v_min_lon,
                  v_min_lat,
                  v_ext_lon,
                  v_ext_lat,
                  v_max_lon,
                  v_max_lat )
            then begin
              (* Bind pre-uploaded buffers *)
              Gl.bind_buffer ctx Gl.array_buffer (Some pos_buf);
              Gl.enable_vertex_attrib_array ctx 0;
              Gl.vertex_attrib_pointer ctx 0 2 Gl.unsigned_short true 0 0;

              Gl.bind_buffer ctx Gl.array_buffer (Some col_buf);
              Gl.enable_vertex_attrib_array ctx 1;
              Gl.vertex_attrib_ipointer ctx 1 1 Gl.unsigned_byte 0 0;

              Gl.bind_buffer ctx Gl.element_array_buffer (Some idx_buf);

              (* Set Uniforms *)
              Gl.uniform2f ctx clc_u.u_tile_range tile_range_lon tile_range_lat;
              Gl.uniform2f ctx clc_u.u_tile_min t_min_lon t_min_lat;
              Gl.uniform2f ctx clc_u.u_tex_min v_min_lon v_min_lat;
              Gl.uniform2f ctx clc_u.u_tex_range v_ext_lon v_ext_lat;

              Gl.draw_elements ctx Gl.triangles index_count Gl.unsigned_int 0
            end)
      uploaded_tiles
  done;

  (* Cleanup - delete pre-uploaded buffers *)
  List.iter
    (fun (_, _, _, _, _, _, land_buffers, water_buffers) ->
      (match land_buffers with
      | Some (pos_buf, col_buf, idx_buf, _) ->
          Gl.delete_buffer ctx pos_buf;
          Gl.delete_buffer ctx col_buf;
          Gl.delete_buffer ctx idx_buf
      | None -> ());
      match water_buffers with
      | Some (pos_buf, col_buf, idx_buf, _, _) ->
          Gl.delete_buffer ctx pos_buf;
          Gl.delete_buffer ctx col_buf;
          Gl.delete_buffer ctx idx_buf
      | None -> ())
    uploaded_tiles;

  Gl.bind_vertex_array ctx None;
  Gl.delete_vertex_array ctx vao;
  Gl.delete_buffer ctx vbo_pos;
  Gl.delete_buffer ctx vbo_col;
  Gl.delete_buffer ctx ebo;
  Gl.disable ctx Gl.depth_test;
  Gl.bind_framebuffer ctx Gl.framebuffer None;

  cover_map_texture

let draw_shadows ~shadow_pid ~shadow_fbo ~shadow_map
    (shadow_uniforms : Render_state.shadow_uniforms) ~matrices ~terrain_geo
    ~index_count ~relief_texture ctx =
  let width = Brr_canvas.Gl.drawing_buffer_width ctx in
  let height = Brr_canvas.Gl.drawing_buffer_height ctx in

  (* Unbind Shadow Map from Texture Unit 4 to prevent Feedback Loop *)
  Gl.active_texture ctx Gl.texture4;
  Gl.bind_texture ctx Gl.texture_2d_array None;

  (* Setup FBO and viewport *)
  Gl.bind_framebuffer ctx Gl.framebuffer (Some shadow_fbo);
  Gl.viewport ctx 0 0 2048 2048;
  Gl.use_program ctx shadow_pid;

  (* Bind Relief Texture *)
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);
  Gl.uniform1i ctx shadow_uniforms.relief 0;

  Gl.bind_vertex_array ctx (Some terrain_geo);

  (* Setup render state *)
  Gl.depth_mask ctx true;
  Gl.disable ctx Gl.scissor_test;
  Gl.disable ctx Gl.blend;
  Gl.enable ctx Gl.depth_test;
  Gl.depth_func ctx Gl.less;
  Gl.disable ctx Gl.cull_face';
  Gl.color_mask ctx false false false false;
  Gl.clear_depth ctx 1.0;

  (* Render shadow map with 4 rotations to cover full 360° terrain *)
  let rotation_angles = [| 0.; pi /. 2.; pi; 3. *. pi /. 2. |] in

  for layer = 0 to 2 do
    Gl.framebuffer_texture_layer ctx Gl.framebuffer Gl.depth_attachment
      shadow_map 0 layer;

    (* Clear full texture including 1-pixel border with depth=1.0 *)
    Gl.disable ctx Gl.scissor_test;
    Gl.clear ctx (Gl.depth_buffer_bit lor Gl.color_buffer_bit);

    (* Enable scissor to render only inner area, leaving 1-pixel border *)
    Gl.enable ctx Gl.scissor_test;
    Gl.scissor ctx 1 1 2046 2046;

    Gl.uniform_matrix4fv ctx shadow_uniforms.shadow_view_proj false
      (Brr.Tarray.of_bigarray1 (Matrix.array matrices.(layer)));

    (* Render 4 rotations to cover full terrain *)
    for rotation = 0 to 3 do
      Gl.uniform1f ctx shadow_uniforms.snapped_alpha rotation_angles.(rotation);
      Gl.draw_elements ctx Gl.triangle_strip index_count Gl.unsigned_int 0
    done;

    Gl.disable ctx Gl.scissor_test
  done;

  Gl.bind_vertex_array ctx None;

  (* Restore state *)
  Gl.color_mask ctx true true true true;
  Gl.cull_face ctx Gl.back;
  Gl.bind_framebuffer ctx Gl.framebuffer None;
  Gl.clear_depth ctx 1.0;
  Gl.viewport ctx 0 0 width height

(** Bind all terrain textures to their units. Call at init and after
    draw_shadows. *)
let bind_terrain_textures ctx ~relief_texture ~ao_texture ~detail_map
    ~shadow_map ~cover_map_texture ~palette_texture =
  let open Brr_canvas in
  Gl.active_texture ctx Gl.texture1;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);
  Gl.active_texture ctx Gl.texture3;
  Gl.bind_texture ctx Gl.texture_2d (Some ao_texture);
  Gl.active_texture ctx Gl.texture5;
  Gl.bind_texture ctx Gl.texture_2d (Some detail_map);
  Gl.active_texture ctx Gl.texture4;
  Gl.bind_texture ctx Gl.texture_2d_array (Some shadow_map);
  Gl.active_texture ctx Gl.texture7;
  Gl.bind_texture ctx Gl.texture_2d_array (Some cover_map_texture);
  Gl.active_texture ctx Gl.texture8;
  Gl.bind_texture ctx Gl.texture_2d (Some palette_texture);
  Gl.active_texture ctx Gl.texture0
(* Text Rendering *)

type lazy_text = {
  text : string;
  mutable texture : (Gl.texture * int * int) option;
}

let text_canvas = Brr_canvas.Canvas.of_el (Brr.El.canvas [])
let text_ctx = Brr_canvas.C2d.get_context text_canvas

let prepare_text_immediate ctx text =
  let open Brr_canvas in
  let text = Jstr.v text in
  C2d.set_font text_ctx (Jstr.v "48px sans");
  let m = C2d.measure_text text_ctx text in
  let ascent = C2d.Text_metrics.font_bounding_box_ascent m in
  let descent = C2d.Text_metrics.font_bounding_box_descent m in
  let left = C2d.Text_metrics.actual_bounding_box_left m in
  let right = C2d.Text_metrics.actual_bounding_box_right m in
  let w = truncate (left +. right +. 0.5) in
  let h = truncate (ascent +. descent +. 0.5) in
  (* Avoid 0x0 canvas which causes GL errors *)
  let w = max 1 w in
  let h = max 1 h in
  Brr_canvas.Canvas.set_w text_canvas w;
  Brr_canvas.Canvas.set_h text_canvas h;
  C2d.set_font text_ctx (Jstr.v "48px sans");
  C2d.fill_text text_ctx text ~x:left ~y:ascent;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d_of_source ctx Gl.texture_2d 0 Gl.rgba w h 0 Gl.rgba
    Gl.unsigned_byte
    (Gl.Tex_image_source.of_canvas_el text_canvas);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.bind_texture ctx Gl.texture_2d None;
  (tid, w, h)

let prepare_text _ctx text = { text; texture = None }

let draw_text ctx (uniforms : Render_state.text_uniforms) transform buffer view
    (lazy_text : lazy_text) =
  let tid, w, h =
    match lazy_text.texture with
    | Some t -> t
    | None ->
        let t = prepare_text_immediate ctx lazy_text.text in
        lazy_text.texture <- Some t;
        t
  in
  let open Brr_canvas in
  let transform = Matrix.(scale (float w /. float h) 1. 1. * transform) in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Matrix.blit transform buffer;
  Gl.uniform_matrix4fv ctx uniforms.transform false view;
  Gl.draw_elements ctx Gl.triangle_strip 4 Gl.unsigned_byte 0
(* Texture unbind removed - next draw_text or terrain pass rebinds anyway *)

(* Main Draw Loop *)

let scale = (*2. *. 27. /. 24.*) 3.2
let text_height = 0.07

(* Helper functions for orientation control *)
let get_inclination_rad q =
  let q_inv = Quaternion.conjugate q in
  let up_cam =
    Quaternion.transform_vector q_inv { x = 0.; y = 0.; z = 1.; w = 0. }
  in
  atan2 up_cam.x up_cam.y

let screen_inclination q =
  let angle = get_inclination_rad q in
  -.angle *. 180. /. Float.pi

let snap_to_turntable q =
  (* 1. Extract Forward vector (World space direction of Camera -Z) *)
  let fwd = Quaternion.transform_vector q { x = 0.; y = 0.; z = -1.; w = 0. } in
  (* 2. Extract Yaw (psi). Align 0 to North (Y).
     RotZ(psi) maps (0,1) to (-sin(psi), cos(psi)).
     We want psi. atan2(sin, cos).
     sin = -x, cos = y.
     psi = atan2(-x, y). *)
  let psi = atan2 (-.fwd.x) fwd.y in
  (* 3. Extract Pitch (theta). Angle from Down (-Z).
     fwd.z = -cos(theta) -> theta = acos(-z) *)
  let fwd_z = max (-1.) (min 1. fwd.z) in
  let theta = acos (-.fwd_z) in
  (* 4. Reconstruct
     Yaw (Z axis) then Pitch (X axis, negative angle from Down) *)
  let q_yaw =
    Quaternion.from_axis_angle { x = 0.; y = 0.; z = 1.; w = 0. } psi
  in
  let q_pitch =
    Quaternion.from_axis_angle { x = 1.; y = 0.; z = 0.; w = 0. } theta
  in
  Quaternion.(q_yaw * q_pitch)

let apply_manual_rotation q da_rad db_rad =
  (* 1. Apply Yaw (Global Z) *)
  let q_yaw =
    Quaternion.from_axis_angle { x = 0.; y = 0.; z = 1.; w = 0. } da_rad
  in
  let q_yawed = Quaternion.mult q_yaw q in

  (* 2. Extract current Pitch (theta) from yawed quaternion
     Forward vector z component tells us the inclination.
     fwd = q * (0,0,-1)
     fwd.z = -cos(theta) -> theta = acos(-fwd.z) *)
  let fwd =
    Quaternion.transform_vector q_yawed { x = 0.; y = 0.; z = -1.; w = 0. }
  in
  let fwd_z = max (-1.) (min 1. fwd.z) in
  let theta = acos (-.fwd_z) in

  (* 3. Calculate target pitch and Clamp *)
  let target_theta = theta +. db_rad in
  let min_pitch = 60. *. Float.pi /. 180. in
  let max_pitch = 120. *. Float.pi /. 180. in
  let clamped_theta = max min_pitch (min max_pitch target_theta) in

  (* 4. Apply effective pitch delta (Local X) *)
  let effective_db = clamped_theta -. theta in
  let q_pitch =
    Quaternion.from_axis_angle { x = 1.; y = 0.; z = 0.; w = 0. } effective_db
  in
  Quaternion.mult q_yawed q_pitch

let draw terrain_pid terrain_geo _tile_texture _relief_texture triangle_pid
    text_pid text_geo ~(terrain_uniforms : Render_state.terrain_uniforms)
    ~(triangle_uniforms : Render_state.triangle_uniforms)
    ~(text_uniforms : Render_state.text_uniforms) ~proj_ba ~transform_ba
    ~inv_view_ba ~proj_ta ~transform_ta ~inv_view_ta ~_shadow_matrices:_ ~_w:_
    ~x ~y ~height ~lat ~lon ~orientation ~points ~tile ~index_count
    ~_ao_texture:_ ~_detail_map:_ ~_shadow_pid:_ ~_shadow_fbo:_ ~_shadow_map:_
    ~(_shadow_uniforms : Render_state.shadow_uniforms) ~_palette_texture:_
    ~_cover_map_texture:_ ~sky_pid ~sky_uniforms
    ~(radial_params : Render_state.radial_params) canvas ctx =
  (* Poll for completed GPU timer queries *)
  Gpu_timer.poll_results ctx;
  let canvas = Brr_canvas.Canvas.of_el canvas in
  let canvas_width = Brr_canvas.Canvas.w canvas in
  let canvas_height = Brr_canvas.Canvas.h canvas in
  Gl.viewport ctx 0 0 canvas_width canvas_height;
  let aspect = float canvas_width /. float canvas_height in
  let deltax, deltay, _ = Render_state.compute_deltas ~lat in
  let transform =
    Matrix.(
      translate 0. 0. (-.height -. 2.)
      * rotation_matrix (Quaternion.conjugate orientation))
  in

  let x_scale, y_scale =
    let s = scale *. !zoom in
    if aspect < 1. then (s /. aspect, s) else (s, s *. aspect)
  in
  let text_scale = scale *. !zoom in
  let proj = Matrix.project ~x_scale ~y_scale ~near_plane:1. in
  let points =
    List.filter_map
      (fun (pt, (x', y')) ->
        let off_x = Render_state.compute_sub_arcsec_offset lon in
        let off_y = Render_state.compute_sub_arcsec_offset lat in
        let px = deltax *. (float (x' - x) -. off_x) in
        let py = deltay *. (float (y - y') -. off_y) in
        let z = Dem_loader.get_height tile y' x' in
        let r = Matrix.({ x = px; y = py; z; w = 1. } *< transform) in
        let r = { r with z = -.r.z } in
        if r.z > 1. && abs_float (r.x /. r.z) < 1. then
          Some (pt, r.x /. r.z, r.y /. r.z)
        else None)
      points
  in
  let points =
    let pos = ref [] in
    let angle = (screen_inclination orientation *. pi /. 180.) +. (pi /. 4.) in
    let ca = cos angle in
    let sa = sin angle in
    List.filter_map
      (fun (texture, x, y) ->
        let p = text_scale *. ((y *. ca) -. (x *. sa)) in
        let shown =
          if
            not
              (List.exists
                 (fun p' -> abs_float (p' -. p) < 0.8 *. text_height)
                 !pos)
          then (
            pos := p :: !pos;
            true)
          else false
        in
        if shown then Some (texture, x, y, shown) else None)
      points
  in

  (* Prepare Clear Color matching fog *)
  let r, g, b = fog_linear in
  Gl.clear_color ctx r g b 1.;
  Gl.clear ctx (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

  Gl.depth_mask ctx true;
  Gl.use_program ctx terrain_pid;
  Gl.enable ctx Gl.depth_test;
  Gl.enable ctx Gl.cull_face';
  (* Determine snapped alpha - changes with camera orientation *)
  let grid_k = radial_params.Render_state.grid_k in
  let current_azimuth = compute_azimuth transform in
  let snapped_alpha = floor ((current_azimuth /. grid_k) +. 0.5) *. grid_k in
  Gl.uniform1f ctx terrain_uniforms.snapped_alpha snapped_alpha;
  (* Matrices - change with camera orientation and aspect ratio *)
  Matrix.blit proj proj_ba;
  Gl.uniform_matrix4fv ctx terrain_uniforms.proj false proj_ta;
  Matrix.blit transform transform_ba;
  Gl.uniform_matrix4fv ctx terrain_uniforms.transform false transform_ta;
  Gl.bind_vertex_array ctx (Some terrain_geo);
  Gl.draw_elements ctx Gl.triangle_strip index_count Gl.unsigned_int 0;
  Gl.bind_vertex_array ctx None;
  Gl.bind_texture ctx Gl.texture_2d None;
  Gl.disable ctx Gl.cull_face';

  (* Draw Sky (Optimized: Z=1.0, Blit, Late Draw) *)
  Gl.depth_mask ctx false;
  Gl.depth_func ctx Gl.lequal;
  (* Draw if Z <= 1.0 (Far Plane) *)
  Gl.disable ctx Gl.cull_face';
  Gl.use_program ctx sky_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  (* Compute Inverse View *)
  let inv_view = Matrix.inverse transform in
  Matrix.blit inv_view inv_view_ba;
  Gl.uniform_matrix4fv ctx sky_uniforms.Render_state.inv_view false inv_view_ta;
  Gl.uniform2f ctx sky_uniforms.Render_state.sky_params x_scale y_scale;
  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  (* VAO text_geo is still bound, reused for POIs *)
  Gl.disable ctx Gl.depth_test;
  Gl.enable ctx Gl.blend;
  Gl.blend_func ctx Gl.one Gl.one_minus_src_alpha;

  (* 1. Triangles *)
  Gl.use_program ctx triangle_pid;
  List.iter
    (fun (_, x, y, shown) ->
      let x = x *. x_scale in
      let y = y *. y_scale in
      let angle = if shown then -.pi /. 4. else 0. in
      let transform =
        let sx = 0.6 *. text_height *. x_scale /. text_scale in
        let sy = 0.6 *. text_height *. y_scale /. text_scale in
        Matrix.(
          rotate_z (angle +. (screen_inclination orientation *. pi /. 180.))
          * scale sx sy 1. * translate x y 0.)
      in
      Matrix.blit transform transform_ba;
      Gl.uniform_matrix4fv ctx triangle_uniforms.transform false transform_ta;
      if shown then Gl.uniform4f ctx triangle_uniforms.color 0. 0. 0. 1.
      else Gl.uniform4f ctx triangle_uniforms.color 0. 0. 0. 0.4;
      Gl.draw_elements ctx Gl.triangles 3 Gl.unsigned_byte 0)
    points;

  (* 2. Text *)
  Gl.use_program ctx text_pid;
  List.iter
    (fun (texture, x, y, shown) ->
      if shown then
        let x = x *. x_scale in
        let y = y *. y_scale in
        let transform =
          let sx = text_height *. x_scale /. text_scale in
          let sy = text_height *. y_scale /. text_scale in
          Matrix.(
            translate 0.7 (-0.5) 0.
            * rotate_z
                ((pi /. 4.) +. (screen_inclination orientation *. pi /. 180.))
            * scale sx sy 1. * translate x y 0.)
        in
        draw_text ctx text_uniforms transform transform_ba transform_ta texture)
    points;

  Gl.disable ctx Gl.blend;
  Gl.bind_vertex_array ctx None

(* Event loop *)

let current_orientation = ref Quaternion.identity
let target_orientation = ref Quaternion.identity
let is_dragging = ref false
let velocity = ref (0., 0.)
let last_input_time = ref 0.

(* Mouse state *)
let mouse_dragging = ref false
let mouse_start_x = ref 0.
let mouse_start_y = ref 0.
let mouse_last_x = ref 0.
let mouse_last_y = ref 0.

(* Touch state *)
let touch_start_x = ref 0.
let touch_start_y = ref 0.
let touch_last_x = ref 0.
let touch_last_y = ref 0.
let touch_dragging = ref false
let pinch_distance = ref 0.

(* Double-tap detection for returning to sensor mode *)
let last_tap_time = ref 0.
let double_tap_threshold = 300.
(* ms *)

(* Drag threshold to distinguish tap from drag (in pixels) *)
let drag_threshold = 10.
let last_frame_time = ref 0.

let event_loop ctx draw =
  let rec loop prev_orientation prev_zoom =
    let t = now () in
    let dt = t -. !last_frame_time in
    last_frame_time := t;

    if !input_mode = Sensor then begin
      (* Sensor Mode: Adaptive Smoothing using SLERP
         Tau (time constant) varies with FOV to prevent jitter when zoomed in.
         - Wide FOV (Zoomed Out): Tau = 0.1s (Fast response)
         - Narrow FOV (Zoomed In): Tau = 0.5s (Slow, smooth response)
      *)
      let tau =
        let min_tau = 0.02 in
        let max_tau = 0.15 in
        let t = (log !zoom -. log min_zoom) /. (log max_zoom -. log min_zoom) in
        let t = max 0. (min 1. t) in
        let t = t *. t in
        min_tau +. (t *. (max_tau -. min_tau))
      in
      let alpha = 1. -. exp (-.dt /. (tau *. 1000.)) in
      current_orientation :=
        Quaternion.slerp !current_orientation !target_orientation alpha
    end
    else if (not !is_dragging) && (fst !velocity <> 0. || snd !velocity <> 0.)
    then begin
      let va, vb = !velocity in
      (* Friction: 0.95 per 16ms *)
      let friction = 0.95 ** (dt /. 16.6) in
      let va = va *. friction in
      let vb = vb *. friction in
      let va = if abs_float va < 0.0001 then 0. else va in
      let vb = if abs_float vb < 0.0001 then 0. else vb in
      velocity := (va, vb);
      velocity := (va, vb);

      if (not !is_dragging) && (not !touch_dragging) && !velocity <> (0., 0.)
      then begin
        let vx, vy = !velocity in
        let dt = dt *. 1.5 in
        (* Speed tweaks *)
        if abs_float vx > 0.001 || abs_float vy > 0.001 then begin
          let da = vx *. dt in
          let db = vy *. dt in
          let da_rad = da *. Float.pi /. 180. in
          let db_rad = db *. Float.pi /. 180. in

          current_orientation :=
            apply_manual_rotation !current_orientation da_rad db_rad
        end
        else velocity := (0., 0.)
      end
    end;
    if !input_mode = Manual then begin
      (* Smooth "Righting Moment": pull current orientation towards upright.
         Tau = 0.2s provides a smooth but firm correction. *)
      let upright = snap_to_turntable !current_orientation in
      let alpha = 1. -. exp (-.dt /. 200.0) in
      current_orientation := Quaternion.slerp !current_orientation upright alpha
    end;
    let orientation = !current_orientation in
    let z = !zoom in
    if orientation <> prev_orientation || z <> prev_zoom || !force_redraw then (
      force_redraw := false;
      draw ~orientation ctx);
    let* () = request_animation_frame () in
    loop orientation z
  in
  last_frame_time := now ();
  loop !current_orientation (!zoom -. 1.)

let tri ~w ~h ~x ~y ~height ~lat ~lon ~points ~tile canvas ctx ~detail_map
    ~clc_tiles ~graphics ~start =
  let {
    terrain_geo;
    indices;
    text_geo;
    triangle_geo;
    terrain_pid;
    triangle_pid;
    text_pid;
    shadow_pid;
    sky_pid;
    sky_uniforms;
    terrain_uniforms;
    triangle_uniforms;
    text_uniforms;
    shadow_map;
    shadow_fbo;
    shadow_uniforms;
    normal_pid;
    mipmap_pid;
    copy_pid;
    relief_uniforms;
    mipmap_uniforms;
    copy_uniforms;
    ao_bake_pid;
    ao_blur_pid;
    ao_bake_uniforms;
    ao_blur_uniforms;
    clc_raster_pid;
    water_raster_pid;
    clc_raster_uniforms;
    water_raster_uniforms;
    radial_params;
    nearest_sampler;
  } =
    graphics
  in
  let _, deltay, _ = Render_state.compute_deltas ~lat in
  let tile_texture = make_tile_texture ctx tile in
  let relief_texture =
    time_gpu ctx "compute_relief" (fun () ->
        compute_relief ctx w h lat triangle_geo tile_texture normal_pid
          mipmap_pid copy_pid relief_uniforms mipmap_uniforms copy_uniforms)
  in
  let points =
    List.map
      (fun ({ Points.name; elevation; _ }, ((x', y') as pos)) ->
        let texture =
          prepare_text ctx
            (match elevation with
            | None -> name
            | Some elevation ->
                (*                Format.eprintf "ZZZ %s %g %d@." name tile.{y', x'} elevation;*)
                Printf.sprintf "%s (%dm)" name elevation)
        in
        let h =
          let height' = Dem_loader.get_height tile y' x' in
          let dist =
            let dx = float (x' - x) in
            let dy = float (y' - y) in
            let dz = height' -. height in
            sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz))
          in
          (height' -. height) /. dist
        in
        ((texture, pos), h))
      points
  in
  let points =
    points
    |> List.sort (fun (_, h) (_, h') : int -> Stdlib.compare h' h)
    |> List.map fst
  in
  let index_count = Bigarray.Array1.dim indices in

  (* Approx 30m per pixel *)
  let ao_texture =
    time_gpu ctx "compute_ao" (fun () ->
        compute_ao ctx w h deltay relief_texture nearest_sampler ao_bake_pid
          ao_blur_pid ao_bake_uniforms ao_blur_uniforms)
  in

  (* Compute session-static values for terrain uniforms *)
  let light_dir_shader =
    let date_ctor = Jv.get Jv.global "Date" in
    let now = Jv.to_float (Jv.call date_ctor "now" [||]) /. 1000. in
    let sx, sy, sz = Sun.position ~lat ~lon ~time:now in
    let sx, sy, sz =
      if sz < 0.2 then
        let date = Jv.new' date_ctor [||] in
        let _ = Jv.call date "setMonth" [| Jv.of_int 6 |] in
        let _ = Jv.call date "setHours" [| Jv.of_int 10 |] in
        let _ = Jv.call date "setMinutes" [| Jv.of_int 0 |] in
        let t = Jv.to_float (Jv.call date "valueOf" [||]) /. 1000. in
        Sun.position ~lat ~lon ~time:t
      else (sx, sy, sz)
    in
    Matrix.{ x = sx; y = -.sy; z = sz; w = 0. }
  in
  let light_dir_shadows =
    Matrix.
      {
        x = light_dir_shader.x;
        y = -.light_dir_shader.y;
        z = light_dir_shader.z;
        w = 0.;
      }
  in
  let splits_dist = [| 2000.; 8000.; 25000. |] in
  let center_offset_x, center_offset_y =
    Render_state.compute_center_offset ~lat ~lon ~x ~y:(h - y)
  in
  let world_center =
    Matrix.{ x = center_offset_x; y = center_offset_y; z = 0.; w = 1. }
  in
  let shadow_matrices =
    (* view_proj is ignored in calculate_shadow_matrices *)
    calculate_shadow_matrices ~light_dir:light_dir_shadows ~world_center
  in

  (* Upload all session-static uniforms *)
  Render_state.upload_session_static ctx terrain_pid sky_pid shadow_pid
    terrain_uniforms sky_uniforms shadow_uniforms ~w ~lat ~x ~y:(h - y) ~lon
    ~light_dir:light_dir_shader ~shadow_matrices ~shadow_splits:splits_dist
    ~fog_color:fog_linear ~zenith_color:zenith_linear;

  (* GPU rasterize CLC tiles to FBO *)
  let cover_map_texture =
    time_gpu ctx "rasterize_clc_tiles" (fun () ->
        rasterize_clc_tiles ctx ~lat ~lon ~clc_tiles ~clc_raster_pid
          ~water_raster_pid clc_raster_uniforms water_raster_uniforms)
  in

  (* CLC Textures *)
  let palette_texture = make_palette_texture ctx in

  (* Render shadows *)
  time_gpu ctx "draw_shadows" (fun () ->
      draw_shadows ~shadow_pid ~shadow_fbo ~shadow_map shadow_uniforms
        ~matrices:shadow_matrices ~terrain_geo ~index_count ~relief_texture ctx);

  (* Bind all terrain textures at init - after all textures are created *)
  bind_terrain_textures ctx ~relief_texture ~ao_texture ~detail_map ~shadow_map
    ~cover_map_texture ~palette_texture;

  let* () = Web_utils.on_gpu_finished ctx in
  start ();
  hide_startup_overlay ();

  let proj_ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 16 in
  let transform_ba =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 16
  in
  let proj_ta = Brr.Tarray.of_bigarray1 proj_ba in
  let transform_ta = Brr.Tarray.of_bigarray1 transform_ba in
  let inv_view_ba =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 16
  in
  let inv_view_ta = Brr.Tarray.of_bigarray1 inv_view_ba in

  event_loop ctx (fun ~orientation ctx ->
      draw terrain_pid terrain_geo tile_texture relief_texture triangle_pid
        text_pid text_geo ~terrain_uniforms ~triangle_uniforms ~text_uniforms
        ~proj_ba ~transform_ba ~inv_view_ba ~proj_ta ~transform_ta ~inv_view_ta
        ~_shadow_matrices:shadow_matrices ~_w:w ~x ~y ~lat ~lon ~orientation
        ~height ~tile ~points ~index_count ~_ao_texture:ao_texture
        ~_detail_map:detail_map ~_shadow_pid:shadow_pid ~_shadow_fbo:shadow_fbo
        ~_shadow_map:shadow_map ~_shadow_uniforms:shadow_uniforms
        ~_palette_texture:palette_texture ~_cover_map_texture:cover_map_texture
        ~sky_pid ~sky_uniforms ~radial_params canvas ctx)
(* Location & UI *)

let featured_locations =
  [
    ("Col Girardin", 44.6078064, 6.8210935, 220.);
    ("La Chalannette, Jausiers", 44.3950846, 6.7669714, 50.);
    ("Col du Blainon", 44.209067, 6.9423065, 0.);
    ("Auron (Vallée de la Tinée)", 44.207447, 6.906400, 40.);
    ("Vallon de la Braïssa", 44.280097, 6.793942, 0.);
    ("Lacs de Morgon", 44.336516, 6.913906, 0.);
    ("Roc Diolon (Orcières)", 44.73360, 6.363068, 0.);
    ("Col Fromage", 44.6896583, 6.8061028, 180.);
    ("La Mortice Sud", 44.573885, 6.7694, 0.);
    ("Le Sommet Rouge", 44.5740068, 6.7954285, 0.);
    ("Col de la Braïssa", 44.278358, 6.790589, 0.);
    ("Montée vers Lac des Neufs Couleurs", 44.536194, 6.804142, 0.);
    ("Pic de Morgon", 44.4920, 6.3975, 0.);
    ("Lac de Roburent", 44.424680, 6.93430, 220.);
    ("Mont Ténibre", 44.2839, 6.9719, 0.);
    ("Baisse de Druos", 44.191930, 7.19195, 0.);
  ]

let get_preset_position () =
  match featured_locations with
  | (_, lat, lon, alpha) :: _ -> (lat, lon, alpha)
  | [] -> (44.3950846, 6.7669714, 170.)

let set_orientation_from_yaw alpha =
  let alpha_rad = alpha *. pi /. 180. in
  (* Initial orientation: Yaw only, Pitch ~90 (looking down) or 0 (horizon)? 
       Original defaults: beta=0 means looking down? 
       Wait, original code:
       rotate_x (-beta)
       
       If beta=0, no X rotation. Camera looks down -Z?
       Camera setup usually looks -Z.
       Terrain is on XY plane (z=0?).
       translate 0 0 (-height - 2).
       So camera is at +Z. Looking down -Z.
       So beta=0 is looking straight down.
       beta=90 is looking at horizon.
       
       Let's stick to that.
                                           
       So for "alpha" in preset (yaw):
       We want Global Z rotation.
    *)
  let q_yaw =
    Quaternion.from_axis_angle { x = 0.; y = 0.; z = 1.; w = 0. } alpha_rad
  in

  (* Set default pitch to something reasonable, e.g. 80 degrees (near horizon) *)
  let pitch_rad = pi /. 2. in
  let q_pitch =
    Quaternion.from_axis_angle { x = 1.; y = 0.; z = 0.; w = 0. } pitch_rad
  in

  (* Rotate around Z first (yaw), then around X (pitch) to maintain turntable *)
  (current_orientation := Quaternion.(q_yaw * q_pitch));
  target_orientation := !current_orientation

let parse_float_safe s = try Some (float_of_string s) with _ -> None

let parse_input_coordinates input =
  let input = String.trim input in
  let input =
    match Jstr.to_string (Jstr.lowercased (Jstr.v input)) with
    | "my location" -> ""
    | _ -> input
  in
  if input = "" then None
  else
    (* 1. Check for Geo URI: geo:lat,lon *)
    let input =
      if Jstr.starts_with ~prefix:(Jstr.v "geo:") (Jstr.v input) then
        String.sub input 4 (String.length input - 4)
      else input
    in
    let coords_str =
      try
        if String.contains input '/' || String.contains input '?' then
          let uri = Brr.Uri.v (Jstr.v input) in
          let params = Brr.Uri.query_params uri in
          let get_param k =
            match Brr.Uri.Params.find (Jstr.v k) params with
            | Some v -> Some (Jstr.to_string v)
            | None -> None
          in
          match (get_param "lat", get_param "lon") with
          | Some lat, Some lon -> lat ^ "," ^ lon
          | _ -> (
              match get_param "ll" with
              | Some ll -> ll
              | None -> (
                  match get_param "q" with
                  | Some q -> q
                  | None -> (
                      let path = Jstr.to_string (Brr.Uri.path uri) in
                      try
                        let start = String.index path '@' + 1 in
                        let rest =
                          String.sub path start (String.length path - start)
                        in
                        match String.index_opt rest 'z' with
                        | Some i -> String.sub rest 0 i
                        | None -> rest
                      with Not_found -> input)))
        else input
      with _ -> input
    in
    (* 3. Robust parsing of "lat,lon", "lat lon", or "lat ; lon" *)
    let parts =
      let jv_coords = Jv.of_string (String.trim coords_str) in
      let jv_re =
        Jv.new' (Jv.get Jv.global "RegExp") [| Jv.of_string "[\\s;\\|:/]+" |]
      in
      let jv_parts = Jv.call jv_coords "split" [| jv_re |] in
      let parts =
        Jv.to_list Jv.to_string jv_parts |> List.filter (fun s -> s <> "")
      in
      (* Handle "45.1,6.7" (no space, just comma separator) *)
      match parts with
      | [ single ] when String.contains single ',' ->
          String.split_on_char ',' single |> List.filter (fun s -> s <> "")
      | _ -> parts
    in
    match parts with
    | lat_s :: lon_s :: _ -> (
        let clean s =
          let s =
            if String.length s > 0 && s.[String.length s - 1] = ',' then
              String.sub s 0 (String.length s - 1)
            else s
          in
          String.map (fun c -> if c = ',' then '.' else c) s
        in
        let lat_v = clean lat_s in
        let lon_v = clean lon_s in
        match (parse_float_safe lat_v, parse_float_safe lon_v) with
        | Some lat, Some lon -> Some (lat, lon)
        | _ -> None)
    | _ -> None

let get_url_position ~size =
  let uri = Brr.Window.location Brr.G.window in
  let params = Brr.Uri.query_params uri in
  let get_float k =
    match Brr.Uri.Params.find (Jstr.v k) params with
    | Some v -> parse_float_safe (Jstr.to_string v)
    | None -> None
  in
  match (get_float "lat", get_float "lon") with
  | Some lat, Some lon ->
      if
        Dem_loader.in_range ~size ~min_lat:43 ~max_lat:46 ~min_lon:5 ~max_lon:9
          ~lat ~lon
      then
        let alpha = Option.value (get_float "alpha") ~default:0. in
        Some (lat, lon, alpha)
      else None
  | _ -> None

let get_current_position ~size =
  let open Fut.Syntax in
  let open Brr_io.Geolocation in
  let opts = opts ~high_accuracy:true () in
  let+ pos = get ~opts (of_navigator Brr.G.navigator) in
  match pos with
  | Ok pos ->
      let lat = Pos.latitude pos in
      let lon = Pos.longitude pos in
      if
        Dem_loader.in_range ~size ~min_lat:43 ~max_lat:46 ~min_lon:5 ~max_lon:9
          ~lat ~lon
      then Some (lat, lon, 0.)
      else None
  | Error _ -> None

type location_source = Url | Geolocation | Preset

let get_position ~size =
  let open Fut.Syntax in
  match get_url_position ~size with
  | Some loc -> Fut.return (Ok (Url, loc))
  | None -> (
      let+ loc = get_current_position ~size in
      match loc with
      | Some loc -> Ok (Geolocation, loc)
      | None -> Ok (Preset, get_preset_position ()))

let create_location_ui ~size =
  let body = Brr.Document.body Brr.G.document in
  let fab =
    let el = Brr.El.button ~at:Brr.At.[ class' (Jstr.v "fab") ] [] in
    Jv.set (Brr.El.to_jv el) "innerHTML"
      (Jv.of_string
         {|<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polygon points="1 6 1 22 8 18 16 22 23 18 23 2 16 6 8 2 1 6"></polygon><line x1="8" y1="2" x2="8" y2="18"></line><line x1="16" y1="6" x2="16" y2="22"></line></svg>|});
    el
  in
  let overlay = Brr.El.div ~at:Brr.At.[ class' (Jstr.v "menu-overlay") ] [] in
  let menu = Brr.El.div ~at:Brr.At.[ class' (Jstr.v "menu") ] [] in
  let close_btn =
    Brr.El.button
      ~at:Brr.At.[ class' (Jstr.v "menu-close") ]
      [ Brr.El.txt (Jstr.v "✕") ]
  in

  Brr.El.append_children menu
    [
      close_btn;
      Brr.El.div
        ~at:Brr.At.[ class' (Jstr.v "menu-title") ]
        [ Brr.El.txt (Jstr.v "Select Location") ];
    ];
  Brr.El.append_children overlay [ menu ];
  Brr.El.append_children body [ fab; overlay ];

  (* Input Section *)
  let input =
    Brr.El.input
      ~at:
        Brr.At.
          [
            class' (Jstr.v "input-coord");
            type' (Jstr.v "text");
            placeholder (Jstr.v "Lat, Lon or Map Link");
          ]
      ()
  in

  let toggle_menu () =
    let visible = Jstr.v "visible" in
    if Brr.El.class' visible overlay then Brr.El.set_class visible false overlay
    else begin
      Brr.El.set_class visible true overlay;
      ignore (Jv.call (Brr.El.to_jv input) "focus" [||])
    end
  in

  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun _ -> toggle_menu ())
       (Brr.El.as_target fab));
  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun e ->
         if Jv.equal (Jv.get (Obj.magic e) "target") (Brr.El.to_jv overlay) then
           toggle_menu ())
       (Brr.El.as_target overlay));
  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun _ -> toggle_menu ())
       (Brr.El.as_target close_btn));

  (* Input Section *)
  let btn_go =
    Brr.El.button
      ~at:Brr.At.[ class' (Jstr.v "btn-go") ]
      [ Brr.El.txt (Jstr.v "GO") ]
  in
  let input_group =
    Brr.El.div ~at:Brr.At.[ class' (Jstr.v "input-group") ] [ input; btn_go ]
  in

  let go () =
    let text = Jv.to_string (Jv.get (Brr.El.to_jv input) "value") in
    match parse_input_coordinates text with
    | Some (lat, lon) ->
        if
          Dem_loader.in_range ~size ~min_lat:43 ~max_lat:46 ~min_lon:5
            ~max_lon:9 ~lat ~lon
        then
          let search = Jstr.v (Printf.sprintf "?lat=%f&lon=%f" lat lon) in
          let uri =
            Brr.Uri.with_query_params
              (Brr.Window.location Brr.G.window)
              (Brr.Uri.Params.of_jstr search)
          in
          navigate_to uri
        else
          Jv.set (Brr.El.to_jv input) "value"
            (Jv.of_string "Location out of range")
    | None ->
        Jv.set (Brr.El.to_jv input) "value" (Jv.of_string "Invalid coordinates")
  in

  ignore
    (Brr.Ev.listen Brr.Ev.keydown
       (fun e ->
         let code = Jstr.to_string (Brr.Ev.Keyboard.code (Brr.Ev.as_type e)) in
         match code with
         | "Enter" ->
             Brr.Ev.prevent_default e;
             Brr.Ev.stop_propagation e;
             go ()
         | "ArrowLeft" | "ArrowRight" -> Brr.Ev.stop_propagation e
         | _ -> ())
       (Brr.El.as_target input));

  ignore (Brr.Ev.listen Brr.Ev.click (fun _ -> go ()) (Brr.El.as_target btn_go));

  (* Current Location *)
  let current_loc_btn =
    Brr.El.div
      ~at:Brr.At.[ class' (Jstr.v "location-item"); tabindex 0 ]
      [
        Brr.El.span
          ~at:Brr.At.[ class' (Jstr.v "location-icon") ]
          [ Brr.El.txt (Jstr.v "📍") ];
        Brr.El.txt (Jstr.v "Use My Location");
      ]
  in

  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun _ ->
         let _ =
           let open Fut.Syntax in
           let* res = get_current_position ~size in
           match res with
           | Some (lat, lon, _) ->
               let search = Jstr.v (Printf.sprintf "?lat=%f&lon=%f" lat lon) in
               let uri =
                 Brr.Uri.with_query_params
                   (Brr.Window.location Brr.G.window)
                   (Brr.Uri.Params.of_jstr search)
               in
               navigate_to uri;
               Fut.return ()
           | None ->
               Jv.set (Brr.El.to_jv input) "value"
                 (Jv.of_string "Location out of range or unavailable");
               Fut.return ()
         in
         ())
       (Brr.El.as_target current_loc_btn));

  (* Featured Locations *)
  let location_list =
    Brr.El.ul ~at:Brr.At.[ class' (Jstr.v "location-list") ] []
  in
  let featured_items =
    List.map
      (fun (name, lat, lon, alpha) ->
        let item =
          Brr.El.li
            ~at:Brr.At.[ class' (Jstr.v "location-item"); tabindex 0 ]
            [
              Brr.El.span
                ~at:Brr.At.[ class' (Jstr.v "location-icon") ]
                [ Brr.El.txt (Jstr.v "🏔️") ];
              Brr.El.txt (Jstr.v name);
            ]
        in
        ignore
          (Brr.Ev.listen Brr.Ev.click
             (fun _ ->
               let search =
                 Jstr.v (Printf.sprintf "?lat=%f&lon=%f&alpha=%f" lat lon alpha)
               in
               let uri =
                 Brr.Uri.with_query_params
                   (Brr.Window.location Brr.G.window)
                   (Brr.Uri.Params.of_jstr search)
               in
               navigate_to uri)
             (Brr.El.as_target item));
        Brr.El.append_children location_list [ item ];
        item)
      featured_locations
  in

  let focusables = [ input; btn_go; current_loc_btn ] @ featured_items in
  let n = List.length focusables in
  List.iteri
    (fun i el ->
      ignore
        (Brr.Ev.listen Brr.Ev.keydown
           (fun e ->
             let code =
               Jstr.to_string (Brr.Ev.Keyboard.code (Brr.Ev.as_type e))
             in
             match code with
             | "ArrowDown" ->
                 Brr.Ev.prevent_default e;
                 Brr.Ev.stop_propagation e;
                 let next = List.nth focusables ((i + 1) mod n) in
                 ignore (Jv.call (Brr.El.to_jv next) "focus" [||])
             | "ArrowUp" ->
                 Brr.Ev.prevent_default e;
                 Brr.Ev.stop_propagation e;
                 let prev = List.nth focusables ((i - 1 + n) mod n) in
                 ignore (Jv.call (Brr.El.to_jv prev) "focus" [||])
             | "Escape" ->
                 Brr.Ev.prevent_default e;
                 Brr.Ev.stop_propagation e;
                 toggle_menu ()
             | "Enter" when el != input && el != btn_go ->
                 Brr.Ev.prevent_default e;
                 Brr.Ev.stop_propagation e;
                 ignore (Jv.call (Brr.El.to_jv el) "click" [||])
             | "Enter" ->
                 (* Let the specific listeners handle it, but stop propagation to window *)
                 Brr.Ev.stop_propagation e
             | _ -> ())
           (Brr.El.as_target el)))
    focusables;

  let quick_select_header =
    Brr.El.div
      ~at:Brr.At.[ class' (Jstr.v "section-title") ]
      [ Brr.El.txt (Jstr.v "Quick Select") ]
  in

  Brr.El.append_children menu
    [
      Brr.El.div
        ~at:Brr.At.[ class' (Jstr.v "section-title") ]
        [ Brr.El.txt (Jstr.v "Coordinates") ];
      input_group;
      quick_select_header;
      current_loc_btn;
      Brr.El.div
        ~at:Brr.At.[ class' (Jstr.v "section-title") ]
        [ Brr.El.txt (Jstr.v "Featured") ];
      location_list;
    ];

  fun visible ->
    let disp = Jstr.v (if visible then "flex" else "none") in
    let disp_header = Jstr.v (if visible then "block" else "none") in
    Brr.El.set_inline_style (Jstr.v "display") disp current_loc_btn;
    Brr.El.set_inline_style (Jstr.v "display") disp_header quick_select_header

let setup_events canvas =
  let deviceorientation =
    Brr.Ev.Type.create (Jstr.v "deviceorientationabsolute")
  in
  let state = ref `Init in

  (* Sensitivity for drag rotation (degrees per pixel) *)

  (* Helper: get current time in ms *)

  (* Helper: calculate distance between two touches *)
  let touch_distance touches =
    if Jv.to_int (Jv.get touches "length") >= 2 then
      let t0 = Jv.call touches "item" [| Jv.of_int 0 |] in
      let t1 = Jv.call touches "item" [| Jv.of_int 1 |] in
      let x0 = Jv.to_float (Jv.get t0 "clientX") in
      let y0 = Jv.to_float (Jv.get t0 "clientY") in
      let x1 = Jv.to_float (Jv.get t1 "clientX") in
      let y1 = Jv.to_float (Jv.get t1 "clientY") in
      Some (sqrt (((x1 -. x0) ** 2.) +. ((y1 -. y0) ** 2.)))
    else None
  in

  let toggle_fullscreen () =
    match Brr.Document.fullscreen_element Brr.G.document with
    | None ->
        ignore
          (Jv.call
             (Jv.get (Jv.get Jv.global "screen") "orientation")
             "lock"
             [| Jv.of_jstr (Jstr.v "portrait") |]);
        ignore
          (Brr.El.request_fullscreen
             ~opts:
               (Brr.El.fullscreen_opts ~navigation_ui:Brr.El.Navigation_ui.hide
                  ())
             (Brr.Document.body Brr.G.document))
    | Some _ -> ignore (Brr.Document.exit_fullscreen Brr.G.document)
  in

  let handle_tap () =
    let now = now_ms () in
    if now -. !last_tap_time < double_tap_threshold then begin
      (* Double tap - switch back to sensor mode *)
      if !input_mode = Manual then begin
        input_mode := Sensor;
        display_temporary_message "Sensor mode"
      end;
      last_tap_time := 0.
    end
    else begin
      (* First tap - toggle fullscreen only in Sensor mode *)
      if !input_mode = Sensor then toggle_fullscreen ();
      last_tap_time := now
    end
  in

  (* Device orientation listener - only active in Sensor mode *)
  ignore
    (Brr.Ev.listen deviceorientation
       (fun ev ->
         (* Bogus event on Chrome desktop *)
         if not (Jv.is_null (Jv.get (Brr.Ev.as_type ev) "alpha")) then
           let screen =
             Jv.to_float
               (Jv.get
                  (Jv.get (Jv.get Jv.global "screen") "orientation")
                  "angle")
           in
           if !input_mode = Sensor then begin
             let angle nm = Jv.to_float (Jv.get (Brr.Ev.as_type ev) nm) in
             let alpha = angle "alpha" in
             let beta = angle "beta" in
             let gamma = angle "gamma" in
             let q =
               Quaternion.(
                 mult
                   (from_axis_angle
                      { x = 0.; y = 0.; z = 1.; w = 0. }
                      (alpha *. pi /. 180.))
                   (mult
                      (from_axis_angle
                         { x = 1.; y = 0.; z = 0.; w = 0. }
                         (beta *. pi /. 180.))
                      (mult
                         (from_axis_angle
                            { x = 0.; y = 1.; z = 0.; w = 0. }
                            (gamma *. pi /. 180.))
                         (from_axis_angle
                            { x = 0.; y = 0.; z = 1.; w = 0. }
                            (-.screen *. pi /. 180.)))))
             in
             target_orientation := q;
             match !state with
             | `Init -> current_orientation := q
             | `Starting ->
                 state := `Started;
                 if beta < 80. then (
                   Lwt.async @@ fun () ->
                   let* () = sleep 1.1 in
                   display_temporary_message "Raise your phone!";
                   Lwt.return ())
             | `Started -> if beta >= 80. then remove_message ()
           end)
       (Brr.Window.as_target Brr.G.window));

  (* We set the device orientation listener early so that we have the
     correct orientation when we start rendering. The other
     controllers are only set after initialization. *)
  fun () ->
    (* Keyboard controls *)
    ignore
      (Brr.Ev.listen Brr.Ev.keydown
         (fun ev ->
           match Jstr.to_string (Brr.Ev.Keyboard.code (Brr.Ev.as_type ev)) with
           | "ArrowLeft" ->
               (* Yaw Left: 5 degrees *)
               input_mode := Manual;
               current_orientation :=
                 apply_manual_rotation !current_orientation
                   (5. *. pi /. 180.)
                   0.
           | "ArrowRight" ->
               (* Yaw Right: -5 degrees *)
               input_mode := Manual;
               current_orientation :=
                 apply_manual_rotation !current_orientation
                   (-5. *. pi /. 180.)
                   0.
           | "ArrowDown" ->
               (* Pitch Down: -5 degrees *)
               input_mode := Manual;
               current_orientation :=
                 apply_manual_rotation !current_orientation 0.
                   (-5. *. pi /. 180.)
           | "ArrowUp" ->
               (* Pitch Up: 5 degrees *)
               input_mode := Manual;
               current_orientation :=
                 apply_manual_rotation !current_orientation 0. (5. *. pi /. 180.)
           | "Equal" | "NumpadAdd" -> zoom := min max_zoom (!zoom *. 1.1)
           | "Minus" | "NumpadSubtract" -> zoom := max min_zoom (!zoom /. 1.1)
           | _ -> ())
         (Brr.Window.as_target Brr.G.window));

    (* Mouse controls *)
    let target = Brr.El.as_target canvas in

    (* Mouse wheel for zoom *)
    ignore
      (Brr.Ev.listen Brr.Ev.wheel
         (fun ev ->
           Brr.Ev.prevent_default ev;
           let wheel = Brr.Ev.as_type ev in
           let delta_y = Brr.Ev.Wheel.delta_y wheel in
           let factor = (if delta_y > 0. then 0.9 else 1.1) ** 0.25 in
           zoom := max min_zoom (min max_zoom (!zoom *. factor)))
         target);

    (* Mouse drag for rotation *)
    ignore
      (Brr.Ev.listen Brr.Ev.mousedown
         (fun ev ->
           let mouse = Brr.Ev.as_type ev in
           let x = Brr.Ev.Mouse.client_x mouse in
           let y = Brr.Ev.Mouse.client_y mouse in
           mouse_dragging := true;
           is_dragging := true;
           velocity := (0., 0.);
           last_input_time := now ();
           mouse_start_x := x;
           mouse_start_y := y;
           mouse_last_x := x;
           mouse_last_y := y)
         target);

    ignore
      (Brr.Ev.listen Brr.Ev.mousemove
         (fun ev ->
           if !mouse_dragging then begin
             input_mode := Manual;
             let mouse = Brr.Ev.as_type ev in
             let x = Brr.Ev.Mouse.client_x mouse in
             let y = Brr.Ev.Mouse.client_y mouse in
             let dx = x -. !mouse_last_x in
             let dy = y -. !mouse_last_y in
             let h =
               Jv.to_float (Jv.get (Brr.El.to_jv canvas) "clientHeight")
             in
             let w = Jv.to_float (Jv.get (Brr.El.to_jv canvas) "clientWidth") in
             let speed =
               2.0 /. (max w h *. scale *. !zoom) *. 180. /. Float.pi
             in
             let gamma =
               0.
               (* !current_orientation.screen *. Float.pi /. 180. *)
             in
             let c = cos gamma in
             let s = sin gamma in
             let dx_eff = (dx *. c) -. (dy *. s) in
             let dy_eff = (dx *. s) +. (dy *. c) in
             let da = dx_eff *. speed in
             let db = dy_eff *. speed in
             let t = now () in
             let dt = t -. !last_input_time in
             last_input_time := t;
             if dt > 0. then begin
               let v_inst_x = da /. dt in
               let v_inst_y = db /. dt in
               let vx, vy = !velocity in
               (* Time-based smoothing (50ms window) to handle variable polling rates *)
               let alpha = 1. -. exp (-.dt /. 50.) in
               velocity :=
                 ( (v_inst_x *. alpha) +. (vx *. (1. -. alpha)),
                   (v_inst_y *. alpha) +. (vy *. (1. -. alpha)) )
             end;
             is_dragging := true;
             mouse_last_x := x;
             mouse_last_y := y;
             current_orientation :=
               let da_rad = da *. Float.pi /. 180. in
               let db_rad = db *. Float.pi /. 180. in
               apply_manual_rotation !current_orientation da_rad db_rad
           end)
         (Brr.Window.as_target Brr.G.window));

    ignore
      (Brr.Ev.listen Brr.Ev.mouseup
         (fun ev ->
           if !mouse_dragging then begin
             mouse_dragging := false;
             is_dragging := false;
             (* Removed inertia killer timeout *)
             let mouse = Brr.Ev.as_type ev in
             let x = Brr.Ev.Mouse.client_x mouse in
             let y = Brr.Ev.Mouse.client_y mouse in
             let dx = x -. !mouse_start_x in
             let dy = y -. !mouse_start_y in
             let dist = sqrt ((dx ** 2.) +. (dy ** 2.)) in
             if dist < drag_threshold then handle_tap ()
           end)
         (Brr.Window.as_target Brr.G.window));

    (* Touch controls *)
    let touchstart = Brr.Ev.Type.create (Jstr.v "touchstart") in
    let touchmove = Brr.Ev.Type.create (Jstr.v "touchmove") in
    let touchend = Brr.Ev.Type.create (Jstr.v "touchend") in

    ignore
      (Brr.Ev.listen touchstart
         (fun ev ->
           let touches = Jv.get (Brr.Ev.as_type ev) "touches" in
           let num_touches = Jv.to_int (Jv.get touches "length") in
           if num_touches = 1 then begin
             (* Single touch - potential drag or tap *)
             let t0 = Jv.call touches "item" [| Jv.of_int 0 |] in
             let x = Jv.to_float (Jv.get t0 "clientX") in
             let y = Jv.to_float (Jv.get t0 "clientY") in
             touch_start_x := x;
             touch_start_y := y;
             touch_last_x := x;
             touch_last_y := y;
             touch_dragging := false;
             is_dragging := true;
             velocity := (0., 0.);
             last_input_time := now ()
           end
           else if num_touches >= 2 then begin
             (* Two or more fingers - pinch zoom *)
             Brr.Ev.prevent_default ev;
             match touch_distance touches with
             | Some d -> pinch_distance := d
             | None -> ()
           end)
         target);

    ignore
      (Brr.Ev.listen touchmove
         (fun ev ->
           let touches = Jv.get (Brr.Ev.as_type ev) "touches" in
           let num_touches = Jv.to_int (Jv.get touches "length") in
           if num_touches = 1 then begin
             let t0 = Jv.call touches "item" [| Jv.of_int 0 |] in
             let x = Jv.to_float (Jv.get t0 "clientX") in
             let y = Jv.to_float (Jv.get t0 "clientY") in
             let dx = x -. !touch_last_x in
             let dy = y -. !touch_last_y in
             let total_dx = x -. !touch_start_x in
             let total_dy = y -. !touch_start_y in
             let total_dist = sqrt ((total_dx ** 2.) +. (total_dy ** 2.)) in
             (* Start dragging if moved beyond threshold *)
             if (not !touch_dragging) && total_dist > drag_threshold then begin
               touch_dragging := true;
               (* Switch to manual mode when user starts dragging *)
               if !input_mode = Sensor then begin
                 input_mode := Manual;
                 display_temporary_message "Manual mode"
               end;
               Brr.Ev.prevent_default ev
             end;
             if !touch_dragging then begin
               let h =
                 Jv.to_float (Jv.get (Brr.El.to_jv canvas) "clientHeight")
               in
               let w =
                 Jv.to_float (Jv.get (Brr.El.to_jv canvas) "clientWidth")
               in
               let speed =
                 2.0 /. (max w h *. scale *. !zoom) *. 180. /. Float.pi
               in
               let gamma =
                 0.
                 (* !current_orientation.screen *. Float.pi /. 180. *)
               in
               let c = cos gamma in
               let s = sin gamma in
               let dx_eff = (dx *. c) -. (dy *. s) in
               let dy_eff = (dx *. s) +. (dy *. c) in
               let da = dx_eff *. speed in
               let db = dy_eff *. speed in
               let t = now () in
               let dt = t -. !last_input_time in
               last_input_time := t;
               if dt > 0. then begin
                 let v_inst_x = da /. dt in
                 let v_inst_y = db /. dt in
                 let vx, vy = !velocity in
                 (* Smoothing: mix history (0.6) with new (0.4) *)
                 velocity :=
                   ( (v_inst_x *. 0.4) +. (vx *. 0.6),
                     (v_inst_y *. 0.4) +. (vy *. 0.6) )
               end;
               is_dragging := true;
               touch_last_x := x;
               touch_last_y := y;
               current_orientation :=
                 let da_rad = da *. Float.pi /. 180. in
                 let db_rad = db *. Float.pi /. 180. in
                 apply_manual_rotation !current_orientation da_rad db_rad
             end
           end
           else if num_touches >= 2 then begin
             (* Pinch zoom *)
             Brr.Ev.prevent_default ev;
             match touch_distance touches with
             | Some d when !pinch_distance > 0. ->
                 let factor = d /. !pinch_distance in
                 zoom := max min_zoom (min max_zoom (!zoom *. factor));
                 pinch_distance := d
             | _ -> ()
           end)
         target);

    ignore
      (Brr.Ev.listen touchend
         (fun ev ->
           let touches = Jv.get (Brr.Ev.as_type ev) "touches" in
           let num_remaining = Jv.to_int (Jv.get touches "length") in

           if num_remaining = 1 then begin
             (* Resync drag state when switching to 1 finger (e.g. end of pinch) *)
             let t0 = Jv.call touches "item" [| Jv.of_int 0 |] in
             let x = Jv.to_float (Jv.get t0 "clientX") in
             let y = Jv.to_float (Jv.get t0 "clientY") in
             touch_start_x := x;
             touch_start_y := y;
             touch_last_x := x;
             touch_last_y := y;
             touch_dragging := false;
             pinch_distance := 0.
           end;

           if num_remaining = 0 then begin
             (* All fingers lifted *)
             is_dragging := false;
             if now () -. !last_input_time > 300. then velocity := (0., 0.);
             if not !touch_dragging then begin
               Brr.Ev.prevent_default ev;
               (* This was a tap, not a drag *)
               handle_tap ()
             end;
             touch_dragging := false;
             pinch_distance := 0.
           end)
         target);

    (* Use ResizeObserver to detect canvas size changes *)
    let observer_cb =
      Jv.callback ~arity:1 (fun entries ->
          let len = Jv.to_int (Jv.get entries "length") in
          if len > 0 then begin
            let entry = Jv.call entries "at" [| Jv.of_int 0 |] in
            let device_box = Jv.get entry "devicePixelContentBoxSize" in
            let device_width, device_height =
              if Jv.is_undefined device_box then (None, None)
              else
                let box = Jv.call device_box "at" [| Jv.of_int 0 |] in
                ( Some (Jv.to_int (Jv.get box "inlineSize")),
                  Some (Jv.to_int (Jv.get box "blockSize")) )
            in
            resize_canvas ?device_width ?device_height canvas;
            force_redraw := true
          end)
    in
    let observer =
      Jv.new' (Jv.get Jv.global "ResizeObserver") [| observer_cb |]
    in
    ignore (Jv.call observer "observe" [| Brr.El.to_jv canvas |]);

    state := `Starting

let wait_for_service_worker =
  let open Fut.Result_syntax in
  let open Brr_webworkers.Service_worker in
  let* r = Container.ready (Container.of_navigator Brr.G.navigator) in
  match Registration.active r with
  | None -> assert false
  | Some r ->
      if state r = State.activated then Fut.return (Ok ())
      else
        let fut, set = Fut.create () in
        ignore
          (Brr.Ev.listen Brr.Ev.statechange
             (fun _ -> if state r = State.activated then set (Ok ()))
             (as_target r));
        fut
(* Main *)

let main () =
  let tile_width = 4096 in
  let set_loc_visible = create_location_ui ~size:tile_width in
  let tile_height = tile_width in
  (* Check that we are close to a power of two *)
  assert (tile_width land (tile_width - 1) = 0);
  let canvas =
    Option.get (Brr.Document.find_el_by_id Brr.G.document (Jstr.v "canvas"))
  in
  let ctx =
    Option.get
      (Brr_canvas.Gl.get_context ~attrs:(Gl.Attrs.v ())
         (Brr_canvas.Canvas.of_el canvas))
  in
  (* Initialize anisotropic filtering early for the detail map *)
  init_anisotropic_filtering ctx;
  (* Start loading detail map immediately *)
  let detail_map = make_detail_map ctx in
  Worker_pool.init ();
  let graphics = init_graphics ctx in
  resize_canvas canvas;

  update_startup_status "Getting current location..." false;
  let* () = to_lwt wait_for_service_worker in
  let* source, (lat, lon, angle) = to_lwt (get_position ~size:tile_width) in
  (* If URL parameters were provided but we fell back to another source (e.g. out of range),
     redirect to the resolved location. *)
  (match source with
  | Preset -> set_loc_visible false
  | Url -> ()
  | _ ->
      let params = Brr.Uri.query_params (Brr.Window.location Brr.G.window) in
      if Brr.Uri.Params.mem (Jstr.v "lat") params then
        let search =
          Jstr.v (Printf.sprintf "?lat=%f&lon=%f&alpha=%f" lat lon angle)
        in
        let uri =
          Brr.Uri.with_query_params
            (Brr.Window.location Brr.G.window)
            (Brr.Uri.Params.of_jstr search)
        in
        navigate_to uri);
  (* current_orientation := { alpha = angle; beta = 90.; gamma = 0.; screen = 0. }; *)
  set_orientation_from_yaw angle;

  let start = setup_events canvas in
  update_startup_status "Loading Terrain..." true;

  (* Load DEM and CLC (for POIs) in parallel *)
  (* Load DEM, CLC, and init graphics in parallel *)
  let tile_loaders =
    Lwt.both
      (Dem_loader.load ~size:tile_width ~lat ~lon)
      (Clc_loader.load_tiles ~lat ~lon ~size:tile_width)
  in
  let* tile, (_, _, _, _, tiles) = tile_loaders in

  if source = Geolocation then (
    Lwt.async (fun () -> Dem_loader.prefetch ~size:7200 ~lat ~lon);
    Lwt.async (fun () -> Clc_loader.prefetch ~size:7200 ~lat ~lon));
  let x = tile_width / 2 in
  let y = (tile_height / 2) - 1 in
  let d = float x /. 3600. in
  let tile_coord = { Points.lon = lon -. d; lat = lat -. d } in
  let tile_coord' = { Points.lon = lon +. d; lat = lat +. d } in
  let* points =
    (* Extract POIs from all loaded tiles and convert to Points.t format *)
    let pois =
      List.concat_map
        (fun (tile, _, _) ->
          List.map
            (fun (poi : Clc_loader.poi) ->
              {
                Points.name = poi.name;
                coord =
                  {
                    Points.lat = floor ((poi.lat *. 3600.) +. 0.5) /. 3600.;
                    lon = floor ((poi.lon *. 3600.) +. 0.5) /. 3600.;
                  };
                elevation =
                  (if poi.elevation = 0 then None else Some poi.elevation);
              })
            tile.Clc_loader.pois)
        tiles
    in
    (* Filter POIs within the visible tile bounds *)
    let filtered =
      List.filter
        (fun { Points.coord = { lat = pt_lat; lon = pt_lon }; _ } ->
          tile_coord.lat < pt_lat && pt_lat < tile_coord'.lat
          && tile_coord.lon < pt_lon && pt_lon < tile_coord'.lon)
        pois
    in
    Lwt.return
      (filtered
      |> List.map
           (let size = tile_width in
            let center_lon_int = truncate (lon *. 3600.) in
            let center_lat_int = truncate (lat *. 3600.) in
            let min_lon_int = center_lon_int - (size / 2) in
            (* Top row index in arcseconds. See Clc_loader/Dem_loader bounds logic *)
            let max_lat_int = center_lat_int + (size / 2) - 1 in
            fun ({ Points.coord = { lat = pt_lat; lon = pt_lon }; _ } as pt) ->
              (* POI coords are already rounded to arcseconds in previous step, so simple truncate is safe *)
              let pt_lon_int = truncate (pt_lon *. 3600.) in
              let pt_lat_int = truncate (pt_lat *. 3600.) in
              let x = max 0 (min (tile_width - 1) (pt_lon_int - min_lon_int)) in
              let y =
                max 0 (min (tile_height - 1) (max_lat_int - pt_lat_int))
              in
              (pt, (x, y))))
  in
  if false then (
    let w = 4 in
    let a = Array.make (((2 * w) + 1) * ((2 * w) + 1)) 0 in
    let count = ref 0 in
    let dxs = ref 0 in
    let dys = ref 0 in
    List.iter
      (fun (_, (x, y)) ->
        if x > w && y > w && x < tile_width - w - 1 && y < tile_height - w - 1
        then (
          let get_h = Dem_loader.get_height tile in
          let max_h = ref (get_h y x) in
          let dx = ref 0 in
          let dy = ref 0 in
          for i = -w to w do
            for j = -w to w do
              let y' = y + i in
              let x' = x + j in
              assert (x' >= 0 && y' >= 0 && x' < tile_width && y' < tile_width);
              let h = get_h y' x' in
              if h > !max_h then (
                max_h := h;
                dy := i;
                dx := j)
            done
          done;
          let x = x + !dx in
          let y = y + !dy in
          let is_peak = ref true in
          for i = -1 to 1 do
            for j = -1 to 1 do
              let y' = y + i in
              let x' = x + j in
              assert (x' >= 0 && y' >= 0 && x' < tile_width && y' < tile_width);
              let h = get_h y' x' in
              if h > !max_h then is_peak := false
            done
          done;
          if !is_peak then (
            let p = (((2 * w) + 1) * (w + !dy)) + w + !dx in
            a.(p) <- a.(p) + 1;
            incr count;
            (*
            Format.eprintf "- %d %d@." !dx !dy;
*)
            dxs := !dxs + !dx;
            dys := !dys + !dy)))
      points;
    Format.eprintf "Mean offsets: %f %f (%d)@."
      (float !dxs /. float !count)
      (float !dys /. float !count)
      !count;
    for i = 0 to 2 * w do
      for j = 0 to 2 * w do
        Format.eprintf "%d " a.((((2 * w) + 1) * i) + j)
      done;
      Format.eprintf "@."
    done);

  (* Bilinear interpolation for height *)
  let get_h = Dem_loader.get_height tile in
  let off_x = Render_state.compute_sub_arcsec_offset lon in
  let off_y = Render_state.compute_sub_arcsec_offset lat in
  let h00 = get_h y x in
  let h10 = get_h y (x + 1) in
  let h01 = get_h (y - 1) x in
  let h11 = get_h (y - 1) (x + 1) in
  let h0 = h00 +. (off_x *. (h10 -. h00)) in
  let h1 = h01 +. (off_x *. (h11 -. h01)) in
  let height = h0 +. (off_y *. (h1 -. h0)) in

  let debug = false in
  let points =
    List.filter
      (fun ({ Points.name; _ }, (dst_x, dst_y)) ->
        if
          (not debug)
          || (String.length name > 8 && String.sub name 0 7 = "Grand G")
        then (
          if debug then prerr_endline name;

          Visibility.test_precise
            (Dem_loader.get_height tile)
            ~src_h:(height +. 2.) ~off_x ~off_y ~src_x:x ~src_y:y ~dst_x ~dst_y
            ())
        else
          Visibility.test
            (Dem_loader.get_height tile)
            ~src_h:(height +. 2.) ~src_x:x ~src_y:y ~dst_x ~dst_y ())
      points
  in

  let* () =
    tri ~w:tile_width ~h:tile_height ~x ~y ~height ~lat ~lon ~points ~tile
      canvas ctx ~detail_map ~clc_tiles:tiles ~graphics ~start
  in
  Lwt.return_unit

let () =
  let open Brr_webworkers.Service_worker in
  ignore
    (Container.register
       (Container.of_navigator Brr.G.navigator)
       (Jstr.v "service_worker.bc.js"))

let () =
  Lwt.async (fun () ->
      Lwt.catch main (fun e ->
          (match e with Jv.Error e -> Brr.Console.error [ e ] | _ -> ());
          display_message (Printexc.to_string e);
          Lwt.fail e))
