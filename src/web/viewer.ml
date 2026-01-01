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
let message = ref None

let remove_message () =
  match !message with
  | Some msg ->
      Brr.El.remove msg;
      message := None
  | None -> ()

let display_message msg =
  remove_message ();
  let msg = Brr.El.(v (Jstr.v "div") [ txt (Jstr.v msg) ]) in
  Brr.El.append_children (Brr.Document.body Brr.G.document) [ msg ];
  message := Some msg

let display_temporary_message msg =
  display_message msg;
  ignore (Brr.G.set_timeout ~ms:10000 remove_message)

module Loader = Loader.Make (Reader)

let pi = 4. *. atan 1.

(* Shaders *)

let deltay = 40_000. /. 360. /. 3600. *. 1000.

type program = {
  vertex_shader : string;
  fragment_shader : string;
  attributes : string list;
}

let n_sectors = 512
let n_rings = 1024

type orientation = {
  alpha : float;
  beta : float;
  gamma : float;
  screen : float;
}

let rec next_power_of_two n p = if n <= p then p else next_power_of_two n (p + p)

let compute_azimuth m =
  let v_up = Matrix.(m *> { x = 0.; y = 1.; z = 0.; w = 0. }) in
  let v_fwd = Matrix.(m *> { x = 0.; y = 0.; z = -1.; w = 0. }) in
  let len_up = (v_up.x ** 2.) +. (v_up.y ** 2.) in
  let len_fwd = (v_fwd.x ** 2.) +. (v_fwd.y ** 2.) in
  let azimuth =
    if len_up > len_fwd then atan2 v_up.y v_up.x else atan2 v_fwd.y v_fwd.x
  in
  azimuth -. (pi /. 2.)

let terrain_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;  // Ensure all floats use high precision (critical for mobile)
        precision highp int;
        uniform mat4 proj;
        uniform mat4 transform;
        uniform highp int w;      // Must be highp for accurate coord calculations
        uniform highp int w_mask;
        uniform highp int w_shift;
        uniform highp vec2 delta;
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
        out highp float v_dist;
        out highp float v_h;
        out highp vec2 reliefCoord;
        out highp vec3 v_world_pos;
        out highp float v_ring;  // For debug visualization

        void main()
        {
          const float PI = 3.14159265359;
          int sector = gl_VertexID & w_mask;
          int ring = gl_VertexID >> w_shift;
          float theta = (float(sector) * inv_sectors_div) * (PI / 2.0) - (PI / 4.0);
          float angle = theta + snapped_alpha + (PI / 2.0);
          // Use exp(k*ring) instead of pow(base, ring) for more consistent cross-platform behavior
          // For ring=0, exp(0.0)-1.0 = 0.0 exactly
          highp float r = grid_scale * (exp(grid_k * float(ring)) - 1.0);
          highp vec2 pos_plane = vec2(cos(angle), sin(angle)) * r;
          highp vec2 coord_meters = center_offset + pos_plane;
          highp vec2 coord = coord_meters * inv_delta;

          // Calculate approximate grid spacing in meters
          // For exponential grid r = A(B^i - 1), dr = k(r + A), ds = kr
          // k = grid_k, A = grid_scale
          highp float grid_spacing = grid_k * (r + grid_scale);

          // LOD level
          highp float lod_f = max(0.0, log2(grid_spacing * inv_avg_delta));
          int lod = min(int(lod_f), max_lod);

          // Texture Size at this LOD
          ivec2 tex_size = textureSize(relief, lod);

          // Manual bilinear interpolation for 2-byte height
          // Normalized Coordinate (0..1)
          highp vec2 norm_coord = vec2(coord.x, float(w) - 1.0 - coord.y) * inv_w;
          norm_coord = clamp(norm_coord, 0.0, 1.0);

          // Coordinate in LOD texels
          highp vec2 lod_pos = norm_coord * vec2(tex_size);

          // Manual Bilinear Interpolation
          highp vec2 lod_tex_pos = clamp(lod_pos, vec2(0.5), vec2(tex_size) - 0.5);
          highp vec2 base_f = floor(lod_tex_pos - 0.5);
          highp ivec2 base = ivec2(base_f);
          highp vec2 f = fract(lod_tex_pos - 0.5);
          highp ivec2 w_max = tex_size - 1;

          // Vectorized Fetch & Decode
          highp vec2 s00 = texelFetch(relief, clamp(base + ivec2(0,0), ivec2(0), w_max), lod).rg;
          highp vec2 s10 = texelFetch(relief, clamp(base + ivec2(1,0), ivec2(0), w_max), lod).rg;
          highp vec2 s01 = texelFetch(relief, clamp(base + ivec2(0,1), ivec2(0), w_max), lod).rg;
          highp vec2 s11 = texelFetch(relief, clamp(base + ivec2(1,1), ivec2(0), w_max), lod).rg;

          highp vec4 R = vec4(s00.r, s10.r, s01.r, s11.r);
          highp vec4 G = vec4(s00.g, s10.g, s01.g, s11.g);
          highp vec4 H = (R * 256.0 + G) * ((1.0/257.0) * 9500.0) - 500.0;

          highp float h0 = mix(H.x, H.y, f.x);
          highp float h1 = mix(H.z, H.w, f.x);
          highp float z = mix(h0, h1, f.y);

          reliefCoord = norm_coord + (0.5 * inv_w);

          v_world_pos = vec3(coord_meters, z);

          vec4 pos = transform * vec4(pos_plane, z, 1.0);
          v_dist = length(pos.xyz);
          v_h = z;
          v_ring = float(ring);
          gl_Position = proj * pos;
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        precision highp sampler2DArray;

        uniform mediump sampler2D relief;
        uniform mediump sampler2D noise;
        uniform mediump sampler2D ao;
        uniform mediump sampler2D rock_texture;  // Detail texture (triplanar)
        uniform mediump sampler2D rock_normal_map;  // Normal map (triplanar)
        uniform highp sampler2DArrayShadow shadow_map;  // Hardware shadow comparison
        
        // CLC Material System Uniforms
        uniform mediump usampler2D u_coverMap;   // CLC ID texture (R8UI)
        uniform mediump sampler2D u_paletteTex;  // 128x1 RGBA palette
        uniform highp vec2 u_coverMapOffset;     // World position of CLC texture origin
        uniform highp float u_coverMapScale;     // Meters per CLC texel
        uniform bool u_useCLC;                   // Enable CLC system (for gradual rollout)
        
        uniform mat4 shadow_matrices[3];
        uniform float shadow_splits[3];

        in highp vec2 reliefCoord;
        in highp float v_dist;
        in highp float v_h;
        in highp vec3 v_world_pos;
        in highp float v_ring;  // For debug visualization

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
          // 2 pixels per material in 128-wide texture
          float u = (id * 2.0 + 0.5) / 128.0;
          vec4 pixelA = texture(u_paletteTex, vec2(u, 0.5));
          vec4 pixelB = texture(u_paletteTex, vec2(u + 1.0/128.0, 0.5));
          
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
        Surface sampleCLCBilinear(vec2 worldPos) {
          // Convert world position to CLC texture coords
          vec2 texCoord = (worldPos - u_coverMapOffset) / u_coverMapScale;
          vec2 texSize = vec2(textureSize(u_coverMap, 0));
          vec2 texelPos = texCoord * texSize - 0.5;
          
          ivec2 p00 = ivec2(floor(texelPos));
          vec2 frac = fract(texelPos);
          
          // Clamp to valid range
          ivec2 maxCoord = ivec2(texSize) - 1;
          p00 = clamp(p00, ivec2(0), maxCoord);
          ivec2 p10 = clamp(p00 + ivec2(1,0), ivec2(0), maxCoord);
          ivec2 p01 = clamp(p00 + ivec2(0,1), ivec2(0), maxCoord);
          ivec2 p11 = clamp(p00 + ivec2(1,1), ivec2(0), maxCoord);
          
          // Sample 4 neighbors as integer IDs
          float id00 = float(texelFetch(u_coverMap, p00, 0).r);
          float id10 = float(texelFetch(u_coverMap, p10, 0).r);
          float id01 = float(texelFetch(u_coverMap, p01, 0).r);
          float id11 = float(texelFetch(u_coverMap, p11, 0).r);
          
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
        
        // Modify CLC surface based on slope (steep = rock)
        void applySlopeModification(inout Surface s, float slope) {
          // Steep slopes force rock regardless of CLC classification
          float rockForce = smoothstep(0.15, 0.5, slope);
          
          if (rockForce > 0.01) {
            // Rock color (grey-brown, linear space)
            vec3 rockAlbedo = vec3(0.09, 0.08, 0.065);  // ~(76, 72, 65) in sRGB
            
            // Blend albedo towards rock based on slope
            s.albedo = mix(s.albedo, rockAlbedo, rockForce);
            
            // Also increase roughness for rock
            s.roughness = mix(s.roughness, 0.7, rockForce);
            
            // Transfer weight from other channels to rock
            float transferFromIce = rockForce * s.detailWeights.a;
            float transferFromGrass = rockForce * s.detailWeights.g * 0.8;
            float transferFromForest = rockForce * s.detailWeights.b * 0.5;
            
            s.detailWeights.r += transferFromIce + transferFromGrass + transferFromForest;
            s.detailWeights.a -= transferFromIce;
            s.detailWeights.g -= transferFromGrass;
            s.detailWeights.b -= transferFromForest;
            
            // Normalize weights
            float total = dot(s.detailWeights, vec4(1.0));
            if (total > 0.01) {
              s.detailWeights /= total;
            }
          }
        }
        
        // Sample packed detail texture with triplanar mapping
        vec4 sampleDetailTriplanar(vec3 worldPos, vec3 normal) {
          float scale = 0.01;  // ~100m per texture repeat
          vec2 uv_xz = worldPos.xz * scale;
          vec2 uv_xy = worldPos.xy * scale;
          vec2 uv_yz = worldPos.yz * scale;
          
          vec3 blend = abs(normal);
          blend /= (blend.x + blend.y + blend.z + 0.0001);
          
          // Use rock_texture as detail map (R channel for all detail types for now)
          // Future: use packed RGBA texture with separate detail per channel
          vec3 d_xz = texture(rock_texture, uv_xz).rgb;
          vec3 d_xy = texture(rock_texture, uv_xy).rgb;
          vec3 d_yz = texture(rock_texture, uv_yz).rgb;
          
          vec3 blended = d_xz * blend.z + d_xy * blend.y + d_yz * blend.x;
          float detail = dot(blended, vec3(0.33));  // Luminance
          
          return vec4(detail, detail, detail, detail);  // Same detail for all channels
        }
        
        // Procedural water with organic shoreline
        float getWaterMask(vec2 worldPos, float waterFactor) {
          // High-frequency noise for organic shoreline edge
          float noise_val = texture(noise, worldPos * 0.002).r;
          float jitter = (noise_val - 0.5) * 0.25;  // +/- 12.5% variation
          
          // Sharp threshold with smooth transition
          float threshold = 0.5 + jitter;
          return smoothstep(threshold - 0.15, threshold + 0.15, waterFactor);
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
        
        float sample_shadow(int layer, vec2 coords, float compare) {
             return texture(shadow_map, vec4(coords, float(layer), compare));
        }

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
        
        void main() {
          // Decode normal from relief texture
          mediump vec2 encodedN = texture(relief, reliefCoord).ba;
          highp vec3 normal;
          normal.xy = encodedN * 2.0 - 1.0;
          normal.z = sqrt(max(0.0, 1.0 - dot(normal.xy, normal.xy)));

          vec3 lightDir = normalize(vec3(-1.0, 1.0, 2.0));
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
          
          if (u_useCLC) {
            // === NEW: CLC-Based Material ===
            Surface surface = sampleCLCBilinear(v_world_pos.xy);
            applySlopeModification(surface, slope);
            
            // Sample detail texture
            vec4 detailSample = sampleDetailTriplanar(v_world_pos, normal);
            float detailMod = dot(surface.detailWeights, detailSample);
            
            // Apply detail modulation to albedo
            terrain_color = surface.albedo * (0.7 + 0.6 * detailMod);
            
            // Water handling
            float waterMask = getWaterMask(v_world_pos.xy, surface.waterFactor);
            terrain_color = applyWaterEffects(terrain_color, waterMask, v_world_pos.xy);
            
            // Normal perturbation based on detail weights
            float tex_scale = 0.01;
            vec2 uv_xz = v_world_pos.xz * tex_scale;
            vec2 uv_xy = v_world_pos.xy * tex_scale;
            vec2 uv_yz = v_world_pos.yz * tex_scale;
            
            vec3 blend = abs(normal);
            blend /= (blend.x + blend.y + blend.z + 0.0001);
            
            vec3 n_xz = texture(rock_normal_map, uv_xz).rgb * 2.0 - 1.0;
            vec3 n_xy = texture(rock_normal_map, uv_xy).rgb * 2.0 - 1.0;
            vec3 n_yz = texture(rock_normal_map, uv_yz).rgb * 2.0 - 1.0;
            vec3 rock_detail = n_xz * blend.z + n_xy * blend.y + n_yz * blend.x;
            
            // Weight normal perturbation by rock weight AND roughness
            // Smooth surfaces (low roughness) should have less normal detail
            float perturbStrength = surface.detailWeights.r * 3.0 + 
                                   surface.detailWeights.g * 0.5 + 
                                   surface.detailWeights.b * 1.0;
            perturbStrength *= surface.roughness;  // Scale by roughness
            
            vec3 perturbed = normal;
            perturbed.xy += rock_detail.xy * perturbStrength;
            final_normal = normalize(perturbed);
            
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
            float skyReflect = max(0.0, reflectDir.z);  // Simple sky gradient
            vec3 envColor = mix(vec3(0.6, 0.7, 0.9), vec3(0.2, 0.4, 0.8), skyReflect);
            
            // Apply specular and reflection to terrain color
            vec3 specColor = vec3(1.0, 0.98, 0.95) * specular * (1.0 - material_roughness) * shadow_val;
            terrain_color += specColor * 0.3;
            terrain_color = mix(terrain_color, envColor, reflectivity * 0.3);
            
          } else {
            // === FALLBACK: Original slope-based biome logic ===
            vec3 c_water = vec3(0.05, 0.25, 0.45);
            vec3 c_grass = vec3(0.1, 0.4, 0.15);
            vec3 c_rock  = vec3(0.3, 0.28, 0.25);

            if (v_h < 0.0) {
               terrain_color = c_water;
            } else {
               float tex_scale = 0.01;
               vec2 uv_xz = v_world_pos.xz * tex_scale;
               vec2 uv_xy = v_world_pos.xy * tex_scale;
               vec2 uv_yz = v_world_pos.yz * tex_scale;
               
               vec3 blend = abs(normal);
               blend = blend / (blend.x + blend.y + blend.z + 0.0001);
               
               float rock_mixin = smoothstep(0.15, 0.5, slope);
               
               vec3 tex_xz = texture(rock_texture, uv_xz).rgb;
               vec3 tex_xy = texture(rock_texture, uv_xy).rgb;
               vec3 tex_yz = texture(rock_texture, uv_yz).rgb;
               vec3 rock_tex = tex_xz * blend.z + tex_xy * blend.y + tex_yz * blend.x;
               
               vec3 n_xz = texture(rock_normal_map, uv_xz).rgb * 2.0 - 1.0;
               vec3 n_xy = texture(rock_normal_map, uv_xy).rgb * 2.0 - 1.0;
               vec3 n_yz = texture(rock_normal_map, uv_yz).rgb * 2.0 - 1.0;
               vec3 rock_detail = n_xz * blend.z + n_xy * blend.y + n_yz * blend.x;
               
               float rock_lum = dot(rock_tex, vec3(0.33));
               
               vec3 grass_normal_sample = texture(rock_normal_map, v_world_pos.xy * 0.005).rgb * 2.0 - 1.0;
               vec3 perturbed = normal;
               perturbed.xy += grass_normal_sample.xy * (1.0 - rock_mixin) * 0.8;
               perturbed.xy += rock_detail.xy * rock_mixin * 3.0;
               final_normal = normalize(perturbed);
               
               float geometric_ao = 0.5 + 0.5 * rock_detail.z;
               geometric_ao = geometric_ao * geometric_ao;
               float micro_ao = 0.5 + 0.5 * rock_lum;
               float rock_ao = mix(1.0, geometric_ao * micro_ao, rock_mixin);
               
               float height_factor = smoothstep(800.0, 2000.0, v_h);
               vec3 c_grass_lush = vec3(0.08, 0.42, 0.12);
               vec3 c_grass_dry = vec3(0.30, 0.35, 0.12);
               vec3 c_grass_height = mix(c_grass_lush, c_grass_dry, height_factor);
               
               vec3 patch_noise = texture(noise, reliefCoord * 5.0).rgb;
               vec3 grass_noise = texture(noise, reliefCoord * 40.0).rgb;
               vec3 grass_color = c_grass_height * (0.85 + 0.3 * grass_noise);
               
               vec3 rock_color = c_rock * (-1.0 + 5.0 * rock_lum) * rock_ao;
               
               terrain_color = mix(grass_color, rock_color, rock_mixin);
            }
          }
          
          // === Lighting (unchanged) ===
          float final_l = max(0.0, dot(final_normal, lightDir));
          
          vec3 sky_color = vec3(0.4, 0.5, 0.7);
          vec3 ground_color = vec3(0.15, 0.12, 0.08);
          float sky_factor = final_normal.z * 0.5 + 0.5;
          vec3 ambient = mix(ground_color, sky_color, sky_factor) * 0.35;
          
          vec3 sun_color = vec3(1.0, 0.95, 0.85);
          vec3 direct = sun_color * final_l * shadow_val * 0.75;
          vec3 lighting = ambient + direct;

          // === AO (unchanged) ===
          float occlusion = texture(ao, reliefCoord).r;
          terrain_color = terrain_color * occlusion;

          // === Fog (unchanged) ===
          vec3 fog_color = pow(vec3(0.37, 0.56, 0.85), vec3(2.2));
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
      {|#version 300 es
        precision highp float;
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
    vertex_shader =
      {|#version 300 es
        out vec2 uv;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          uv = vec2(x, y);
          gl_Position = vec4(2.0 * x - 1.0, 2.0 * y - 1.0, 0.0, 1.0);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform sampler2D relief;
        uniform int width;
        uniform float scale; // Added scale uniform
        in vec2 uv;
        out float occlusion;

        const float PI = 3.14159265;

        // Decode height from RG channels (same as terrain shader)
        float decode_height(vec2 c) {
          return (c.r * 256.0 + c.g) * ((1.0/257.0) * 9500.0) - 500.0;
        }

        vec3 get_pos(vec2 coord) {
          float h = decode_height(texture(relief, coord).rg);
          // Reconstruct World Position
          // UV * Width * Scale = World Meters
          return vec3(coord * float(width) * scale, h);
        }

        // Pseudo-random noise
        float rand(vec2 co){
            return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
        }

        void main() {
          float dist_step = 1.0 / float(width); 
          
          // Center Height
          float h_center = decode_height(texture(relief, uv).rg);
          
          float AO = 0.0;
          float R_uv = 150.0 * dist_step; // ~150 pixels radius in UV space
          
          // Precompute world distance for one step 
          // R_pixels = 150.0. 
          // R_world = 150.0 * scale.
          // step_len_uv = R_uv / 16.0
          // step_len_world = (150.0 * scale) / 16.0
          float step_dist_world = (150.0 * scale) / 16.0;
          
          // Jitter
          float random_val = rand(gl_FragCoord.xy);
          float random_angle = random_val * 2.0 * PI;
          
          float directions = 8.0;
          float steps = 16.0; 
          
          for (float d = 0.0; d < 8.0; d++) {
             float angle = random_angle + (d / 8.0) * 2.0 * PI;
             vec2 dir = vec2(cos(angle), sin(angle));
             
             // Maximize Tangent (Height Diff / Dist) instead of Angle
             // Initialize to small value (tan(-89deg))
             float max_tan = -100.0;
             float max_obj_s = 0.0;
             
             for (float s = 1.0; s <= 16.0; s++) {
                vec2 sample_uv = uv + dir * (s / 16.0) * R_uv;
                
                // Boundary check
                if (sample_uv.x < 0.0 || sample_uv.x > 1.0 || sample_uv.y < 0.0 || sample_uv.y > 1.0) continue;
                
                float h_sample = decode_height(texture(relief, sample_uv).rg);
                float h_diff = h_sample - h_center;
                float dist = s * step_dist_world;
                
                float tan_s = h_diff / dist;
                
                if (tan_s > max_tan) {
                   max_tan = tan_s;
                   max_obj_s = s;
                }
             }
             
             // Convert max_tan back to sin(horizon_angle)
             float sin_horizon = max_tan / sqrt(1.0 + max_tan * max_tan);
             
             // Distance Attenuation
             // 1.0 - (dist / max_dist)^2
             float dist_ratio = max_obj_s / 16.0;
             float attenuation = 1.0 - dist_ratio * dist_ratio;
             
             AO += max(0.0, sin_horizon) * max(0.0, attenuation);
          }
          
          AO = AO / 8.0; 
          
          occlusion = 1.0 - AO; // Output visibility
        }
      |};
    attributes = [];
  }

let ao_blur_program =
  {
    vertex_shader =
      {|#version 300 es
        out vec2 uv;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          uv = vec2(x, y);
          gl_Position = vec4(2.0 * x - 1.0, 2.0 * y - 1.0, 0.0, 1.0);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform sampler2D ao_tex;
        uniform sampler2D relief;
        uniform vec2 inv_res;
        in vec2 uv;
        out float color;

        // Height decode (same as other shaders)
        float decode_height(vec2 c) {
          return (c.r * 256.0 + c.g) * ((1.0/257.0) * 9500.0) - 500.0;
        }

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

[@@@warning "-32"]

let shadow_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        precision highp int;
        uniform mat4 shadow_view_proj;
        uniform highp int w;      // Must be highp for accurate coord calculations
        uniform highp int w_mask;
        uniform highp int w_shift;
        uniform highp vec2 delta;
        uniform highp vec2 center_offset;
        uniform highp float snapped_alpha;
        uniform highp float inv_sectors_div;
        uniform highp float grid_k;
        uniform highp float grid_base;
        uniform highp float grid_scale;
        uniform highp vec2 inv_delta;
        uniform highp float inv_w;
        uniform highp float inv_avg_delta;
        uniform int max_lod;
        uniform mediump sampler2D relief;
        
        void main()
        {
          const float PI = 3.14159265359;
          int sector = gl_VertexID & w_mask;
          int ring = gl_VertexID >> w_shift;
          float theta = (float(sector) * inv_sectors_div) * (PI / 2.0) - (PI / 4.0);
          float angle = theta + snapped_alpha + (PI / 2.0);
          // Use exp(k*ring) instead of pow for consistent cross-platform behavior
          float r = grid_scale * (exp(grid_k * float(ring)) - 1.0);
          
          float x = r * cos(angle);
          float y = r * sin(angle);
          
          vec2 world_pos = vec2(x, y) + center_offset;
          
          // Compute relief coordinate
          vec2 coord = world_pos * inv_delta;

          // Calculate approximate grid spacing in meters
          float grid_spacing = grid_k * (r + grid_scale);
          
          // LOD level
          float lod_f = max(0.0, log2(grid_spacing * inv_avg_delta));
          int lod = min(int(lod_f), max_lod);
          
          // Texture Size at this LOD
          ivec2 tex_size = textureSize(relief, lod);

          // Manual bilinear interpolation for 2-byte height
          highp vec2 norm_coord = vec2(coord.x, float(w) - 1.0 - coord.y) * inv_w;
          norm_coord = clamp(norm_coord, 0.0, 1.0);
          
          highp vec2 lod_pos = norm_coord * vec2(tex_size);
          
          // Manual Bilinear Interpolation
          highp vec2 lod_tex_pos = clamp(lod_pos, vec2(0.5), vec2(tex_size) - 0.5);
          highp vec2 base_f = floor(lod_tex_pos - 0.5);
          highp ivec2 base = ivec2(base_f);
          highp vec2 f = fract(lod_tex_pos - 0.5);
          highp ivec2 w_max = tex_size - 1;
          
          // Vectorized Fetch & Decode
          highp vec2 s00 = texelFetch(relief, clamp(base + ivec2(0,0), ivec2(0), w_max), lod).rg;
          highp vec2 s10 = texelFetch(relief, clamp(base + ivec2(1,0), ivec2(0), w_max), lod).rg;
          highp vec2 s01 = texelFetch(relief, clamp(base + ivec2(0,1), ivec2(0), w_max), lod).rg;
          highp vec2 s11 = texelFetch(relief, clamp(base + ivec2(1,1), ivec2(0), w_max), lod).rg;

          highp vec4 R = vec4(s00.r, s10.r, s01.r, s11.r);
          highp vec4 G = vec4(s00.g, s10.g, s01.g, s11.g);
          
          highp vec4 H = (R * 256.0 + G) * ((1.0/257.0) * 9500.0) - 500.0;

          float h0 = mix(H.x, H.y, f.x);
          float h1 = mix(H.z, H.w, f.x);
          float h = mix(h0, h1, f.y);
          
          gl_Position = shadow_view_proj * vec4(world_pos.x, world_pos.y, h, 1.0);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        // No Color Output for Shadow Map
         void main() {
         }
      |};
    attributes = [];
  }

(* OpenGL setup *)

module Gl = Brr_canvas.Gl

type buffer = Buffer : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> buffer

let create_buffer ctx target (Buffer b) =
  let id = Gl.create_buffer ctx in
  Gl.bind_buffer ctx target (Some id);
  Gl.buffer_data ctx target (Brr.Tarray.of_bigarray1 b) Gl.static_draw;
  id

let create_geometry ctx ~indices ~buffers =
  let gid = Gl.create_vertex_array ctx in
  Gl.bind_vertex_array ctx (Some gid);
  let iid = create_buffer ctx Gl.element_array_buffer (Buffer indices) in
  Gl.bind_buffer ctx Gl.element_array_buffer (Some iid);
  let bind_attrib loc dim typ data =
    let id = create_buffer ctx Gl.array_buffer data in
    Gl.bind_buffer ctx Gl.array_buffer (Some id);
    Gl.enable_vertex_attrib_array ctx loc;
    Gl.vertex_attrib_pointer ctx loc dim typ false 0 0
  in
  List.iteri (fun loc (dim, typ, data) -> bind_attrib loc dim typ data) buffers;
  Gl.bind_vertex_array ctx None;
  Gl.bind_buffer ctx Gl.array_buffer None;
  Gl.bind_buffer ctx Gl.element_array_buffer None;
  gid

let compile_shader ctx src typ =
  let sid = Gl.create_shader ctx typ in
  Gl.shader_source ctx sid (Jstr.v src);
  Gl.compile_shader ctx sid;
  if Jv.to_bool (Gl.get_shader_parameter ctx sid Gl.compile_status) then sid
  else
    let log = Gl.get_shader_info_log ctx sid in
    Gl.delete_shader ctx sid;
    failwith (Jstr.to_string log)

let create_program ctx p =
  let vid = compile_shader ctx p.vertex_shader Gl.vertex_shader in
  let fid = compile_shader ctx p.fragment_shader Gl.fragment_shader in
  let pid = Gl.create_program ctx in
  Gl.attach_shader ctx pid vid;
  Gl.delete_shader ctx vid;
  Gl.attach_shader ctx pid fid;
  Gl.delete_shader ctx fid;
  List.iteri
    (fun i attr -> Gl.bind_attrib_location ctx pid i (Jstr.v attr))
    p.attributes;
  Gl.link_program ctx pid;
  if Jv.to_bool (Gl.get_program_parameter ctx pid Gl.link_status) then pid
  else
    let log = Gl.get_program_info_log ctx pid in
    Gl.delete_program ctx pid;
    failwith (Jstr.to_string log)

(* Geometry *)

let linearize2 a =
  Buffer
    Bigarray.(reshape_1 (genarray_of_array2 a) (Array2.dim1 a * Array2.dim2 a))

let linearize3 a =
  Buffer
    Bigarray.(
      reshape_1 (genarray_of_array3 a)
        (Array3.dim1 a * Array3.dim2 a * Array3.dim3 a))

let instantiate ~size =
  let sz = (size + 65535) lsr 16 in
  let _WebAssembly = Jv.(get global "WebAssembly") in
  let file = Jstr.v "compute.wasm" in
  let memory =
    Jv.(new' (get _WebAssembly "Memory") [| obj [| ("initial", of_int sz) |] |])
  in
  Fut.of_promise
    ~ok:(fun e ->
      ( Brr.Tarray.Buffer.of_jv (Jv.get memory "buffer"),
        Jv.(get (get e "instance") "exports") ))
    Jv.(
      call _WebAssembly "instantiateStreaming"
        [|
          call global "fetch" [| Jv.of_jstr file |];
          obj [| ("env", obj [| ("memory", memory) |]) |];
        |])

let _precompute tile_height tile_width tile =
  let normals =
    Bigarray.(Array3.create Int8_signed C_layout)
      (tile_height - 2) (tile_width - 2) 2
  in
  let heights =
    Bigarray.(Array2.create Float32 C_layout) (tile_height - 2) (tile_width - 2)
  in
  let deltax = deltay *. cos (44. *. pi /. 180.) in
  if true then (
    to_lwt
    @@
    let tile_size = tile_height * tile_width * 4 in
    let heights_size = (tile_height - 2) * (tile_width - 2) * 4 in
    let normals_size = (tile_height - 2) * (tile_width - 2) * 2 in
    let size = tile_size + heights_size + normals_size in
    let open Fut.Result_syntax in
    let+ memory, funcs = instantiate ~size in
    let t = Unix.gettimeofday () in
    Brr.Tarray.set_tarray
      Brr.Tarray.(of_buffer Float32 memory)
      ~dst:0
      (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array2 tile));
    let t' = Unix.gettimeofday () in
    ignore
      (Jv.call funcs "precompute"
         [|
           Jv.of_int tile_width;
           Jv.of_int tile_height;
           Jv.of_float deltax;
           Jv.of_float deltay;
           Jv.of_int 0;
           Jv.of_int tile_size;
           Jv.of_int (tile_size + heights_size);
         |]);
    Format.eprintf "precompute (kernel) %f@." (Unix.gettimeofday () -. t');
    Brr.Tarray.set_tarray
      (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array2 heights))
      ~dst:0
      Brr.Tarray.(
        of_buffer ~byte_offset:tile_size ~length:(heights_size / 4) Float32
          memory);
    Brr.Tarray.set_tarray
      (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array3 normals))
      ~dst:0
      Brr.Tarray.(
        of_buffer ~byte_offset:(tile_size + heights_size) ~length:normals_size
          Int8 memory);
    Format.eprintf "precompute %f@." (Unix.gettimeofday () -. t);
    (linearize2 heights, linearize3 normals))
  else
    let t = Unix.gettimeofday () in
    for y = 1 to tile_height - 2 do
      for x = 1 to tile_width - 2 do
        let nx = (tile.{y, x - 1} -. tile.{y, x + 1}) *. deltay in
        let ny = (tile.{y - 1, x} -. tile.{y + 1, x}) *. deltax in
        let nz = 2. *. deltax *. deltay in
        let n = 127. /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
        normals.{tile_height - 2 - y, x - 1, 0} <- truncate (nx *. n);
        normals.{tile_height - 2 - y, x - 1, 1} <- truncate (ny *. n);
        normals.{tile_height - 2 - y, x - 1, 2} <- truncate (nz *. n);
        heights.{tile_height - 2 - y, x - 1} <- tile.{y, x}
      done
    done;
    Format.eprintf "PRECOMPUTE %f@." (Unix.gettimeofday () -. t);
    Lwt.return (linearize2 heights, linearize3 normals)
(* TODO: Update fallback if keeping it, but plan is to delete *)

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
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.r32f
    (Bigarray.Array2.dim1 tile)
    (Bigarray.Array2.dim2 tile)
    0 Gl.red Gl.float
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array2 tile))
    0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.bind_texture ctx Gl.texture_2d None;
  tid

let make_noise_texture ctx =
  let size = 256 in
  let data =
    Bigarray.(Array1.create int8_unsigned c_layout (size * size * 3))
  in
  for i = 0 to (size * size * 3) - 1 do
    data.{i} <- Random.int 256
  done;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rgb size size 0 Gl.rgb Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 data))
    0;
  Gl.generate_mipmap ctx Gl.texture_2d;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.repeat;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.repeat;
  Gl.bind_texture ctx Gl.texture_2d None;
  tid

(* Create procedural rock texture with coherent patterns *)
let make_rock_texture ctx =
  let size = 256 in
  let data =
    Bigarray.(Array1.create int8_unsigned c_layout (size * size * 3))
  in
  (* Simple value noise function *)
  let hash x y =
    let n = x + (y * 57) in
    let n = (n lsl 13) lxor n in
    ((n * ((n * n * 15731) + 789221)) + 1376312589) land 0x7fffffff
  in
  let noise x y = float (hash x y land 255) /. 255.0 in
  (* Smooth noise with bilinear interpolation - tileable at given period *)
  let smooth_noise_tiled fx fy period =
    let x = int_of_float fx in
    let y = int_of_float fy in
    let x0 = x mod period in
    let y0 = y mod period in
    let x1 = (x + 1) mod period in
    let y1 = (y + 1) mod period in
    let frac_x = fx -. float x in
    let frac_y = fy -. float y in
    let v00 = noise x0 y0 in
    let v10 = noise x1 y0 in
    let v01 = noise x0 y1 in
    let v11 = noise x1 y1 in
    let i0 = v00 +. (frac_x *. (v10 -. v00)) in
    let i1 = v01 +. (frac_x *. (v11 -. v01)) in
    i0 +. (frac_y *. (i1 -. i0))
  in
  (* Multi-octave fractal noise - tileable with rotation to reduce banding *)
  let fractal_noise x y =
    (* Rotate coordinates at each octave to break up directional patterns *)
    let rot_x1 = x in
    let rot_y1 = y in
    let rot_x2 = (x *. 0.866) +. (y *. 0.5) in
    (* 30 degree rotation *)
    let rot_y2 = (y *. 0.866) -. (x *. 0.5) in
    let rot_x3 = (x *. 0.5) +. (y *. 0.866) in
    (* 60 degree rotation *)
    let rot_y3 = (y *. 0.5) -. (x *. 0.866) in
    let v1 = smooth_noise_tiled (rot_x1 *. 16.0) (rot_y1 *. 16.0) 16 in
    let v2 = smooth_noise_tiled (rot_x2 *. 32.0) (rot_y2 *. 32.0) 32 in
    let v3 = smooth_noise_tiled (rot_x3 *. 64.0) (rot_y3 *. 64.0) 64 in
    let v4 = smooth_noise_tiled (rot_x1 *. 128.0) (rot_y1 *. 128.0) 128 in
    (v1 *. 0.4) +. (v2 *. 0.3) +. (v3 *. 0.2) +. (v4 *. 0.1)
  in
  for py = 0 to size - 1 do
    for px = 0 to size - 1 do
      let fx = float px /. float size in
      let fy = float py /. float size in
      let v = fractal_noise fx fy in
      (* Rock color: grey-brown tones *)
      let base_r = 0.35 +. (v *. 0.3) in
      let base_g = 0.32 +. (v *. 0.28) in
      let base_b = 0.28 +. (v *. 0.25) in
      let idx = ((py * size) + px) * 3 in
      data.{idx} <- min 255 (int_of_float (base_r *. 255.0));
      data.{idx + 1} <- min 255 (int_of_float (base_g *. 255.0));
      data.{idx + 2} <- min 255 (int_of_float (base_b *. 255.0))
    done
  done;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rgb size size 0 Gl.rgb Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 data))
    0;
  Gl.generate_mipmap ctx Gl.texture_2d;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.repeat;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.repeat;
  Gl.bind_texture ctx Gl.texture_2d None;
  (* Also generate a normal map from the height values *)
  let normal_data =
    Bigarray.(Array1.create int8_unsigned c_layout (size * size * 3))
  in
  let strength = 5.0 in
  (* Normal strength - higher = more bumpy *)
  for py = 0 to size - 1 do
    for px = 0 to size - 1 do
      (* Get heights at neighboring pixels (with wrapping) *)
      let get_height x y =
        let wx = (x + size) mod size in
        let wy = (y + size) mod size in
        let idx = ((wy * size) + wx) * 3 in
        float data.{idx} /. 255.0
      in
      let h_left = get_height (px - 1) py in
      let h_right = get_height (px + 1) py in
      let h_down = get_height px (py - 1) in
      let h_up = get_height px (py + 1) in
      (* Central differences for gradient *)
      let dx = (h_right -. h_left) *. strength in
      let dy = (h_up -. h_down) *. strength in
      (* Tangent-space normal: (-dx, -dy, 1) normalized *)
      let len = sqrt ((dx *. dx) +. (dy *. dy) +. 1.0) in
      let nx = -.dx /. len in
      let ny = -.dy /. len in
      let nz = 1.0 /. len in
      (* Encode to RGB: map [-1,1] to [0,255] *)
      let idx = ((py * size) + px) * 3 in
      normal_data.{idx} <- int_of_float (((nx *. 0.5) +. 0.5) *. 255.0);
      normal_data.{idx + 1} <- int_of_float (((ny *. 0.5) +. 0.5) *. 255.0);
      normal_data.{idx + 2} <- int_of_float (((nz *. 0.5) +. 0.5) *. 255.0)
    done
  done;
  let normal_tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some normal_tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rgb size size 0 Gl.rgb Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 normal_data))
    0;
  Gl.generate_mipmap ctx Gl.texture_2d;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.repeat;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.repeat;
  Gl.bind_texture ctx Gl.texture_2d None;
  (tid, normal_tid)

(* Create CLC palette texture (128x1 RGBA, 2 pixels per material) *)
let make_palette_texture ctx =
  let data = Clc_palette.generate_palette () in
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rgba8 128 1 0 Gl.rgba Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 data))
    0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  Gl.bind_texture ctx Gl.texture_2d None;
  tid

(* Create dummy CLC cover map for testing (uniform grass ID) *)
let make_dummy_cover_map ctx =
  let size = 64 in
  let data = Bigarray.(Array1.create int8_unsigned c_layout (size * size)) in
  (* Fill with natural grassland ID (index 26 = code 321) *)
  let grass_idx = Clc_palette.get_index 321 in
  for i = 0 to (size * size) - 1 do
    data.{i} <- grass_idx
  done;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  (* Use R8UI for integer texture *)
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.r8ui size size 0 Gl.red_integer
    Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 data))
    0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  Gl.bind_texture ctx Gl.texture_2d None;
  (tid, size)

[@@@warning "-32"]

let create_shadow_map ctx width height layers =
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d_array (Some tid);
  Gl.tex_storage3d ctx Gl.texture_2d_array 1 Gl.depth_component24 width height
    layers;

  (* Linear filter for smooth shadow edges with hardware comparison *)
  Gl.tex_parameteri ctx Gl.texture_2d_array Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d_array Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d_array Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx Gl.texture_2d_array Gl.texture_wrap_t Gl.clamp_to_edge;

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

  (* If Gl.none is not available in Brr/valid binding, just ensure no color attachment is bound *)
  (* But wait, Gl.draw_buffers is needed if we want to be explicit about no color *)
  (* If function is missing, skip it. Default is DRAW_BUFFER0 *)

  (* We just won't bind any color texture. *)

  (* Check status *)
  let status = Gl.check_framebuffer_status ctx Gl.framebuffer in
  if status <> Gl.framebuffer_complete then
    Format.eprintf "Shadow FBO Incomplete: %d@." status;

  Gl.bind_framebuffer ctx Gl.framebuffer None;
  fbo

let calculate_shadow_matrices ~near_plane:_ ~view_proj:_ ~splits:_ ~light_dir
    ~world_center ~shadow_map_size:_ =
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

let compute_ao ctx width height scale relief_texture =
  (* Helper to create FBO and R8 Texture *)
  let create_r8_target w h =
    let tid = Gl.create_texture ctx in
    Gl.bind_texture ctx Gl.texture_2d (Some tid);
    Gl.tex_storage2d ctx Gl.texture_2d 1 Gl.r8 w h;
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.linear;
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
    tid
  in

  let bake_pid = create_program ctx ao_bake_program in
  let blur_pid = create_program ctx ao_blur_program in

  let ao_bake_tex = create_r8_target width height in
  let ao_final_tex = create_r8_target width height in

  let fbo = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some fbo);

  (* PASS 1: Bake AO *)
  Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0 Gl.texture_2d
    ao_bake_tex 0;

  Gl.viewport ctx 0 0 width height;
  Gl.use_program ctx bake_pid;

  let relief_loc = Gl.get_uniform_location ctx bake_pid (Jstr.v "relief") in
  let width_loc = Gl.get_uniform_location ctx bake_pid (Jstr.v "width") in
  let scale_loc = Gl.get_uniform_location ctx bake_pid (Jstr.v "scale") in

  Gl.uniform1i ctx relief_loc 0;
  Gl.uniform1i ctx width_loc width;
  Gl.uniform1f ctx scale_loc scale;

  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);

  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  (* Fullscreen Quad *)

  (* PASS 2: Blur AO with bilateral filter *)
  Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0 Gl.texture_2d
    ao_final_tex 0;

  Gl.use_program ctx blur_pid;

  let ao_loc = Gl.get_uniform_location ctx blur_pid (Jstr.v "ao_tex") in
  let relief_blur_loc =
    Gl.get_uniform_location ctx blur_pid (Jstr.v "relief")
  in
  let inv_res_loc = Gl.get_uniform_location ctx blur_pid (Jstr.v "inv_res") in

  Gl.uniform1i ctx ao_loc 0;
  Gl.uniform1i ctx relief_blur_loc 1;
  Gl.uniform2f ctx inv_res_loc (1.0 /. float width) (1.0 /. float height);

  (* Bind AO bake texture on unit 0 *)
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some ao_bake_tex);

  (* Bind relief texture on unit 1 for bilateral comparison *)
  Gl.active_texture ctx Gl.texture1;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);

  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  (* Cleanup *)
  Gl.delete_framebuffer ctx fbo;
  Gl.delete_texture ctx ao_bake_tex;
  Gl.delete_program ctx bake_pid;
  Gl.delete_program ctx blur_pid;

  (* Restore State *)
  Gl.bind_framebuffer ctx Gl.framebuffer None;
  Gl.bind_texture ctx Gl.texture_2d None;

  ao_final_tex

let text_canvas = Brr_canvas.Canvas.of_el (Brr.El.canvas [])
let text_ctx = Brr_canvas.C2d.get_context text_canvas

let prepare_text ctx text =
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
  Brr_canvas.Canvas.set_w text_canvas w;
  Brr_canvas.Canvas.set_h text_canvas h;
  C2d.set_font text_ctx (Jstr.v "48px sans");
  C2d.fill_text text_ctx text ~x:left ~y:ascent;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d_of_source ctx Gl.texture_2d 0 Gl.rgba w h 0 Gl.rgba
    Gl.unsigned_byte
    (Gl.Tex_image_source.of_canvas_el text_canvas);
  Gl.generate_mipmap ctx Gl.texture_2d;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  Gl.bind_texture ctx Gl.texture_2d None;
  (tid, w, h)

let draw_text ctx transform_loc transform (tid, w, h) =
  let open Brr_canvas in
  let transform = Matrix.(scale (float w /. float h) 1. 1. * transform) in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.uniform_matrix4fv ctx transform_loc false
    (Brr.Tarray.of_bigarray1 (Matrix.array transform));
  Gl.draw_elements ctx Gl.triangle_strip 4 Gl.unsigned_byte 0;
  Gl.bind_texture ctx Gl.texture_2d None

let draw_shadows ~shadow_pid ~shadow_fbo ~shadow_map ~matrices ~splits:_
    ~terrain_geo ~index_count ~relief_texture ~x ~y ~lat ~lon ~w
    ~snapped_alpha:_ ctx =
  let width = Brr_canvas.Gl.drawing_buffer_width ctx in
  let height = Brr_canvas.Gl.drawing_buffer_height ctx in
  let deltax = deltay *. cos (lat *. pi /. 180.) in

  (* Unbind Shadow Map from Texture Unit 4 to prevent Feedback Loop *)
  Gl.active_texture ctx Gl.texture4;
  Gl.bind_texture ctx Gl.texture_2d_array None;

  (* Setup FBO and viewport *)
  Gl.bind_framebuffer ctx Gl.framebuffer (Some shadow_fbo);
  Gl.viewport ctx 0 0 2048 2048;
  Gl.use_program ctx shadow_pid;

  (* Radial Grid calculation *)
  let w_stride = next_power_of_two (n_sectors + 1) 1 in
  let w_mask_radial = w_stride - 1 in
  let w_shift_radial =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
    log2 w_stride
  in

  (* Exponential Grid Parameters *)
  let grid_k = pi /. float n_sectors in
  let height_term = exp (grid_k *. float (n_rings - 1)) in
  let grid_base = exp grid_k in
  let grid_scale = 70000. /. (height_term -. 1.) in
  let avg_delta = (deltax +. deltay) *. 0.5 in

  (* Set all uniforms once *)
  Gl.uniform1i ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "w_mask"))
    w_mask_radial;
  Gl.uniform1i ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "w_shift"))
    w_shift_radial;
  Gl.uniform1f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "inv_sectors_div"))
    (1. /. (float n_sectors /. 2.));
  Gl.uniform1f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "grid_k"))
    grid_k;
  Gl.uniform1f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "grid_base"))
    grid_base;
  Gl.uniform1f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "grid_scale"))
    grid_scale;
  Gl.uniform2f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "inv_delta"))
    (1. /. deltax) (1. /. deltay);
  Gl.uniform1f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "inv_avg_delta"))
    (1. /. avg_delta);
  Gl.uniform1i ctx (Gl.get_uniform_location ctx shadow_pid (Jstr.v "w")) w;
  Gl.uniform1f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "inv_w"))
    (1. /. float w);

  let max_lod =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
    log2 w
  in
  Gl.uniform1i ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "max_lod"))
    max_lod;

  (* Center Offset *)
  let off_x = (lon *. 3600.) -. floor (lon *. 3600.) in
  let off_y = (lat *. 3600.) -. floor (lat *. 3600.) in
  let center_offset_x = deltax *. (float x +. off_x -. 0.5) in
  let center_offset_y = deltay *. (float y +. off_y -. 0.5) in
  Gl.uniform2f ctx
    (Gl.get_uniform_location ctx shadow_pid (Jstr.v "center_offset"))
    center_offset_x center_offset_y;

  (* Bind Relief Texture *)
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);
  Gl.uniform1i ctx (Gl.get_uniform_location ctx shadow_pid (Jstr.v "relief")) 0;

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
  let snapped_alpha_loc =
    Gl.get_uniform_location ctx shadow_pid (Jstr.v "snapped_alpha")
  in
  let svp_loc =
    Gl.get_uniform_location ctx shadow_pid (Jstr.v "shadow_view_proj")
  in

  for layer = 0 to 2 do
    Gl.framebuffer_texture_layer ctx Gl.framebuffer Gl.depth_attachment
      shadow_map 0 layer;

    let status = Gl.check_framebuffer_status ctx Gl.framebuffer in
    if status <> Gl.framebuffer_complete then
      Format.eprintf "Shadow FBO Error (Layer %d): %d@." layer status;

    (* Clear full texture including 1-pixel border with depth=1.0 *)
    Gl.disable ctx Gl.scissor_test;
    Gl.clear ctx (Gl.depth_buffer_bit lor Gl.color_buffer_bit);

    (* Enable scissor to render only inner area, leaving 1-pixel border *)
    Gl.enable ctx Gl.scissor_test;
    Gl.scissor ctx 1 1 2046 2046;

    Gl.uniform_matrix4fv ctx svp_loc false
      (Brr.Tarray.of_bigarray1 (Matrix.array matrices.(layer)));

    (* Render 4 rotations to cover full terrain *)
    for rotation = 0 to 3 do
      Gl.uniform1f ctx snapped_alpha_loc rotation_angles.(rotation);
      Gl.draw_elements ctx Gl.triangle_strip index_count Gl.unsigned_int 0
    done;

    Gl.disable ctx Gl.scissor_test
  done;

  (* Restore state *)
  Gl.color_mask ctx true true true true;
  Gl.cull_face ctx Gl.back;
  Gl.bind_framebuffer ctx Gl.framebuffer None;
  Gl.clear_depth ctx 1.0;
  Gl.viewport ctx 0 0 width height

let scale = (*2. *. 27. /. 24.*) 3.2
let text_height = 0.07

(* Track whether shadows have been rendered - only render once per session *)
let shadow_rendered = ref false

let draw terrain_pid terrain_geo _tile_texture relief_texture triangle_pid
    text_pid text_geo ~w ~h:_ ~x ~y ~height ~lat ~lon ~orientation ~points ~tile
    ~index_count ~noise_texture ~ao_texture ~rock_texture ~rock_normal_map
    ~shadow_pid ~shadow_fbo ~shadow_map ~palette_texture ~cover_map_texture
    ~cover_map_size ~use_clc canvas ctx =
  let canvas_width = truncate (Brr.El.inner_w canvas) in
  let canvas_height = truncate (Brr.El.inner_h canvas) in
  let canvas = Brr_canvas.Canvas.of_el canvas in
  if Brr_canvas.Canvas.w canvas <> canvas_width then
    Brr_canvas.Canvas.set_w canvas canvas_width;
  if Brr_canvas.Canvas.h canvas <> canvas_height then
    Brr_canvas.Canvas.set_h canvas canvas_height;
  Gl.viewport ctx 0 0 canvas_width canvas_height;
  let aspect = float canvas_width /. float canvas_height in
  let deltax = deltay *. cos (lat *. pi /. 180.) in
  let transform =
    Matrix.(
      translate 0. 0. (-.height -. 2.)
      * rotate_z (-.orientation.alpha *. pi /. 180.)
      * rotate_x (-.orientation.beta *. pi /. 180.)
      * rotate_y (-.orientation.gamma *. pi /. 180.)
      * rotate_z (orientation.screen *. pi /. 180.))
  in
  (* Radial Grid calculation *)
  let w_stride = next_power_of_two (n_sectors + 1) 1 in
  let w_mask_radial = w_stride - 1 in
  let w_shift_radial =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n lsr 1) in
    log2 w_stride
  in
  let screen_inclination =
    orientation.screen
    +. 180. /. pi
       *. atan2
            (sin (orientation.gamma *. pi /. 180.)
            *. cos (orientation.beta *. pi /. 180.))
            (sin (orientation.beta *. pi /. 180.))
  in
  let x_scale, y_scale =
    if aspect < 1. then (scale /. aspect, scale) else (scale, scale *. aspect)
  in
  let proj = Matrix.project ~x_scale ~y_scale ~near_plane:0.1 in
  let points =
    List.filter_map
      (fun (pt, (x', y')) ->
        let px = deltax *. float (x' - x) in
        let py = deltay *. float (y - y') in
        let z = tile.{y', x'} in
        let r = Matrix.({ x = px; y = py; z; w = 1. } *< transform) in
        let r = { r with z = -.r.z } in
        if r.z > 1. then Some (pt, r.x /. r.z, r.y /. r.z) else None)
      points
  in
  let points =
    let pos = ref [] in
    let angle = (screen_inclination *. pi /. 180.) +. (pi /. 4.) in
    let ca = cos angle in
    let sa = sin angle in
    List.filter_map
      (fun (texture, x, y) ->
        let p = scale *. ((y *. ca) -. (x *. sa)) in
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

  (* SHADOW PASS *)
  let grid_k = pi /. float n_sectors in
  let current_azimuth = compute_azimuth transform in
  let snapped_alpha = floor ((current_azimuth /. grid_k) +. 0.5) *. grid_k in

  let light_dir =
    let m = Matrix.{ x = -1.; y = 1.; z = 2.; w = 0. } in
    (*
    let m = Matrix.{ x = -4.; y = -2.; z = 1.; w = 0. } in
*)
    let len = sqrt ((m.x *. m.x) +. (m.y *. m.y) +. (m.z *. m.z)) in
    Matrix.{ x = m.x /. len; y = m.y /. len; z = m.z /. len; w = 0. }
  in
  let view_proj = Matrix.(proj * transform) in
  let z_far = 50000. in
  let splits_ratios = [| 50. /. z_far; 400. /. z_far; 4000. /. z_far |] in
  (* Splits must match shadow matrix radii: 2000, 8000, 25000 *)
  let splits_dist = [| 2000.; 8000.; 25000. |] in

  (* Calculate World Center for Shadows *)
  (* Use z=0 since both shadow map terrain and lookup use absolute heights *)
  let off_x = (lon *. 3600.) -. floor (lon *. 3600.) in
  let off_y = (lat *. 3600.) -. floor (lat *. 3600.) in
  let center_offset_x = deltax *. (float x +. off_x -. 0.5) in
  let center_offset_y = deltay *. (float y +. off_y -. 0.5) in
  let world_center =
    Matrix.{ x = center_offset_x; y = center_offset_y; z = 0.; w = 1. }
  in

  let shadow_matrices =
    calculate_shadow_matrices ~near_plane:0.1 ~view_proj ~splits:splits_dist
      ~light_dir ~world_center ~shadow_map_size:2048.
  in

  (* Render shadows once on first frame *)
  if not !shadow_rendered then begin
    shadow_rendered := true;
    draw_shadows ~shadow_pid ~shadow_fbo ~shadow_map ~matrices:shadow_matrices
      ~splits:splits_ratios ~terrain_geo ~index_count ~relief_texture ~x ~y ~lat
      ~lon ~w ~snapped_alpha ctx
  end;

  Gl.clear_color ctx 0.37 0.56 0.85 1.;
  Gl.clear ctx (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

  Gl.use_program ctx terrain_pid;
  Gl.enable ctx Gl.depth_test;

  Gl.enable ctx Gl.cull_face';
  (* Radial Grid Uniforms *)
  (* Radial Grid Uniforms *)
  let width_shift_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "w_shift")
  in
  Gl.uniform1i ctx width_shift_loc w_shift_radial;

  let sectors_div_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "inv_sectors_div")
  in
  Gl.uniform1f ctx sectors_div_loc (1. /. (float n_sectors /. 2.));

  (* Exponential Grid Parameters *)
  let grid_k = pi /. float n_sectors in
  let height_term = exp (grid_k *. float (n_rings - 1)) in
  let grid_base = exp grid_k in
  let grid_scale = 70000. /. (height_term -. 1.) in

  let grid_k_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "grid_k") in
  let grid_base_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "grid_base")
  in
  let grid_scale_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "grid_scale")
  in

  Gl.uniform1f ctx grid_k_loc grid_k;
  Gl.uniform1f ctx grid_base_loc grid_base;
  Gl.uniform1f ctx grid_scale_loc grid_scale;

  let width_mask_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "w_mask")
  in
  Gl.uniform1i ctx width_mask_loc w_mask_radial;

  (* Determine snapped alpha *)
  let sector_angle = grid_k in
  (* Reuse k *)
  let current_azimuth = compute_azimuth transform in
  let snapped_alpha =
    floor ((current_azimuth /. sector_angle) +. 0.5) *. sector_angle
  in
  let sa_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "snapped_alpha")
  in
  Gl.uniform1f ctx sa_loc snapped_alpha;

  (* Center Offset in world meters relative to tile origin *)
  let off_x = (lon *. 3600.) -. floor (lon *. 3600.) in
  let off_y = (lat *. 3600.) -. floor (lat *. 3600.) in
  let center_offset_x = deltax *. (float x +. off_x -. 0.5) in
  let center_offset_y = deltay *. (float y +. off_y -. 0.5) in
  let co_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "center_offset")
  in
  Gl.uniform2f ctx co_loc center_offset_x center_offset_y;

  let w_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "w") in
  let inv_w_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "inv_w") in
  let max_lod_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "max_lod")
  in
  Gl.uniform1i ctx w_loc w;
  Gl.uniform1f ctx inv_w_loc (1. /. float w);
  let max_lod =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
    log2 w
  in
  Gl.uniform1i ctx max_lod_loc max_lod;
  let inv_delta_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "inv_delta")
  in
  let inv_avg_delta_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "inv_avg_delta")
  in
  Gl.uniform2f ctx inv_delta_loc (1. /. deltax) (1. /. deltay);
  let avg_delta = (deltax +. deltay) *. 0.5 in
  Gl.uniform1f ctx inv_avg_delta_loc (1. /. avg_delta);
  let proj_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "proj") in
  Gl.uniform_matrix4fv ctx proj_loc false
    (Brr.Tarray.of_bigarray1 (Matrix.array proj));
  let transform_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "transform")
  in
  Gl.uniform_matrix4fv ctx transform_loc false
    (Brr.Tarray.of_bigarray1 (Matrix.array transform));
  (*  let tile_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "tile") in*)
  let relief_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "relief") in
  let noise_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "noise") in
  let ao_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "ao") in
  let rock_tex_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "rock_texture")
  in
  let rock_normal_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "rock_normal_map")
  in

  Gl.uniform1i ctx relief_loc 1;
  Gl.uniform1i ctx noise_loc 2;
  Gl.uniform1i ctx ao_loc 3;
  Gl.uniform1i ctx rock_tex_loc 5;
  Gl.uniform1i ctx rock_normal_loc 6;

  (* CLC Uniforms *)
  let cover_map_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "u_coverMap")
  in
  let palette_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "u_paletteTex")
  in
  let cover_offset_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "u_coverMapOffset")
  in
  let cover_scale_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "u_coverMapScale")
  in
  let use_clc_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "u_useCLC")
  in
  Gl.uniform1i ctx cover_map_loc 7;
  Gl.uniform1i ctx palette_loc 8;
  (* Cover map offset: world origin of the CLC texture *)
  (* For now, use 0,0 since we don't have real CLC data *)
  Gl.uniform2f ctx cover_offset_loc 0.0 0.0;
  (* Cover map scale: meters per CLC texel (2m for high-res L0) *)
  let cover_scale = deltax *. float w /. float cover_map_size in
  Gl.uniform1f ctx cover_scale_loc cover_scale;
  (* Enable/disable CLC system *)
  Gl.uniform1i ctx use_clc_loc (if use_clc then 1 else 0);

  Gl.bind_vertex_array ctx (Some terrain_geo);
  Gl.active_texture ctx Gl.texture0;
  (*  Gl.bind_texture ctx Gl.texture_2d (Some tile_texture);*)
  Gl.active_texture ctx Gl.texture1;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);
  Gl.active_texture ctx Gl.texture2;
  Gl.bind_texture ctx Gl.texture_2d (Some noise_texture);
  Gl.active_texture ctx Gl.texture3;
  Gl.bind_texture ctx Gl.texture_2d (Some ao_texture);
  Gl.active_texture ctx Gl.texture5;
  Gl.bind_texture ctx Gl.texture_2d (Some rock_texture);
  Gl.active_texture ctx Gl.texture6;
  Gl.bind_texture ctx Gl.texture_2d (Some rock_normal_map);
  Gl.active_texture ctx Gl.texture4;
  Gl.bind_texture ctx Gl.texture_2d_array (Some shadow_map);
  (* CLC Textures *)
  Gl.active_texture ctx Gl.texture7;
  Gl.bind_texture ctx Gl.texture_2d (Some cover_map_texture);
  Gl.active_texture ctx Gl.texture8;
  Gl.bind_texture ctx Gl.texture_2d (Some palette_texture);

  (* Gl.bind_texture ctx Gl.texture_2d (Some shadow_debug_color); *)

  (* Shadow Uniforms *)
  let sm_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "shadow_matrices")
  in
  (* Flatten Matrices *)
  let flat_matrices =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (16 * 3)
  in
  for i = 0 to 2 do
    let m = Matrix.array shadow_matrices.(i) in
    for j = 0 to 15 do
      flat_matrices.{(i * 16) + j} <- m.{j}
    done
  done;

  (* Upload Matrix Array *)
  Gl.uniform_matrix4fv ctx sm_loc false (Brr.Tarray.of_bigarray1 flat_matrices);

  let ss_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "shadow_splits")
  in
  let splits_ba =
    Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout splits_dist
  in
  Gl.uniform1fv ctx ss_loc (Brr.Tarray.of_bigarray1 splits_ba);

  let smap_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "shadow_map")
  in
  Gl.uniform1i ctx smap_loc 4;

  (* Bind AO *)
  Gl.draw_elements ctx Gl.triangle_strip index_count Gl.unsigned_int 0;
  Gl.bind_vertex_array ctx None;
  Gl.bind_texture ctx Gl.texture_2d None;
  Gl.disable ctx Gl.depth_test;
  Gl.disable ctx Gl.cull_face';
  Gl.active_texture ctx Gl.texture0;

  Gl.use_program ctx triangle_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  let transform_loc =
    Gl.get_uniform_location ctx triangle_pid (Jstr.v "transform")
  in
  let color_loc = Gl.get_uniform_location ctx triangle_pid (Jstr.v "color") in
  Gl.enable ctx Gl.blend;
  Gl.blend_func ctx Gl.one Gl.one_minus_src_alpha;
  List.iter
    (fun (_, x, y, shown) ->
      let x = x *. x_scale in
      let y = y *. y_scale in
      let angle = if shown then -.pi /. 4. else 0. in
      let transform =
        let sx = 0.6 *. text_height *. x_scale /. scale in
        let sy = 0.6 *. text_height *. y_scale /. scale in
        Matrix.(
          rotate_z (angle +. (screen_inclination *. pi /. 180.))
          * scale sx sy 1. * translate x y 0.)
      in
      Gl.uniform_matrix4fv ctx transform_loc false
        (Brr.Tarray.of_bigarray1 (Matrix.array transform));
      if shown then Gl.uniform4f ctx color_loc 0. 0. 0. 1.
      else Gl.uniform4f ctx color_loc 0. 0. 0. 0.4;
      Gl.draw_elements ctx Gl.triangles 3 Gl.unsigned_byte 0)
    points;
  Gl.bind_vertex_array ctx None;
  Gl.disable ctx Gl.blend;

  Gl.use_program ctx text_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  Gl.enable ctx Gl.blend;
  Gl.blend_func ctx Gl.one Gl.one_minus_src_alpha;
  let transform_loc =
    Gl.get_uniform_location ctx text_pid (Jstr.v "transform")
  in
  List.iter
    (fun (texture, x, y, shown) ->
      if shown then
        let x = x *. x_scale in
        let y = y *. y_scale in
        let transform =
          let sx = text_height *. x_scale /. scale in
          let sy = text_height *. y_scale /. scale in
          Matrix.(
            translate 0.7 (-0.5) 0.
            * rotate_z ((pi /. 4.) +. (screen_inclination *. pi /. 180.))
            * scale sx sy 1. * translate x y 0.)
        in
        draw_text ctx transform_loc transform texture)
    points;
  Gl.disable ctx Gl.blend;

  Gl.bind_vertex_array ctx None

(* Event loop *)

let current_orientation = ref { alpha = 0.; beta = 0.; gamma = 0.; screen = 0. }

let request_animation_frame () =
  let t, u = Lwt.task () in
  ignore (Brr.G.request_animation_frame (fun _ -> Lwt.wakeup u ()));
  t

let event_loop ctx draw =
  let rec loop prev_orientation =
    let orientation = !current_orientation in
    if orientation <> prev_orientation then draw ~orientation ctx;
    let* () = request_animation_frame () in
    loop orientation
  in
  loop { !current_orientation with alpha = !current_orientation.alpha -. 1. }

(* Main *)

(* compute_gradient_cpu removed *)

let rec next_power_of_two n p = if n <= p then p else next_power_of_two n (p + p)

let mipmap_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        layout(location = 0) in vec3 position;
        out highp vec2 v_uv;
        void main() {
          v_uv = position.xy * 0.5 + 0.5;
          gl_Position = vec4(position, 1.);
        }|};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform sampler2D source_texture;
        uniform vec2 source_size;
        uniform float base_k;
        uniform float decay;
        uniform int level; // Target level (unused in logic, but passed)
        uniform int source_level; // Explicit source level
        in highp vec2 v_uv;
        out vec4 frag_color;
        void main() {
          vec2 size = source_size;
          // Map UV to Source Pixel Coordinates
          // v_uv points to center of the 2x2 block in source
          ivec2 p = ivec2(v_uv * size);
          
          // Force alignment to even coordinates (top-left of 2x2 block)
          ivec2 p00 = (p / 2) * 2;
          
          ivec2 c00 = clamp(p00, ivec2(0), ivec2(size) - 1);
          ivec2 c10 = clamp(p00 + ivec2(1, 0), ivec2(0), ivec2(size) - 1);
          ivec2 c01 = clamp(p00 + ivec2(0, 1), ivec2(0), ivec2(size) - 1);
          ivec2 c11 = clamp(p00 + ivec2(1, 1), ivec2(0), ivec2(size) - 1);
          
          // Source is main texture at source_level
          vec4 h00_v = texelFetch(source_texture, c00, source_level);
          vec4 h10_v = texelFetch(source_texture, c10, source_level);
          vec4 h01_v = texelFetch(source_texture, c01, source_level);
          vec4 h11_v = texelFetch(source_texture, c11, source_level);
          
          float h00 = h00_v.r + h00_v.g / 256.0;
          float h10 = h10_v.r + h10_v.g / 256.0;
          float h01 = h01_v.r + h01_v.g / 256.0;
          float h11 = h11_v.r + h11_v.g / 256.0;
          
          float k = base_k;
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
    attributes = [ "position" ];
  }

let copy_program =
  {
    vertex_shader =
      {|#version 300 es
        layout(location = 0) in vec3 position;
        out mediump vec2 v_uv;
        void main() {
          v_uv = position.xy * 0.5 + 0.5;
          gl_Position = vec4(position, 1.);
        }|};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform sampler2D source;
        uniform int level;
        uniform vec2 source_size;
        in mediump vec2 v_uv;
        out vec4 color;
        void main() {
          // Robust 1:1 Copy using UVs + Explicit Level
          // Works now that MinFilter is Mipmap-compatible
          ivec2 p = ivec2(v_uv * source_size);
          p = clamp(p, ivec2(0), ivec2(source_size) - 1);
          color = texelFetch(source, p, level);
        }|};
    attributes = [ "position" ];
  }

let gradient_program =
  {
    vertex_shader =
      {|#version 300 es
        out vec2 tileCoord;
        uniform vec2 size;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          tileCoord = vec2(x, y) * (size - 1.) + vec2(1.5, 1.5);
          gl_Position = vec4(2. * vec2(x, y) - 1., 0, 1.);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform vec2 size;
        uniform vec2 delta;
        in vec2 tileCoord;
        uniform sampler2D tile;
        out mediump vec4 color;

        float get_z(vec2 offset) {
            return texture(tile, (tileCoord + offset) / (size + 2.)).r;
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

let compute_relief ctx width height triangle_geo tile_texture =
  assert (width = height);

  let relief_pid = create_program ctx gradient_program in
  (* Not used in shader explicitly yet, using pow directly *)
  let max_level =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
    log2 (max width height)
  in
  let levels = max_level + 1 in

  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  (* Linear is fine for RGBA8 *)
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_base_level 0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_max_level (levels - 1);

  (* Use RGBA8 (4 bytes per pixel) *)
  Gl.tex_storage2d ctx Gl.texture_2d levels Gl.rgba8 width height;

  let fb = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some fb);
  let attachmentPoint = Gl.color_attachment0 in
  Gl.framebuffer_texture2d ctx Gl.framebuffer attachmentPoint Gl.texture_2d tid
    0;
  Gl.viewport ctx 0 0 width height;
  Gl.use_program ctx relief_pid;
  (* Draw Gradient (Level 0) *)
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some tile_texture);

  Gl.bind_vertex_array ctx (Some triangle_geo);
  let size_loc = Gl.get_uniform_location ctx relief_pid (Jstr.v "size") in
  Gl.uniform2f ctx size_loc (float width) (float height);

  (* Use default 44.0 latitude for gradient *)
  let deltax = deltay *. cos (44. *. pi /. 180.) in
  let delta_loc = Gl.get_uniform_location ctx relief_pid (Jstr.v "delta") in
  Gl.uniform2f ctx delta_loc deltax deltay;

  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  (* Mipmap Generation Loop *)
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  (* Mipmap Generation Loop *)
  let mipmap_pid = create_program ctx mipmap_program in
  let copy_pid = create_program ctx copy_program in
  (* Locations for Mipmap Program *)
  let source_level_loc =
    Gl.get_uniform_location ctx mipmap_pid (Jstr.v "source_level")
  in
  let base_k_loc = Gl.get_uniform_location ctx mipmap_pid (Jstr.v "base_k") in
  let decay_loc = Gl.get_uniform_location ctx mipmap_pid (Jstr.v "decay") in
  let source_loc =
    Gl.get_uniform_location ctx mipmap_pid (Jstr.v "source_texture")
  in
  let mipmap_size_loc =
    Gl.get_uniform_location ctx mipmap_pid (Jstr.v "source_size")
  in

  (* Locations for Copy Program *)
  let copy_source_loc =
    Gl.get_uniform_location ctx copy_pid (Jstr.v "source")
  in
  let copy_level_loc = Gl.get_uniform_location ctx copy_pid (Jstr.v "level") in
  let copy_size_loc =
    Gl.get_uniform_location ctx copy_pid (Jstr.v "source_size")
  in

  Gl.bind_vertex_array ctx (Some triangle_geo);

  (* Common Uniforms *)
  Gl.use_program ctx mipmap_pid;
  Gl.uniform1i ctx source_loc 0;
  Gl.uniform1f ctx base_k_loc 0.1;
  Gl.uniform1f ctx decay_loc 0.5;

  Gl.use_program ctx copy_pid;
  Gl.uniform1i ctx copy_source_loc 0;

  (* Not used in shader explicitly yet, using pow directly *)

  (* Start from level 1 *)

  (* Generate *)
  (* Ensure sampling from Level N-1 restricted? No, texturing samples from BaseLevel -> MaxLevel. *)
  (* To sample specifically from Level N-1, we might need to set GL_TEXTURE_BASE_LEVEL temporarily. *)
  (* Check error *)
  let check_err () =
    let err = Gl.get_error ctx in
    if err <> Gl.no_error then Format.eprintf "GL ERROR %d@." err
  in

  (* Temporary texture for ping-ponging to avoid feedback loop *)
  let temp_tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some temp_tid);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
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
      Gl.uniform1i ctx copy_source_loc 0;
      Gl.uniform1i ctx copy_level_loc (level - 1);
      (* Source size is previous level size *)
      Gl.uniform2f ctx copy_size_loc (float (w * 2)) (float (h * 2));

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
      Gl.uniform1i ctx source_loc 0;

      let source_w = float (w * 2) in
      let source_h = float (h * 2) in
      Gl.uniform2f ctx mipmap_size_loc source_w source_h;
      Gl.uniform1i ctx source_level_loc 0;

      (* Temp is Level 0 *)
      Gl.viewport ctx 0 0 w h;
      Gl.draw_elements ctx Gl.triangles 6 Gl.unsigned_byte 0;

      check_err ();
      loop (level + 1) (w / 2) (h / 2))
  in
  loop 1 (width / 2) (height / 2);

  Gl.delete_texture ctx temp_tid;

  (* Restore Texture Params - Not modified inside loop anymore *)
  (* Gl.bind_texture ctx Gl.texture_2d (Some tid);
     Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_base_level 0;
     Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_max_level 1000;
     Gl.bind_texture ctx Gl.texture_2d None; *)

  (* Restore Framebuffer *)
  Gl.bind_framebuffer ctx Gl.framebuffer None;

  Gl.bind_vertex_array ctx None;

  (tid, relief_pid)

let tri ~w ~h ~x ~y ~height ~lat ~lon ~points ~tile canvas ctx =
  let terrain_geo, indices =
    let sectors = n_sectors + 1 in
    let rings = n_rings in
    let w' = next_power_of_two sectors 1 in
    let indices = build_indices sectors w' rings in
    (create_geometry ctx ~indices ~buffers:[], indices)
  in
  let text_geo =
    create_geometry ctx
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
      Buffer b
    in
    create_geometry ctx ~indices ~buffers:[ (3, Gl.float, positions) ]
  in
  let terrain_pid = create_program ctx terrain_program in
  let triangle_pid = create_program ctx triangle_program in
  let text_pid = create_program ctx text_program in
  let tile_texture = make_tile_texture ctx tile in
  let relief_texture, _ = compute_relief ctx w h triangle_geo tile_texture in
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
          let height' = tile.{y', x'} in
          let dist =
            sqrt
              ((float (x' - x) ** 2.)
              +. (float (y' - y) ** 2.)
              +. ((height' -. height) ** 2.))
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
  let noise_texture = make_noise_texture ctx in
  let rock_texture, rock_normal_map = make_rock_texture ctx in
  let scale = deltay in
  (* Approx 30m per pixel *)
  let ao_texture = compute_ao ctx w h scale relief_texture in
  let shadow_map = create_shadow_map ctx 2048 2048 3 in
  let shadow_fbo = create_shadow_fbo ctx shadow_map in
  let shadow_pid = create_program ctx shadow_program in

  (* CLC Textures *)
  let palette_texture = make_palette_texture ctx in
  let cover_map_texture, cover_map_size = make_dummy_cover_map ctx in
  (* Set to true to enable CLC material system, false for original rendering *)
  let use_clc = true in

  event_loop ctx (fun ~orientation ctx ->
      draw terrain_pid terrain_geo tile_texture relief_texture triangle_pid
        text_pid text_geo ~w ~h ~x ~y ~lat ~lon ~orientation ~height ~tile
        ~points ~index_count ~noise_texture ~ao_texture ~rock_texture
        ~rock_normal_map ~shadow_pid ~shadow_fbo ~shadow_map ~palette_texture
        ~cover_map_texture ~cover_map_size ~use_clc canvas ctx)

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

let get_preset_position () =
  if false then (44.3950846, 6.7669714, 170.) (* La Chalannette, Jausiers *)
  else if true then (44.6078064, 6.8210935, 0.)
  else if false then
    (44.5738851 +. (1. /. 3600.), 6.7692490 +. (1. /. 3600.), 0.)
  else if true then (44.5740068 +. (1. /. 3600.), 6.7954285 +. (1. /. 3600.), 0.)
  else if true then (44.536194, 6.804142, 0.)
  else if true then (44.527946, 6.802877, 0.)
  else if true then (44.3950846, 6.7669714, 170.) (* La Chalannette, Jausiers *)
  else if true then (48.849418, 2.3674101, 0.) (* Paris *)
  else if true then (44.607649, 6.8204019, 220.) (* Col Girardin *)
  else if true then (44.209067, 6.9423065, 0.) (* Col du Blainon *)
  else if true then (44.207447, 6.906400, 40.)
    (* Auron vers est vallée de la Tinée *)
  else if true then (44.278358, 6.790589, 0.)
  else if true then (44.280097, 6.793942, 0.) (* Vallon de la Braïssa *)
  else if true then (44.336025, 6.907772, 0.) (* Lacs de Morgon *)
  else if true then (44.73365, 6.3630684, 0.) (* Roc Diolon (Orcières) *)
  else if true then (44.6896583, 6.8061028, 180.) (* Col Fromage *)
  else (44.789628, 6.670200, 66.)

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
        Loader.in_range ~size ~min_lat:43 ~max_lat:46 ~min_lon:5 ~max_lon:9 ~lat
          ~lon
      (*
        || Loader.in_range ~size ~min_lat:48 ~max_lat:49 ~min_lon:2 ~max_lon:2
             ~lat ~lon
*)
      then Some (lat, lon, 0.)
      else None
  | Error _ -> None

let get_position ~size =
  let open Fut.Syntax in
  let+ loc = get_current_position ~size in
  match loc with
  | Some loc -> Ok (true, loc)
  | None -> Ok (false, get_preset_position ())

let setup_events () =
  let deviceorientation =
    Brr.Ev.Type.create (Jstr.v "deviceorientationabsolute")
  in
  let state = ref `Init in
  ignore
    (Brr.Ev.listen deviceorientation
       (fun ev ->
         let angle nm = Jv.to_float (Jv.get (Brr.Ev.as_type ev) nm) in
         let alpha = angle "alpha" in
         let beta = angle "beta" in
         let gamma = angle "gamma" in
         (match !state with
         | `Init -> ()
         | `Starting ->
             state := `Started;
             if beta < 90. then display_temporary_message "Raise your phone!"
         | `Started -> if beta >= 90. then remove_message ());
         let screen =
           Jv.to_float
             (Jv.get (Jv.get (Jv.get Jv.global "screen") "orientation") "angle")
         in
         current_orientation := { alpha; beta; gamma; screen })
       (Brr.Window.as_target Brr.G.window));
  ignore
    (Brr.Ev.listen Brr.Ev.keydown
       (fun ev ->
         match Jstr.to_string (Brr.Ev.Keyboard.code (Brr.Ev.as_type ev)) with
         | "ArrowLeft" ->
             current_orientation :=
               {
                 !current_orientation with
                 alpha = !current_orientation.alpha +. 5.;
               }
         | "ArrowRight" ->
             current_orientation :=
               {
                 !current_orientation with
                 alpha = !current_orientation.alpha -. 5.;
               }
         | "ArrowDown" ->
             current_orientation :=
               {
                 !current_orientation with
                 beta = max 60. (!current_orientation.beta -. 5.);
               }
         | "ArrowUp" ->
             current_orientation :=
               {
                 !current_orientation with
                 beta = min 120. (!current_orientation.beta +. 5.);
               }
         | _ -> ())
       (Brr.Window.as_target Brr.G.window));
  fun () -> state := `Starting

let main () =
  let tile_width = 4098 in
  let tile_height = tile_width in
  (* Check that we are close to a power of two *)
  assert (next_power_of_two (tile_width - 2) 1 - (tile_width - 2) < 16);
  display_message "Getting current location...";
  let* () = to_lwt wait_for_service_worker in
  let* use_geoloc, (lat, lon, angle) = to_lwt (get_position ~size:tile_width) in
  current_orientation := { alpha = angle; beta = 90.; gamma = 0.; screen = 0. };
  let start = setup_events () in
  display_message "Loading...";
  let* tile = Loader.f ~size:tile_width ~lat ~lon in
  if use_geoloc then Lwt.async (fun () -> Loader.prefetch ~size:6144 ~lat ~lon);
  let x = tile_width / 2 in
  let y = (tile_height / 2) - 1 in
  let d = float x /. 3600. in
  let tile_coord = { Points.lon = lon -. d; lat = lat -. d } in
  let tile_coord' = { Points.lon = lon +. d; lat = lat +. d } in
  let* points =
    let width = 3600 in
    let height = 3600 in
    let* points = Reader.read_file "data/points.geojson" in
    (*
    let points =
      {|
{"features":[
    {
      "properties": {
        "ele": "2881",
        "name": "Cime de la Charvie"
      },
      "geometry": {
        "coordinates": [
          6.7626741,
          44.8556257
        ]
      }
    }
]}
|}
    in
 *)
    Lwt.return
      (Points.find tile_coord tile_coord' points
      |> List.map (fun ({ Points.coord = { lat; lon }; _ } as pt) ->
          let x =
            min (tile_width - 1)
              (truncate ((lon -. tile_coord.lon) *. float width))
          in
          let y =
            min (tile_height - 1)
              (truncate ((tile_coord'.lat -. lat) *. float height))
          in
          (pt, (x, y))))
  in
  let points =
    List.filter
      (fun (_, (dst_x, dst_y)) ->
        Visibility.test tile ~src_x:x ~src_y:y ~dst_x ~dst_y)
      points
  in
  (* Bilinear interpolation for height *)
  let off_x = (lon *. 3600.) -. floor (lon *. 3600.) in
  let off_y = (lat *. 3600.) -. floor (lat *. 3600.) in
  let h00 = tile.{y, x} in
  let h10 = tile.{y, x + 1} in
  let h01 = tile.{y - 1, x} in
  let h11 = tile.{y - 1, x + 1} in
  let h0 = h00 +. (off_x *. (h10 -. h00)) in
  let h1 = h01 +. (off_x *. (h11 -. h01)) in
  let height = h0 +. (off_y *. (h1 -. h0)) in
  Format.eprintf "ZZZ %f %f@." lat lon;
  Format.eprintf "ZZZ %f %f %f %f (%f %f) => %f@." h00 h10 h01 h11 off_x off_y
    height;

  let canvas =
    Option.get (Brr.Document.find_el_by_id Brr.G.document (Jstr.v "canvas"))
  in
  let toggle_fullscreen _ =
    match Brr.Document.fullscreen_element Brr.G.document with
    | None ->
        ignore
          (Brr.El.request_fullscreen
             ~opts:
               (Brr.El.fullscreen_opts ~navigation_ui:Brr.El.Navigation_ui.hide
                  ())
             canvas)
    | Some _ -> ignore (Brr.Document.exit_fullscreen Brr.G.document)
  in
  ignore Brr.(Ev.listen Ev.click toggle_fullscreen (El.as_target canvas));
  let ctx =
    Option.get
      (Brr_canvas.Gl.get_context ~attrs:(Gl.Attrs.v ())
         (Brr_canvas.Canvas.of_el canvas))
  in
  remove_message ();
  start ();
  tri ~w:(tile_width - 2) ~h:(tile_height - 2) ~x ~y ~height ~lat ~lon ~points
    ~tile canvas ctx

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
