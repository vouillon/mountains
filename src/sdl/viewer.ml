(*
https://registry.opendata.aws/copernicus-dem/

aws s3 cp  s3://copernicus-dem-30m/Copernicus_DSM_COG_10_N44_00_E006_00_DEM/Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif ~/tmp/relief

=====

https://developer.nvidia.com/gpugems/gpugems2/part-i-geometric-complexity/chapter-2-terrain-rendering-using-gpu-based-geometry
https://blogs.igalia.com/itoral/2016/10/13/opengl-terrain-renderer-rendering-the-terrain-mesh/
http://casual-effects.blogspot.com/2014/04/fast-terrain-rendering-with-continuous.html
https://mikejsavage.co.uk/geometry-clipmaps/

https://iquilezles.org/articles/fog/

Terrender: A Web-Based Multi-Resolution Terrain ...
RASTeR: Simple and efficient terrain rendering on the GPU
Top-Down View-Dependent Terrain Triangulation using the Octagon Metric.
Visualization of large terrains made easy.

http://app.geotiff.io/identify

curl https://copernicus-dem-30m.s3.amazonaws.com/Copernicus_DSM_COG_10_N45_00_E006_00_DEM/Copernicus_DSM_COG_10_N45_00_E006_00_DEM.tif
*)

(*
- web app
- use device orientation events

- draw closer triangles before farther triangles
  (split the whole grid in four quadrants and draw triangles appropriately)

var x = fetch('https://overpass-api.de/api/interpreter', {method:'POST', body: '[out:json][bbox:44,6,45,7];(node[natural=peak]; node[natural=saddle];);out;'}).then(resp=>resp.text());
*)
module Loader = Loader.Make (Reader)

let ( let** ) = Lwt.bind

open Tsdl
open Tsdl_ttf
open Tgles3

let read_file f =
  In_channel.with_open_bin f @@ fun ic ->
  really_input_string ic (in_channel_length ic)

let ( let* ) = Result.bind
let pi = 4. *. atan 1.
let n_sectors = 256
let n_rings = 512

(* Helper functions. *)

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f ->
    f a;
    Int32.to_int a.{0}

let set_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f i ->
    a.{0} <- Int32.of_int i;
    f a

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a;
  Gl.string_of_bigarray a

(* Shaders *)

let deltay = 40_000. /. 360. /. 3600. *. 1000.

type program = {
  vertex_shader : string;
  fragment_shader : string;
  attributes : string list;
}

let terrain_program =
  {
    vertex_shader =
      {|#version 300 es
        uniform mat4 proj;
        uniform mat4 transform;
        uniform int w_mask;
        uniform int w_shift;
        uniform mediump int w;
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
        void main()
        {
          const float PI = 3.14159265359;
          int sector = gl_VertexID & w_mask;
          int ring = gl_VertexID >> w_shift;
          float theta = (float(sector) * inv_sectors_div) * (PI / 2.0) - (PI / 4.0);
          float angle = theta - snapped_alpha + (PI / 2.0);
          float r = grid_scale * (pow(grid_base, float(ring)) - 1.0);
          highp vec2 pos_plane = vec2(cos(angle), sin(angle)) * r;
          highp vec2 coord_meters = center_offset + pos_plane;
          highp vec2 coord = coord_meters * inv_delta;
          
          float grid_spacing = grid_k * (r + grid_scale);
          
          // LOD level
          float lod_f = max(0.0, log2(grid_spacing * inv_avg_delta));
          int lod = min(int(lod_f), max_lod);
          
          ivec2 tex_size = textureSize(relief, lod);

          // Manual bilinear interpolation for 2-byte height
          highp vec2 norm_coord = vec2(coord.x, float(w) - 1.0 - coord.y) * inv_w;
          norm_coord = clamp(norm_coord, 0.0, 1.0);
          
          highp vec2 lod_pos = norm_coord * vec2(tex_size);
          highp vec2 lod_tex_pos = clamp(lod_pos, vec2(0.5), vec2(tex_size) - 0.5);
          
          highp vec2 base_f = floor(lod_tex_pos - 0.5);
          highp ivec2 base = ivec2(base_f);
          highp vec2 f = fract(lod_tex_pos - 0.5);
          
          highp ivec2 w_max = tex_size - 1;
          
          highp vec2 s00 = texelFetch(relief, clamp(base + ivec2(0,0), ivec2(0), w_max), lod).rg;
          highp vec2 s10 = texelFetch(relief, clamp(base + ivec2(1,0), ivec2(0), w_max), lod).rg;
          highp vec2 s01 = texelFetch(relief, clamp(base + ivec2(0,1), ivec2(0), w_max), lod).rg;
          highp vec2 s11 = texelFetch(relief, clamp(base + ivec2(1,1), ivec2(0), w_max), lod).rg;

          highp vec4 R = vec4(s00.r, s10.r, s01.r, s11.r);
          highp vec4 G = vec4(s00.g, s10.g, s01.g, s11.g);
          
          highp vec4 H = (R * 256.0 + G) * ((1.0/257.0) * 9500.0) - 500.0;

          float h0 = mix(H.x, H.y, f.x);
          float h1 = mix(H.z, H.w, f.x);
          float z = mix(h0, h1, f.y);
          
          reliefCoord = norm_coord + (0.5 * inv_w);

          vec4 pos = transform * vec4(pos_plane, z, 1.0);
          v_dist = length(pos.xyz);
          v_h = z;
          gl_Position = proj * pos;
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform mediump sampler2D relief;
        uniform mediump sampler2D noise; // Added Noise Sampler
        uniform mediump int w;
        uniform vec3 u_lightDir; // Dynamic Sun Direction
        in highp vec2 reliefCoord;
        in highp float v_dist;
        in highp float v_h;
        out lowp vec4 color;

        void main() {
          mediump vec2 encodedN = 
            texture(relief, reliefCoord).ba;
          highp vec3 normal;
          normal.xy = encodedN * 2.0 - 1.0;
          normal.z = sqrt(max(0.0, 1.0 - dot(normal.xy, normal.xy)));
          
          lowp float l = max(0.0, dot(normal, normalize(u_lightDir)));
          lowp float lighting = 0.1 + 0.9 * l; // Deeper shadows (Ambient 0.1)
          
          // Biome Colors (Vibrant & Darker to counteract Gamma)
          lowp vec3 c_water = vec3(0.05, 0.25, 0.45);
          lowp vec3 c_grass = vec3(0.1, 0.4, 0.15); // Deep Vibrant Green
          lowp vec3 c_rock  = vec3(0.3, 0.28, 0.25); // Darker Rock
          lowp vec3 c_snow  = vec3(0.95, 0.95, 0.98); // White
          
          // Slope Factor (0 = flat, 1 = vertical)
          float slope = 1.0 - normal.z;
          
          // Mixing Logic
          vec3 terrain_color;
          
          if (v_h < 0.0) {
             terrain_color = c_water;
          } else {
             // Base: Grass
             terrain_color = c_grass;
             
             // Slope: Grass -> Rock
             // mix when slope > 0.2, fully rock at 0.5
             float rock_mixin = smoothstep(0.15, 0.5, slope);
             terrain_color = mix(terrain_color, c_rock, rock_mixin);
          }
          
          // Noise Modulation
          lowp vec3 noise_val = texture(noise, reliefCoord * 40.0).rgb;
          terrain_color = terrain_color * (0.8 + 0.4 * noise_val);
          // terrain_color = terrain_color * 1.0; 

          // Match fog to clear color (0.37, 0.56, 0.85)
          lowp vec3 fog_color = pow(vec3(0.37, 0.56, 0.85), vec3(2.2));
          float fog_coeff = exp(v_dist * -3e-5); // Slightly clearer fog
          
          lowp vec3 final_color = mix(fog_color, lighting * terrain_color, fog_coeff);
          color = vec4(pow(final_color, vec3(1.0 / 2.2)), 1.);
        }
      |};
    attributes = [];
  }

let triangle_program =
  {
    vertex_shader =
      {|#version 300 es
        uniform mat4 transform;
        void main() {
          float x = float(gl_VertexID - 1) / 2.;
          float y = float(gl_VertexID != 1) * (sqrt(3.)/ 2.);
          gl_Position = transform * vec4(x, y, 0, 1.);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        out vec4 color;
        void main() {
          color = vec4(0,0,0,1);
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

let rec _next_power_of_two n p =
  if n <= p then p else _next_power_of_two n (p + p)

let mipmap_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        out highp vec2 v_uv;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          v_uv = vec2(x, y);
          gl_Position = vec4(2. * x - 1., 2. * y - 1., 0., 1.);
        }|};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform sampler2D source_texture;
        uniform vec2 source_size;
        uniform float base_k;
        uniform float decay;
        uniform int level;
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
    attributes = [];
  }

let copy_program =
  {
    vertex_shader =
      {|#version 300 es
        precision highp float;
        out mediump vec2 v_uv;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          v_uv = vec2(x, y);
          gl_Position = vec4(2. * x - 1., 2. * y - 1., 0., 1.);
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
          ivec2 p = ivec2(v_uv * source_size);
          p = clamp(p, ivec2(0), ivec2(source_size) - 1);
          color = texelFetch(source, p, level);
        }|};
    attributes = [];
  }

let relief_program =
  {
    vertex_shader =
      {|#version 300 es
        out vec2 tileCoord;
        uniform vec2 size;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          tileCoord = vec2(x, y) * (size - 1.) + vec2(0.5, 0.5);
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
            return texture(tile, (tileCoord + offset) / size).r;
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

(* OpenGL setup *)

type buffer = Buffer : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> buffer

let create_buffer target (Buffer b) =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer target id;
  Gl.buffer_data target bytes (Some b) Gl.static_draw;
  id

let delete_buffer bid = set_int (Gl.delete_buffers 1) bid

type geometry = { vertex_array : int; buffers : int list }

let create_geometry ~indices ~buffers =
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array gid;
  let iid = create_buffer Gl.element_array_buffer (Buffer indices) in
  Gl.bind_buffer Gl.element_array_buffer iid;
  let bind_attrib loc dim typ data =
    let id = create_buffer Gl.array_buffer data in
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
    id
  in
  let buffers =
    List.mapi (fun loc (dim, typ, data) -> bind_attrib loc dim typ data) buffers
  in
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;
  Ok { vertex_array = gid; buffers = iid :: buffers }

let bind_vertex_array { vertex_array; _ } = Gl.bind_vertex_array vertex_array

let delete_geometry { vertex_array = gid; buffers = bids } =
  set_int (Gl.delete_vertex_arrays 1) gid;
  List.iter delete_buffer bids;
  Ok ()

let compile_shader src typ =
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid
  else
    let len = get_shader sid Gl.info_log_length in
    let log = get_string len (Gl.get_shader_info_log sid len None) in
    Gl.delete_shader sid;
    Error (`Msg log)

type pid = Program of int

let create_program p =
  let* vid = compile_shader p.vertex_shader Gl.vertex_shader in
  let* fid = compile_shader p.fragment_shader Gl.fragment_shader in
  let pid = Gl.create_program () in
  let get_program pid e = get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid;
  Gl.delete_shader vid;
  Gl.attach_shader pid fid;
  Gl.delete_shader fid;
  List.iteri (fun i attr -> Gl.bind_attrib_location pid i attr) p.attributes;
  Gl.link_program pid;
  if get_program pid Gl.link_status = Gl.true_ then Ok (Program pid)
  else
    let len = get_program pid Gl.info_log_length in
    let log = get_string len (Gl.get_program_info_log pid len None) in
    Gl.delete_program pid;
    Error (`Msg log)

let use_program (Program pid) = Gl.use_program pid
let get_uniform_location (Program pid) attr = Gl.get_uniform_location pid attr

let delete_program (Program pid) =
  Gl.delete_program pid;
  Ok ()

(* Geometry *)

let make_tile_texture tile =
  let tid = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tid;
  let w = Bigarray.Array2.dim1 tile in
  let h = Bigarray.Array2.dim2 tile in
  Gl.tex_image2d Gl.texture_2d 0 Gl.r32f w h 0 Gl.red Gl.float
    (`Data (Bigarray.reshape_1 (Bigarray.genarray_of_array2 tile) (w * h)));
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.bind_texture Gl.texture_2d 0;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.bind_texture Gl.texture_2d 0;
  tid

let make_noise_texture () =
  let size = 256 in
  let data =
    Bigarray.(Array1.create int8_unsigned c_layout (size * size * 3))
  in
  for i = 0 to (size * size * 3) - 1 do
    data.{i} <- Random.int 256
  done;
  let tid = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tid;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgb size size 0 Gl.rgb Gl.unsigned_byte
    (`Data data);
  Gl.generate_mipmap Gl.texture_2d;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear_mipmap_linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.repeat;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.repeat;
  Gl.bind_texture Gl.texture_2d 0;
  tid

let compute_relief_gpu width height text_geo tile_texture =
  assert (width = height);

  let* relief_pid = create_program relief_program in

  let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
  let max_level = log2 width in
  let levels = max_level + 1 in

  let tid = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tid;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear_mipmap_linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_base_level 0;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_max_level (levels - 1);

  (* Use RGBA8 *)
  Gl.tex_storage2d Gl.texture_2d levels Gl.rgba8 width height;

  let fb = get_int (Gl.gen_framebuffers 1) in
  Gl.bind_framebuffer Gl.framebuffer fb;
  let attachmentPoint = Gl.color_attachment0 in
  Gl.framebuffer_texture2d Gl.framebuffer attachmentPoint Gl.texture_2d tid 0;
  Gl.viewport 0 0 width height;

  use_program relief_pid;

  bind_vertex_array text_geo;

  (* Use a default lat for gradient or pass it? Gradient is precomputed. *)
  let deltax = deltay *. cos (44. *. pi /. 180.) in
  let size_loc = get_uniform_location relief_pid "size" in
  Gl.uniform2f size_loc (float width) (float height);

  let delta_loc = get_uniform_location relief_pid "delta" in
  Gl.uniform2f delta_loc deltax deltay;

  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d tile_texture;

  Gl.draw_elements Gl.triangle_strip 4 Gl.unsigned_byte (`Offset 0);

  (* Start Mipmap Generation *)
  Gl.bind_texture Gl.texture_2d tid;

  let* mipmap_pid = create_program mipmap_program in
  let* copy_pid = create_program copy_program in

  let source_loc = get_uniform_location mipmap_pid "source_texture" in
  let mipmap_size_loc = get_uniform_location mipmap_pid "source_size" in
  let source_level_loc = get_uniform_location mipmap_pid "source_level" in
  let base_k_loc = get_uniform_location mipmap_pid "base_k" in
  let decay_loc = get_uniform_location mipmap_pid "decay" in

  let copy_source_loc = get_uniform_location copy_pid "source" in
  let copy_level_loc = get_uniform_location copy_pid "level" in
  let copy_size_loc = get_uniform_location copy_pid "source_size" in

  use_program mipmap_pid;
  Gl.uniform1i source_loc 0;
  Gl.uniform1f base_k_loc 0.1;
  Gl.uniform1f decay_loc 0.5;

  use_program copy_pid;
  Gl.uniform1i copy_source_loc 0;

  let temp_tid = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d temp_tid;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  Gl.tex_storage2d Gl.texture_2d 1 Gl.rgba8 width height;
  Gl.bind_texture Gl.texture_2d 0;

  let rec loop level w h =
    if level > max_level || w < 1 || h < 1 then Ok ()
    else
      (* 1. Copy Source (Level N-1) to Temp Texture *)
      (* Bind FBO to Temp Texture (Level 0) *)
      let () =
        Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0
          Gl.texture_2d temp_tid 0
      in

      (* Bind Source: tid *)
      let () = Gl.bind_texture Gl.texture_2d tid in

      let () = use_program copy_pid in
      let () = Gl.uniform1i copy_source_loc 0 in
      let () = Gl.uniform1i copy_level_loc (level - 1) in
      let () = Gl.uniform2f copy_size_loc (float (w * 2)) (float (h * 2)) in

      let () = Gl.viewport 0 0 (w * 2) (h * 2) in
      let () = bind_vertex_array text_geo in
      let () =
        Gl.draw_elements Gl.triangle_strip 4 Gl.unsigned_byte (`Offset 0)
      in

      (* 2. Downsample Temp(0) -> tid(N) *)
      (* Bind FBO to Dest: tid(N) *)
      let () =
        Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0
          Gl.texture_2d tid level
      in

      (* Bind Source: Temp *)
      let () = Gl.bind_texture Gl.texture_2d temp_tid in

      let () = use_program mipmap_pid in
      let () = Gl.uniform1i source_loc 0 in
      let () = Gl.uniform2f mipmap_size_loc (float (w * 2)) (float (h * 2)) in
      let () = Gl.uniform1i source_level_loc 0 in

      let () = Gl.viewport 0 0 w h in
      let () = bind_vertex_array text_geo in
      let () =
        Gl.draw_elements Gl.triangle_strip 4 Gl.unsigned_byte (`Offset 0)
      in

      loop (level + 1) (w / 2) (h / 2)
  in
  let* () = loop 1 (width / 2) (height / 2) in

  set_int (Gl.delete_textures 1) temp_tid;

  Gl.bind_framebuffer Gl.framebuffer 0;
  Gl.bind_texture Gl.texture_2d 0;
  bind_vertex_array { vertex_array = 0; buffers = [] };

  let* () = delete_program mipmap_pid in
  let* () = delete_program copy_pid in

  Ok (relief_pid, tid)

let build_indices w w' h =
  let is =
    Bigarray.(
      Array1.create Bigarray.int32 c_layout (((h - 1) * ((2 * w) + 1)) - 1))
  in
  for i = 0 to h - 2 do
    for j = 0 to w - 1 do
      is.{(i * ((2 * w) + 1)) + (j * 2) + 1} <- Int32.of_int (j + (i * w'));
      is.{(i * ((2 * w) + 1)) + (j * 2)} <- Int32.of_int (j + ((i + 1) * w'))
    done;
    if i > 0 then is.{(i * ((2 * w) + 1)) - 1} <- Int32.of_int (-1)
  done;
  is

let load_font () =
  let* () = Ttf.init () in
  Ttf.open_font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" 48

let draw_text font transform_loc transform text =
  let color = Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255 in
  let* surface = Ttf.render_utf8_blended font text color in
  let p = Sdl.get_surface_pitch surface in
  let _w, h = Sdl.get_surface_size surface in
  let w = p / 4 in
  let transform =
    Matrix.(
      scale (float w /. float h) 1. 1.
      * translate 0.7 (-0.5) 0.
      * rotate_z (pi /. 4.)
      * transform)
  in
  let a = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
  let tid = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tid;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba w h 0 Gl.rgba Gl.unsigned_byte
    (`Data a);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.uniform_matrix4fv transform_loc 1 false (Matrix.array transform);
  Gl.draw_elements Gl.triangle_strip 4 Gl.unsigned_byte (`Offset 0);
  set_int (Gl.delete_textures 1) tid;
  Sdl.free_surface surface;
  Ok ()

let scale = (*2. *. 27. /. 24.*) 3.2
let text_height = 0.07

let draw terrain_pid terrain_geo _tile_texture relief_texture noise_texture
    triangle_pid text_pid text_geo ~font ~aspect ~w ~h:_ ~x ~y ~height ~lat ~lon
    ~angle ~inclination ~points ~tile win =
  let deltax = deltay *. cos (lat *. pi /. 180.) in
  let transform =
    Matrix.(
      translate 0. 0. (-.height -. 2.)
      * (rotate_z (angle *. pi /. 180.) * rotate_x (-.inclination *. pi /. 180.)))
  in
  let points : (Points.t * _ * _) list =
    List.filter_map
      (fun (pt, (x', y')) ->
        let x_world = deltax *. float (x' - x) in
        let y_world = deltay *. float (y - y') in
        let z_world = tile.{y', x'} in

        let r_view =
          Matrix.(
            { x = x_world; y = y_world; z = z_world; w = 1. } *< transform)
        in
        if r_view.z < -1. (* near plane *) then
          Some (pt, r_view.x /. -.r_view.z, r_view.y /. -.r_view.z)
        else None)
      points
    |> List.sort (fun (_, _, y) (_, _, y') : int -> Stdlib.compare y' y)
  in
  Format.eprintf "POINTS(1) %d@." (List.length points);
  let points =
    let pos = ref [] in
    List.filter
      (fun (_, x, y) ->
        let p = scale *. (x -. y) /. sqrt 2. in
        if not (List.exists (fun p' -> abs_float (p' -. p) < text_height) !pos)
        then (
          pos := p :: !pos;
          true)
        else false)
      points
  in
  Format.eprintf "POINTS(2) %d@." (List.length points);

  Gl.clear_color 0.37 0.56 0.85 1.;
  Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

  use_program terrain_pid;
  Gl.enable Gl.depth_test;
  (* Gl.enable Gl.cull_face_enum; *)
  (* Radial Grid Uniforms *)
  (* Radial Grid Uniforms *)
  let w_stride = _next_power_of_two (n_sectors + 1) 1 in
  let w_mask_radial = w_stride - 1 in
  let w_shift_radial =
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n lsr 1) in
    log2 w_stride
  in

  let width_shift_loc = get_uniform_location terrain_pid "w_shift" in
  Gl.uniform1i width_shift_loc w_shift_radial;
  let width_mask_loc = get_uniform_location terrain_pid "w_mask" in
  Gl.uniform1i width_mask_loc w_mask_radial;

  let sectors_div_loc = get_uniform_location terrain_pid "inv_sectors_div" in
  Gl.uniform1f sectors_div_loc (1. /. (float n_sectors /. 2.));

  (* Exponential Grid Parameters *)
  let grid_k = pi /. float n_sectors in
  let height_term = exp (grid_k *. float (n_rings - 1)) in
  let grid_base = exp grid_k in
  let grid_scale = 50000. /. (height_term -. 1.) in

  let grid_k_loc = get_uniform_location terrain_pid "grid_k" in
  let grid_base_loc = get_uniform_location terrain_pid "grid_base" in
  let grid_scale_loc = get_uniform_location terrain_pid "grid_scale" in

  Gl.uniform1f grid_k_loc grid_k;
  Gl.uniform1f grid_base_loc grid_base;
  Gl.uniform1f grid_scale_loc grid_scale;

  (* Determine snapped alpha *)
  let sector_angle = grid_k in
  let current_alpha_rad = angle *. pi /. 180. in
  let snapped_alpha =
    floor ((current_alpha_rad /. sector_angle) +. 0.5) *. sector_angle
  in
  let sa_loc = get_uniform_location terrain_pid "snapped_alpha" in
  Gl.uniform1f sa_loc snapped_alpha;

  (* Center Offset *)
  let off_x = (lon *. 3600.) -. floor (lon *. 3600.) in
  let off_y = (lat *. 3600.) -. floor (lat *. 3600.) in
  let center_offset_x = deltax *. (float x +. off_x -. 0.5) in
  let center_offset_y = deltay *. (float y +. off_y -. 0.5) in
  let co_loc = get_uniform_location terrain_pid "center_offset" in
  Gl.uniform2f co_loc center_offset_x center_offset_y;

  let width_loc = get_uniform_location terrain_pid "w" in
  let inv_width_loc = get_uniform_location terrain_pid "inv_w" in
  let max_lod_loc = get_uniform_location terrain_pid "max_lod" in

  Gl.uniform1i width_loc w;
  Gl.uniform1f inv_width_loc (1. /. float w);

  let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
  let max_lod = log2 w in
  Gl.uniform1i max_lod_loc max_lod;

  let inv_delta_loc = get_uniform_location terrain_pid "inv_delta" in
  let inv_avg_delta_loc = get_uniform_location terrain_pid "inv_avg_delta" in

  Gl.uniform2f inv_delta_loc (1. /. deltax) (1. /. deltay);
  let avg_delta = (deltax +. deltay) *. 0.5 in
  Gl.uniform1f inv_avg_delta_loc (1. /. avg_delta);

  let proj =
    Matrix.project ~x_scale:(scale /. aspect) ~y_scale:scale ~near_plane:1.
  in
  let proj_loc = get_uniform_location terrain_pid "proj" in
  Gl.uniform_matrix4fv proj_loc 1 false (Matrix.array proj);
  let transform_loc = get_uniform_location terrain_pid "transform" in
  Gl.uniform_matrix4fv transform_loc 1 false (Matrix.array transform);
  (* let tile_loc = get_uniform_location terrain_pid "tile" in *)
  let relief_loc = get_uniform_location terrain_pid "relief" in
  let noise_loc = get_uniform_location terrain_pid "noise" in
  (* Gl.uniform1i tile_loc 0; *)
  Gl.uniform1i relief_loc 1;
  Gl.uniform1i noise_loc 2;

  let sx, sy, sz =
    let now = Unix.gettimeofday () in
    let sx, sy, sz = Sun.position ~lat ~lon ~time:now in
    if sz < 0.2 then
      let tm = Unix.localtime now in
      let tm = { tm with Unix.tm_hour = 10; tm_min = 0; tm_sec = 0 } in
      let t, _ = Unix.mktime tm in
      Sun.position ~lat ~lon ~time:t
    else (sx, sy, sz)
  in
  let ld_loc = get_uniform_location terrain_pid "u_lightDir" in
  Gl.uniform3f ld_loc sx sy sz;

  bind_vertex_array terrain_geo;
  Gl.active_texture Gl.texture0;
  (* Gl.bind_texture Gl.texture_2d tile_texture; *)
  Gl.active_texture Gl.texture1;
  Gl.bind_texture Gl.texture_2d relief_texture;
  Gl.active_texture Gl.texture2;
  Gl.bind_texture Gl.texture_2d noise_texture;
  Gl.enable Gl.primitive_restart_fixed_index;
  Gl.draw_elements Gl.triangle_strip
    (((n_rings - 1) * ((2 * (n_sectors + 1)) + 1)) - 1)
    Gl.unsigned_int (`Offset 0);
  Gl.disable Gl.primitive_restart_fixed_index;
  Gl.bind_vertex_array 0;
  Gl.disable Gl.depth_test;
  Gl.disable Gl.cull_face_enum;
  Gl.active_texture Gl.texture0;

  use_program triangle_pid;
  bind_vertex_array text_geo;
  let transform_loc = get_uniform_location triangle_pid "transform" in
  List.iter
    (fun (_, x, y) ->
      let x = x *. scale /. aspect in
      let y = y *. scale in
      let transform =
        Matrix.(
          rotate_z (-.pi /. 4.)
          * scale (0.6 *. text_height /. aspect) (0.6 *. text_height) 1.
          * translate x y 0.)
      in
      Gl.uniform_matrix4fv transform_loc 1 false (Matrix.array transform);
      Gl.draw_elements Gl.triangles 3 Gl.unsigned_byte (`Offset 0))
    points;
  Gl.bind_vertex_array 0;

  use_program text_pid;
  bind_vertex_array text_geo;
  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;
  let transform_loc = get_uniform_location text_pid "transform" in
  List.iter
    (fun ({ Points.name; elevation; _ }, x, y) ->
      let x = x *. scale /. aspect in
      let y = y *. scale in
      let transform =
        Matrix.(scale (text_height /. aspect) text_height 1. * translate x y 0.)
      in
      ignore
        (draw_text font transform_loc transform
           (match elevation with
           | None -> name
           | Some elevation -> Printf.sprintf "%s (%dm)" name elevation)))
    points;
  Gl.disable Gl.blend;

  Gl.bind_vertex_array 0;

  Sdl.gl_swap_window win;
  Ok ()

let reshape _win w h = Gl.viewport 0 0 w h

(* Window and OpenGL context *)

let pp_opengl_info ppf () =
  let pp = Format.fprintf in
  let pp_opt ppf = function
    | None -> pp ppf "error"
    | Some s -> pp ppf "%s" s
  in
  pp ppf "@[<v>@,";
  pp ppf "Renderer @[<v>@[%a@]@," pp_opt (Gl.get_string Gl.renderer);
  pp ppf "@[OpenGL %a / GLSL %a@]@]@," pp_opt (Gl.get_string Gl.version) pp_opt
    (Gl.get_string Gl.shading_language_version);
  pp ppf "@]"

let create_window ~gl:(maj, min) =
  let w_atts = Sdl.Window.(opengl + resizable) in
  let w_title = Printf.sprintf "OpenGL %d.%d (core profile)" maj min in
  let set a v = Sdl.gl_set_attribute a v in
  let* () = set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_es in
  let* () = set Sdl.Gl.context_major_version maj in
  let* () = set Sdl.Gl.context_minor_version min in
  let* () = set Sdl.Gl.doublebuffer 1 in
  let* win = Sdl.create_window ~w:640 ~h:480 w_title w_atts in
  let* ctx = Sdl.gl_create_context win in
  let* () = Sdl.gl_make_current win ctx in
  Sdl.log "%a" pp_opengl_info ();
  Ok (win, ctx)

let destroy_window win ctx =
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window win;
  Ok ()

(* Event loop *)

let event_loop win angle inclination draw =
  let e = Sdl.Event.create () in
  let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let rec loop angle inclination =
    let* () = Sdl.wait_event (Some e) in
    match event e with
    | `Quit -> Ok ()
    | `Key_down when key_scancode e = `Escape -> Ok ()
    | `Key_down when key_scancode e = `Right ->
        let angle = angle +. 3. in
        let w, h = Sdl.get_window_size win in
        draw ~aspect:(float w /. float h) ~angle ~inclination win;
        loop angle inclination
    | `Key_down when key_scancode e = `Left ->
        let angle = angle -. 3. in
        let w, h = Sdl.get_window_size win in
        draw ~aspect:(float w /. float h) ~angle ~inclination win;
        loop angle inclination
    | `Key_down when key_scancode e = `Up ->
        let inclination = min 120. (inclination +. 3.) in
        let w, h = Sdl.get_window_size win in
        draw ~aspect:(float w /. float h) ~angle ~inclination win;
        loop angle inclination
    | `Key_down when key_scancode e = `Down ->
        let inclination = max 60. (inclination -. 3.) in
        let w, h = Sdl.get_window_size win in
        draw ~aspect:(float w /. float h) ~angle ~inclination win;
        loop angle inclination
    | `Window_event -> (
        match window_event e with
        | `Exposed | `Resized ->
            let w, h = Sdl.get_window_size win in
            reshape win w h;
            draw ~aspect:(float w /. float h) ~angle ~inclination win;
            loop angle inclination
        | _ -> loop angle inclination)
    | _ -> loop angle inclination
  in
  draw ~aspect:(640. /. 480.) ~angle ~inclination win;
  loop angle inclination

(* Main *)

let tri ~gl:((_maj, _min) as gl) ~w ~h ~x ~y ~lat ~lon ~angle ~height ~points
    ~tile =
  let* () = Sdl.init Sdl.Init.video in
  let* font = load_font () in
  let* win, ctx = create_window ~gl in
  let* terrain_geo =
    let sectors = n_sectors + 1 in
    let rings = n_rings in
    let w' = _next_power_of_two sectors 1 in
    create_geometry ~indices:(build_indices sectors w' rings) ~buffers:[]
  in
  let* text_geo =
    create_geometry
      ~indices:(Bigarray.(Array1.init int8_unsigned c_layout) 4 (fun i -> i))
      ~buffers:[]
  in
  let tile_texture = make_tile_texture tile in
  let noise_texture = make_noise_texture () in
  let w_viewport, h_viewport = Sdl.gl_get_drawable_size win in
  let* relief_pid, relief_texture =
    compute_relief_gpu w h text_geo tile_texture
  in
  reshape win w_viewport h_viewport;
  let* terrain_pid = create_program terrain_program in
  let* triangle_pid = create_program triangle_program in
  let* text_pid = create_program text_program in
  let inclination = 90. in
  let* () =
    event_loop win angle inclination (fun ~aspect ~angle ~inclination win ->
        ignore
          (draw terrain_pid terrain_geo tile_texture relief_texture
             noise_texture triangle_pid text_pid text_geo ~font ~aspect ~w ~h ~x
             ~y ~height ~lat ~lon ~angle ~inclination ~points ~tile win))
  in
  let* () = delete_program terrain_pid in
  let* () = delete_program triangle_pid in
  let* () = delete_program text_pid in
  let* () = delete_program relief_pid in
  let* () = delete_geometry terrain_geo in
  let* () = delete_geometry text_geo in
  set_int (Gl.delete_textures 1) tile_texture;
  set_int (Gl.delete_textures 1) relief_texture;
  set_int (Gl.delete_textures 1) noise_texture;
  let* () = destroy_window win ctx in
  Sdl.quit ();
  Ok ()

(*
let coordinates { Tiff.width; height; tile_width; tile_height; _ } lat lon =
  let y = truncate (fst (Float.modf lat) *. float height) in
  let x = truncate ((fst (Float.modf lon) *. float width) +. 0.5) in
  let y = height - 1 - y in
  let tx = x / tile_width in
  let ty = y / tile_height in
  let x = x mod tile_width in
  let y = y mod tile_height in
  let tile_index = tx + (ty * ((width + tile_width - 1) / tile_width)) in
  let tile_lon = floor lon +. (float tx *. float tile_width /. float width) in
  let tile_lat =
    floor lat +. 1. -. (float (ty + 1) *. float tile_height /. float height)
  in
  (
    tile_index,
    x,
    y,
    { Points.lon = tile_lon; lat = tile_lat },
    {
      Points.lon = tile_lon +. (float tile_width /. float width);
      lat = tile_lat +. (float tile_height /. float height);
    }
  )
*)

let main () =
  let lat = ref (*44.607649*) 44.6075287 in
  let lon = ref (*6.8204019*) 6.8210935 in
  let angle = ref 0. in
  let options =
    [
      ("-lat", Arg.Set_float lat, "Latitude of the viewer");
      ("-lon", Arg.Set_float lon, "Longitude of the viewer");
      ("-angle", Arg.Set_float angle, "Angle of the view");
    ]
  in
  let anon _ = () in
  let exec = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf "Usage: %s [OPTION]\n Tests Tgles3.\nOptions:" exec
  in
  Arg.parse (Arg.align options) anon usage;
  let lat, lon, angle = (!lat, !lon, !angle) in
  (*
  let ch = open_in "Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif" in
  let ({ Tiff.width; height; tile_width; tile_height; _ } as info) =
    Tiff.read_info ch
  in
  let tile_index, x, y, tile_coord, tile_coord' = coordinates info lat lon in
  let tile = Tiff.read_tile ch info tile_index in
  Format.eprintf "ZZZZ %d %d %d %f@." tile_index x y tile.{y, x};
*)
  let tile_width =
    4096
    (*2050*)
  in
  let tile_height = tile_width in
  (* Check that it is a power of two *)
  assert (tile_width land (tile_width - 1) = 0);
  let** tile = Loader.f ~size:tile_width ~lat ~lon in
  let x = tile_width / 2 in
  let y = (tile_height / 2) - 1 in
  let d = float x /. 3600. in
  let tile_coord = { Points.lon = lon -. d; lat = lat -. d } in
  let tile_coord' = { Points.lon = lon +. d; lat = lat +. d } in
  let points =
    let width = 3600 in
    let height = 3600 in
    let points = read_file "data/points.geojson" in
    Points.find tile_coord tile_coord' points
    |> List.map (fun ({ Points.coord = { lat; lon }; _ } as pt) ->
        let x =
          min (tile_width - 1)
            (truncate ((lon -. tile_coord.lon) *. float width))
        in
        let y =
          min (tile_height - 1)
            (truncate ((tile_coord'.lat -. lat) *. float height))
        in
        (pt, (x, y)))
  in
  (* Bilinear interpolation offsets for precise visibility *)
  let off_x = (lon *. 3600.) -. floor (lon *. 3600.) in
  let off_y = (lat *. 3600.) -. floor (lat *. 3600.) in
  let points =
    List.filter
      (fun (_, (dst_x, dst_y)) ->
        Visibility.test_precise
          (fun r c -> tile.{r, c})
          ~off_x ~off_y ~src_x:x ~src_y:y ~dst_x ~dst_y ())
      points
  in
  (* Bilinear interpolation for height *)
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

  match
    tri ~gl:(3, 0) ~w:tile_width ~h:tile_height ~x ~y ~lat ~lon ~angle ~height
      ~points ~tile
  with
  | Ok () -> exit 0
  | Error (`Msg msg) ->
      Sdl.log "%s@." msg;
      exit 1

let () = Lwt_main.run (main ())
