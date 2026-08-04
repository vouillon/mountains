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
let now_ms () = Brr.(Performance.now_ms G.performance)

(* GPU Timer using EXT_disjoint_timer_query_webgl2 with fallback to glFinish *)
module Gpu_timer = struct
  type ext = { time_elapsed_ext : int; gpu_disjoint : int }

  let extension : ext option ref = ref None

  let init ctx =
    let ext =
      Brr_canvas.Gl.get_extension ctx (Jstr.v "EXT_disjoint_timer_query_webgl2")
    in
    if Jv.is_some ext then begin
      let time_elapsed_ext = Jv.to_int (Jv.get ext "TIME_ELAPSED_EXT") in
      let gpu_disjoint = Jv.to_int (Jv.get ext "GPU_DISJOINT") in
      extension := Some { time_elapsed_ext; gpu_disjoint };
      Brr.Console.(
        log [ Jstr.v "GPU Timer: Using EXT_disjoint_timer_query_webgl2" ])
    end
    else
      Brr.Console.(
        log
          [ Jstr.v "GPU Timer: Extension unavailable, using glFinish fallback" ])

  (* Pending queries waiting for results *)
  type pending_query = {
    name : string;
    query : Brr_canvas.Gl.query;
    t0 : float;
  }

  let pending_queries : pending_query list ref = ref []

  (* Poll for completed queries and log results *)
  let poll_results ctx =
    let module Gl = Brr_canvas.Gl in
    match !extension with
    | None -> ()
    | Some ext ->
        (* Check if GPU was disjoint (results may be invalid) *)
        let disjoint =
          Jv.to_bool Brr_canvas.Gl.(get_parameter ctx ext.gpu_disjoint)
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
                Brr_canvas.Gl.(
                  get_query_parameter ctx pq.query query_result_available)
            in
            if available then begin
              let time_ns =
                Jv.to_float
                  Brr_canvas.Gl.(get_query_parameter ctx pq.query query_result)
              in
              let time_ms = time_ns /. 1_000_000. in
              let wall_ms = now_ms () -. pq.t0 in
              let prefix = if disjoint then "[DISJOINT] " else "" in
              Brr.Console.(
                log
                  [
                    Jstr.v
                      (Printf.sprintf "%s%s: GPU %.2fms (wall %.1fms)" prefix
                         pq.name time_ms wall_ms);
                  ]);
              Brr_canvas.Gl.delete_query ctx pq.query
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
        let query = Brr_canvas.Gl.create_query ctx in
        Brr_canvas.Gl.begin_query ctx ext.time_elapsed_ext query;
        Some { name; query; t0 = now_ms () }

  (* End timing and queue for result polling *)
  let end_query ctx pq_opt =
    match (!extension, pq_opt) with
    | Some ext, Some pq ->
        Brr_canvas.Gl.end_query ctx ext.time_elapsed_ext;
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
  | None -> f ()

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
let fog_linear = (0.18, 0.42, 0.85)
let zenith_linear = (0.01, 0.10, 0.45)

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

(* The index buffer built by [build_indices] is block-major: [n_index_blocks]
   contiguous runs of equal size, block [b] covering the sector range
   [b * index_block_size, (b + 1) * index_block_size] (blocks share their
   boundary column, so consecutive blocks are watertight). A contiguous block
   range is therefore a single [draw_elements] offset/count, which is what the
   per-frame azimuth culling in [draw] uses. *)
let index_block_size = 32
let n_index_blocks = n_sectors / index_block_size

(* type orientation = Quaternion.t *)

(* Input mode: Sensor (device orientation) vs Manual (touch/mouse drag) *)
type input_mode = Sensor | Manual

let input_mode = ref Sensor
let zoom = ref 1.0
let min_zoom = 0.5
let max_zoom = 3.0
let clamp_zoom z = max min_zoom (min max_zoom z)

(* Math Helpers *)

let rotation_matrix orientation = Quaternion.to_matrix orientation

let compute_azimuth q =
  let fwd = Quaternion.transform_vector q { x = 0.; y = 0.; z = -1.; w = 0. } in
  atan2 (-.fwd.x) fwd.y

(* GLSL Common *)

let quad_vertex_shader = [%blob "shaders/quad.vert"]
let common_fragment_header = [%blob "shaders/common_header.frag"]
let radial_vertex_common = [%blob "shaders/radial_common.vert"]

(* Terrain shader with compile-time CLC toggle for optimal code generation *)
let terrain_program =
  {
    vertex_shader =
      [%blob "shaders/terrain_prefix.vert"] ^ radial_vertex_common
      ^ [%blob "shaders/terrain_main.vert"];
    fragment_shader = common_fragment_header ^ [%blob "shaders/terrain.frag"];
    attributes = [];
  }

let triangle_program =
  {
    vertex_shader = [%blob "shaders/triangle.vert"];
    fragment_shader = [%blob "shaders/triangle.frag"];
    attributes = [];
  }

let path_program =
  {
    vertex_shader =
      [%blob "shaders/terrain_prefix.vert"] ^ radial_vertex_common
      ^ [%blob "shaders/path.vert"];
    fragment_shader = [%blob "shaders/path.frag"];
    attributes = [];
  }

let current_gpx_str = ref None

let text_program =
  {
    vertex_shader = [%blob "shaders/text.vert"];
    fragment_shader = common_fragment_header ^ [%blob "shaders/text.frag"];
    attributes = [];
  }

let ao_bake_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader = common_fragment_header ^ [%blob "shaders/ao_bake.frag"];
    attributes = [];
  }

let ao_blur_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader = common_fragment_header ^ [%blob "shaders/ao_blur.frag"];
    attributes = [];
  }

let shadow_program =
  {
    vertex_shader =
      [%blob "shaders/shadow_prefix.vert"] ^ radial_vertex_common
      ^ [%blob "shaders/shadow_main.vert"];
    fragment_shader = [%blob "shaders/shadow.frag"];
    attributes = [];
  }

(* CLC Rasterization Shader - renders CLC tile triangles to R8UI FBO *)
let clc_raster_program =
  {
    vertex_shader = [%blob "shaders/clc_raster.vert"];
    fragment_shader = [%blob "shaders/clc_raster.frag"];
    attributes = [ "in_norm_pos"; "in_color_idx" ];
  }

let water_raster_program =
  {
    vertex_shader = [%blob "shaders/water_raster.vert"];
    fragment_shader = [%blob "shaders/water_raster.frag"];
    attributes = [ "in_pos"; "in_color_idx" ];
  }

let sky_program =
  {
    vertex_shader = [%blob "shaders/sky.vert"];
    fragment_shader = common_fragment_header ^ [%blob "shaders/sky.frag"];
    attributes = [];
  }

let downsample_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader = [%blob "shaders/downsample.frag"];
    attributes = [];
  }

let normal_program =
  {
    vertex_shader = quad_vertex_shader;
    fragment_shader = common_fragment_header ^ [%blob "shaders/normal.frag"];
    attributes = [];
  }

(* Graphics Resources & Setup *)

module Gl = Brr_canvas.Gl

let build_indices w w' h =
  let t = Unix.gettimeofday () in
  let block_size = index_block_size in
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

let apply_anisotropic_filtering ?limit ctx =
  if !max_anisotropy > 1.0 then
    let v =
      match limit with
      | Some l -> Float.min l !max_anisotropy
      | None -> !max_anisotropy
    in
    (* TEXTURE_MAX_ANISOTROPY_EXT = 0x84FE *)
    Gl.tex_parameterf ctx Gl.texture_2d 0x84FE v

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
            let view = Brr.Tarray.Data_view.of_buffer buffer in
            let get_u32 off =
              Int32.to_int (Brr.Tarray.Data_view.get_uint32_le view off)
            in
            (* Header fields at known offsets:
               12: vkFormat, 16: typeSize, 20: pixelWidth, 24: pixelHeight,
               28: pixelDepth, 32: layerCount, 36: faceCount, 40: levelCount *)
            let pixel_width = get_u32 20 in
            let pixel_height = get_u32 24 in
            let level_count = get_u32 40 in
            (* Level index starts at offset 80, each entry is 24 bytes (3 x uint64) *)
            let get_u64_low =
              (* Just read low 32 bits - file offsets won't exceed 4GB *)
              get_u32
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
            (* The detail map is band-limited noise sampled 4-5x per fragment
               at grazing angles: 16x anisotropy is the single most expensive
               fetch group in the terrain shader and 4x is visually identical
               (verified against pinned A/B captures). *)
            apply_anisotropic_filtering ~limit:4.0 ctx;
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

(* Bound to the high-resolution relief units of locations that have no such
   data: a sampler of an active program must not name an incomplete texture,
   even when the branch that would read it is never taken. *)
let make_hd_placeholder ctx =
  let data = Bigarray.(Array1.create int8_unsigned c_layout 2) in
  data.{0} <- 0;
  data.{1} <- 0;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rg8 1 1 0 Gl.rg Gl.unsigned_byte
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array1 data))
    0;
  Web_utils.set_texture_params_nearest_clamp ctx Gl.texture_2d;
  Gl.bind_texture ctx Gl.texture_2d None;
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
  downsample_pid : Gl.program;
  ao_bake_pid : Gl.program;
  ao_blur_pid : Gl.program;
  relief_uniforms : Render_state.relief_uniforms;
  downsample_uniforms : Render_state.downsample_uniforms;
  ao_bake_uniforms : Render_state.ao_bake_uniforms;
  ao_blur_uniforms : Render_state.ao_blur_uniforms;
  clc_raster_pid : Gl.program;
  water_raster_pid : Gl.program;
  clc_raster_uniforms : Render_state.clc_raster_uniforms;
  water_raster_uniforms : Render_state.water_raster_uniforms;
  radial_params : Render_state.radial_params;
  nearest_sampler : Gl.sampler;
  hd_placeholder : Gl.texture;
  path_pid : Gl.program;
  path_uniforms : Render_state.path_uniforms;
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
  let downsample_pid = Web_utils.create_program ctx downsample_program in
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
  let downsample_uniforms =
    Render_state.init_downsample_uniforms ctx downsample_pid
  in
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
  let path_pid = Web_utils.create_program ctx path_program in
  let path_uniforms = Render_state.init_path_uniforms ctx path_pid in
  Gl.use_program ctx path_pid;
  Render_state.upload_path_static ctx path_uniforms radial_params;

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
    downsample_pid;
    relief_uniforms;
    downsample_uniforms;
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
    hd_placeholder = make_hd_placeholder ctx;
    path_pid;
    path_uniforms;
  }
(* Rendering Passes *)

let compute_ao ctx width height scale relief_texture nearest_sampler ao_bake_pid
    ao_blur_pid (bake_u : Render_state.ao_bake_uniforms)
    (blur_u : Render_state.ao_blur_uniforms) =
  (* Helper to create FBO and R8 Texture. [mipmap] allocates the full mip chain
     and enables trilinear + anisotropic filtering; only the texture sampled
     every frame by the terrain shader needs it (the intermediate bake target is
     read once, texel-for-texel, by the blur pass). *)
  let create_r8_target ?(mipmap = false) w h =
    let tid = Gl.create_texture ctx in
    Gl.bind_texture ctx Gl.texture_2d (Some tid);
    let levels = if mipmap then Web_utils.log2 (max w h) + 1 else 1 in
    Gl.tex_storage2d ctx Gl.texture_2d levels Gl.r8 w h;
    Web_utils.set_texture_params_linear_clamp ctx Gl.texture_2d;
    if mipmap then begin
      Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
        Gl.linear_mipmap_linear;
      apply_anisotropic_filtering ctx
    end;
    tid
  in

  (* Use Half Resolution for AO *)
  let ao_width = width / 2 in
  let ao_height = height / 2 in

  let ao_bake_tex = create_r8_target ao_width ao_height in
  let ao_final_tex = create_r8_target ~mipmap:true ao_width ao_height in

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

  (* Fill the mip chain: rendering through the FBO only wrote level 0. Must run
     after the framebuffer is unbound. *)
  Gl.bind_texture ctx Gl.texture_2d (Some ao_final_tex);
  Gl.generate_mipmap ctx Gl.texture_2d;
  Gl.bind_texture ctx Gl.texture_2d None;

  ao_final_tex

(* Sharpness of the soft-max height downsampling at each mip level (see
   downsample.frag): the deficit of a lone summit texel against a true max is
   ln(4)/k per level, so a constant 0.25 keeps distant silhouettes within
   ~5.5 m per level of the true summit heights (the previous halving
   schedule, 0.1 * 0.5^(level-1), lost 28-110 m at the levels drawn at
   40-70 km). Shared between the GPU bake and the CPU replica in
   [rendered_height], which must agree exactly. *)
let downsample_k _level = 0.25

(* [spacing_scale] is the texel spacing of [tile_texture] relative to one
   arcsecond: the near-field high-resolution grid (see [Hd_dem]) is finer than
   the base tile, and normals computed with the base spacing would come out
   that many times too flat.

   [border] leaves the outermost texel of every level at zero, which decodes as
   -500 m: that is how out-of-range fetches sink below the horizon instead of
   smearing the edge value, and it is invisible at the base tile's rim, 63 km
   from the observer. The high-resolution pyramid's rim lands a few kilometres
   away, in plain view, where such a ring reads as a wall of downward spikes:
   it is filled with real heights instead. *)
let compute_relief ?(spacing_scale = 1.0) ?(border = true) ctx width height lat
    triangle_geo tile_texture normal_pid downsample_pid
    (u : Render_state.relief_uniforms)
    (downsample_u : Render_state.downsample_uniforms) =
  assert (width = height);

  (* Not used in shader explicitly yet, using pow directly *)
  let max_level = Web_utils.log2 (max width height) in
  let levels = max_level + 1 in

  (* Heights and encoded normals live in two RG8 pyramids instead of one RGBA8:
     every consumer used to discard half of every texel. [filtered] tells them
     apart — the normal texture is the only one sampled through [texture()]
     (terrain.frag's fragment normal decode), while the height texture is only
     ever read by [texelFetch] (the vertex LOD system, which still needs the
     full mip chain) or through the AO passes' NEAREST sampler object. Total
     memory is unchanged; bytes per tap are halved on both paths. *)
  let create_rg8_pyramid ~filtered =
    let id = Gl.create_texture ctx in
    Gl.bind_texture ctx Gl.texture_2d (Some id);
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_base_level 0;
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_max_level (levels - 1);
    if filtered then begin
      Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
        Gl.linear_mipmap_linear;
      Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear
    end
    else begin
      Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
        Gl.nearest_mipmap_nearest;
      Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest
    end;
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
    Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
    if filtered then apply_anisotropic_filtering ctx;
    Gl.tex_storage2d ctx Gl.texture_2d levels Gl.rg8 width height;
    id
  in
  let tid = create_rg8_pyramid ~filtered:false in
  let nid = create_rg8_pyramid ~filtered:true in

  (* normal.frag writes heights to attachment 0 and the encoded normal to
     attachment 1, so both pyramids are filled by the same passes as before. *)
  let fb = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some fb);
  let attach_level level =
    Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0
      Gl.texture_2d tid level;
    Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment1
      Gl.texture_2d nid level
  in
  attach_level 0;
  Gl.draw_buffers ctx [ Gl.color_attachment0; Gl.color_attachment1 ];
  Gl.viewport ctx 0 0 width height;

  (* Clear both relief textures to zero first *)
  Gl.clear_color ctx 0. 0. 0. 0.;
  Gl.clear ctx Gl.color_buffer_bit;

  (* Use Scissor to leave 1-pixel border zero *)
  if border then begin
    Gl.enable ctx Gl.scissor_test;
    Gl.scissor ctx 1 1 (width - 2) (height - 2)
  end;

  Gl.use_program ctx normal_pid;
  (* Draw normals (Level 0) *)
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some tile_texture);

  Gl.bind_vertex_array ctx (Some triangle_geo);
  Gl.uniform2f ctx u.size (float width) (float height);

  (* Use the provided latitude for normals *)
  let deltax, deltay, _ = Render_state.compute_deltas ~lat in
  let deltax = deltax *. spacing_scale and deltay = deltay *. spacing_scale in
  Gl.uniform2f ctx u.delta deltax deltay;
  (* Level 0 samples full tile texture *)
  Gl.uniform2f ctx u.uv_scale 1.0 1.0;

  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  Gl.disable ctx Gl.scissor_test;

  (* Common Uniforms *)
  Gl.use_program ctx downsample_pid;
  Gl.uniform1i ctx downsample_u.source_texture 0;

  (* Start from level 1 *)

  (* Temporary texture for height downsampling (RG8) *)
  let temp_tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some temp_tid);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  Gl.tex_storage2d ctx Gl.texture_2d 1 Gl.rg8 width height;
  Gl.bind_texture ctx Gl.texture_2d None;

  (* Separate single-attachment framebuffer for the height downsample: [fb] has
     a normal target bound to attachment 1 and downsample.frag writes no second
     output, so reusing it would leave that attachment undefined. *)
  let ds_fb = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some ds_fb);
  Gl.framebuffer_texture2d ctx Gl.framebuffer Gl.color_attachment0 Gl.texture_2d
    temp_tid 0;

  let rec loop level w h =
    if level > max_level || w < 1 || h < 1 then ()
    else (
      (* 1. Downsample Height: tid(Level N-1) -> temp_tid *)
      Gl.bind_framebuffer ctx Gl.framebuffer (Some ds_fb);

      (* Bind Source: tid *)
      Gl.active_texture ctx Gl.texture0;
      Gl.bind_texture ctx Gl.texture_2d (Some tid);

      (* Use Downsample Program *)
      Gl.use_program ctx downsample_pid;
      Gl.uniform1i ctx downsample_u.level (level - 1);
      Gl.uniform1f ctx downsample_u.k (downsample_k level);

      let source_w = float (w * 2) in
      let source_h = float (h * 2) in
      Gl.uniform2f ctx downsample_u.source_size source_w source_h;

      Gl.viewport ctx 0 0 w h;

      (* Draw Quad *)
      Gl.draw_elements ctx Gl.triangles 6 Gl.unsigned_byte 0;

      (* 2. Compute Normals: temp_tid -> tid/nid(Level N) *)

      (* Bind FBO to Dest: tid and nid at Level N *)
      Gl.bind_framebuffer ctx Gl.framebuffer (Some fb);
      attach_level level;

      (* Bind Source: temp_tid *)
      Gl.active_texture ctx Gl.texture0;
      Gl.bind_texture ctx Gl.texture_2d (Some temp_tid);

      (* Use Normal Program *)
      Gl.use_program ctx normal_pid;
      Gl.uniform2f ctx u.size (float w) (float h);

      (* Scale delta for this level *)
      let scale = 2.0 ** float level in
      Gl.uniform2f ctx u.delta (deltax *. scale) (deltay *. scale);

      (* Temp texture is full size (width), but we only wrote to top-left w*h *)
      (* Scale UVs to sample only the valid region *)
      let uv_scale_x = float w /. float width in
      let uv_scale_y = float h /. float height in
      Gl.uniform2f ctx u.uv_scale uv_scale_x uv_scale_y;

      Gl.viewport ctx 0 0 w h;

      (* Clear Mip Level to Zero *)
      Gl.clear_color ctx 0. 0. 0. 0.;
      Gl.clear ctx Gl.color_buffer_bit;

      if not border then Gl.draw_elements ctx Gl.triangles 6 Gl.unsigned_byte 0
      else if w > 2 && h > 2 then (
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
  Gl.delete_framebuffer ctx ds_fb;
  Gl.delete_framebuffer ctx fb;

  Gl.bind_vertex_array ctx None;

  (tid, nid)

let rasterize_clc_tiles ctx ~lat ~lon ~w ~clc_tiles ~clc_raster_pid
    ~water_raster_pid (clc_u : Render_state.clc_raster_uniforms)
    (water_u : Render_state.water_raster_uniforms) =
  (* CLC GPU Rasterization setup.

     A clipmap level spans half the ground of the one above it at the same texel
     count, so a texel subtends a constant angle wherever you stand: at
     [cover_map_size] = 1024 that is 1/512 to 1/256 of the distance, i.e. 2 to 4
     pixels at a 60 degree field of view over 1080. Material boundaries came out
     as a staircase of that width. 2048 halves it to 1-2 pixels, which is where
     the bilinear blend in [sampleCLCBilinear] stops being visible as steps.
     The shader reads the size from [textureSize] and the count from
     [u_numLevels], so neither is baked into it. Costs 28 MiB per location
     against 7. *)
  let cover_map_size = 2048 in

  (* Create FBO for CLC rasterization *)
  let clc_fbo = Gl.create_framebuffer ctx in

  (* Create R8UI texture array for CLC output (7 levels) *)
  let clc_levels = 7 in
  let cover_map_texture = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d_array (Some cover_map_texture);
  Web_utils.set_texture_params_nearest_clamp ctx Gl.texture_2d_array;
  Gl.tex_storage3d ctx Gl.texture_2d_array 1 Gl.r8ui cover_map_size
    cover_map_size clc_levels;
  (* Sea clear value (index 44 = Sea and ocean) — clearBufferuiv requires 4 components *)
  let sea_clear_val = Brr.Tarray.create Brr.Tarray.Uint32 4 in
  (Brr.Tarray.to_bigarray1 sea_clear_val).{0} <- 44l;

  (* Depth buffer for overdraw prevention (smaller features drawn first win).
     One buffer, not one per level: the render loop below is level-outer and
     clears depth at the top of every level, so only ever one is live. At 2048
     the seven it used to allocate would have been 56 MiB of transient. *)
  let clc_depth_rb =
    let rb = Gl.create_renderbuffer ctx in
    Gl.bind_renderbuffer ctx Gl.renderbuffer (Some rb);
    Gl.renderbuffer_storage ctx Gl.renderbuffer Gl.depth_component16
      cover_map_size cover_map_size;
    rb
  in

  (* Attach to FBO (attach layer 0 initially) *)
  Gl.bind_framebuffer ctx Gl.framebuffer (Some clc_fbo);
  Gl.framebuffer_texture_layer ctx Gl.framebuffer Gl.color_attachment0
    cover_map_texture 0 0;
  Gl.framebuffer_renderbuffer ctx Gl.framebuffer Gl.depth_attachment
    Gl.renderbuffer clc_depth_rb;
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
  (* Center calculus from User requirements *)
  (* The point at (w/2, h/2) is at arcsec_floor(lat/lon). *)
  (* This corresponds to the center of the pixel at index w/2. *)
  (* Pixel centers are at integral arcseconds. *)
  (* Therefore, the geometric center of the tile (w x h area) is offset by -0.5 arcsec from the anchor coordinate. *)
  let half_arcsec = 0.5 /. 3600. in
  let center_lat_deg =
    (float (Web_utils.arcsec_floor lat) /. 3600.) -. half_arcsec
  in
  let center_lon_deg =
    (float (Web_utils.arcsec_floor lon) /. 3600.) -. half_arcsec
  in

  (* Largest level (Index 6) matches DEM extent exactly *)
  let dem_extent_deg = float w /. 3600. in

  (* Pre-calculate view bounds for each level *)
  (* Level 6: extent = dem_extent. Scale = 1. *)
  (* Level 0: extent = dem_extent / 64. Scale = 64. *)
  let level_bounds =
    Array.init 7 (fun level ->
        (* level 0 is smallest, level 6 is largest *)
        (* scale factor relative to DEM: 1.0 for level 6, 1/64 for level 0 *)
        let factor = 2.0 ** float (level - 6) in
        let extent = dem_extent_deg *. factor in

        let min_lat = center_lat_deg -. (extent /. 2.) in
        let min_lon = center_lon_deg -. (extent /. 2.) in
        let max_lat = min_lat +. extent in
        let max_lon = min_lon +. extent in

        (min_lon, min_lat, extent, extent, max_lon, max_lat))
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

    (* Switch FBO to this level ONCE. The depth attachment is the same buffer
       for every level and was bound before the loop; the clear below is what
       makes it reusable. *)
    Gl.framebuffer_texture_layer ctx Gl.framebuffer Gl.color_attachment0
      cover_map_texture 0 level;

    (* Clear color to sea and depth buffer *)
    Gl.clear_bufferuiv ctx Gl.color 0 sea_clear_val;
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
  Gl.bind_renderbuffer ctx Gl.renderbuffer None;
  Gl.delete_framebuffer ctx clc_fbo;
  Gl.delete_renderbuffer ctx clc_depth_rb;

  cover_map_texture

let draw_shadows ~shadow_pid ~shadow_fbo ~shadow_map
    (shadow_uniforms : Render_state.shadow_uniforms) ~matrices ~terrain_geo
    ~index_count ~(radial_params : Render_state.radial_params) ~relief_texture
    ~hd_relief_texture ctx =
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
  (* The shadow vertex shader shares radial_common.vert, so the near-field
     high-resolution heights must be bound here too: the bake then inherits
     the HD terrain. *)
  Gl.active_texture ctx Gl.texture6;
  Gl.bind_texture ctx Gl.texture_2d (Some hd_relief_texture);
  Gl.active_texture ctx Gl.texture0;

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

    (* A ring can only cast into this cascade's ortho box if it lies within
       shadow_radius * sqrt 2 + depth_range of the centre (constants mirrored
       from [calculate_shadow_matrices]); ring [r] sits at radius
       grid_scale * (exp (grid_k * r) - 1), see radial_common.vert. Rings are
       contiguous within each index block, so restrict every block to the
       strips actually needed. *)
    let ring_limit =
      let split_radius =
        if layer = 0 then 2000.0 else if layer = 1 then 8000.0 else 25000.0
      in
      let shadow_radius = split_radius *. 1.5 in
      let depth_range = max 10000. (shadow_radius *. 2.) in
      let reach = (shadow_radius *. sqrt 2.) +. depth_range in
      let gk = radial_params.Render_state.grid_k in
      let gs = radial_params.Render_state.grid_scale in
      let r = int_of_float (ceil (log ((reach /. gs) +. 1.) /. gk)) + 1 in
      min (n_rings - 1) (max 1 r)
    in
    let indices_per_block = index_count / n_index_blocks in
    let strip_indices = indices_per_block / (n_rings - 1) in
    let block_count = ring_limit * strip_indices in

    (* Render 4 rotations to cover full terrain *)
    for rotation = 0 to 3 do
      Gl.uniform1f ctx shadow_uniforms.snapped_alpha rotation_angles.(rotation);
      if ring_limit = n_rings - 1 then
        Gl.draw_elements ctx Gl.triangle_strip index_count Gl.unsigned_int 0
      else
        for b = 0 to n_index_blocks - 1 do
          Gl.draw_elements ctx Gl.triangle_strip block_count Gl.unsigned_int
            (b * indices_per_block * 4)
        done
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
let bind_terrain_textures ctx ~relief_texture ~relief_normal_texture ~ao_texture
    ~detail_map ~shadow_map ~cover_map_texture ~palette_texture
    ~hd_relief_texture ~hd_relief_normal_texture =
  let open Brr_canvas in
  Gl.active_texture ctx Gl.texture1;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_texture);
  Gl.active_texture ctx Gl.texture2;
  Gl.bind_texture ctx Gl.texture_2d (Some relief_normal_texture);
  Gl.active_texture ctx Gl.texture3;
  Gl.bind_texture ctx Gl.texture_2d (Some ao_texture);
  Gl.active_texture ctx Gl.texture5;
  Gl.bind_texture ctx Gl.texture_2d (Some detail_map);
  Gl.active_texture ctx Gl.texture6;
  Gl.bind_texture ctx Gl.texture_2d (Some hd_relief_texture);
  Gl.active_texture ctx Gl.texture9;
  Gl.bind_texture ctx Gl.texture_2d (Some hd_relief_normal_texture);
  Gl.active_texture ctx Gl.texture4;
  Gl.bind_texture ctx Gl.texture_2d_array (Some shadow_map);
  Gl.active_texture ctx Gl.texture7;
  Gl.bind_texture ctx Gl.texture_2d_array (Some cover_map_texture);
  Gl.active_texture ctx Gl.texture8;
  Gl.bind_texture ctx Gl.texture_2d (Some palette_texture);
  Gl.active_texture ctx Gl.texture0
(* Text Rendering *)

(* The labels are read outdoors, where sunlight reflecting off the screen adds a
   veil that flattens everything shown: plain black glyphs, 9:1 against the sky
   indoors, fall to about 2:1 under it, and over shaded forest they are already
   invisible without any veil. So the glyphs are bold, and each carries a white
   halo: together they span the display range whatever lies behind them, and
   measure 3:1 under the same veil. The weight is what does most of that work,
   which is why the halo can stay narrow enough not to blur the letterforms. *)
let text_font = "bold 48px sans"

(* A stroke straddles the outline it follows, so the halo reaches out by half of
   this width. *)
let halo_width = 3.5

type lazy_text = {
  text : string;
  (* The texture, and the transform taking the unit quad to it: the halo makes
     the texture larger than the glyph box, and that transform is what keeps
     the glyphs where they would be without one (see
     [prepare_text_immediate]). *)
  mutable texture : (Gl.texture * Matrix.t) option;
}

let text_canvas = Brr_canvas.Canvas.of_el (Brr.El.canvas [])
let text_ctx = Brr_canvas.C2d.get_context text_canvas

let prepare_text_immediate ctx text =
  let open Brr_canvas in
  let text = Jstr.v text in
  C2d.set_font text_ctx (Jstr.v text_font);
  let m = C2d.measure_text text_ctx text in
  let ascent = C2d.Text_metrics.font_bounding_box_ascent m in
  let descent = C2d.Text_metrics.font_bounding_box_descent m in
  let left = C2d.Text_metrics.actual_bounding_box_left m in
  let right = C2d.Text_metrics.actual_bounding_box_right m in
  let glyph_w = truncate (left +. right +. 0.5) in
  let glyph_h = truncate (ascent +. descent +. 0.5) in
  (* Room for the halo around the glyph box, plus one pixel so the bilinear
     filter never reaches a texel the halo only partly covers. *)
  let pad = truncate (ceil (halo_width /. 2.)) + 1 in
  let w = glyph_w + (2 * pad) in
  let h = glyph_h + (2 * pad) in
  if w > Brr_canvas.Canvas.w text_canvas then
    Brr_canvas.Canvas.set_w text_canvas (2 * w);
  if h > Brr_canvas.Canvas.h text_canvas then
    Brr_canvas.Canvas.set_h text_canvas h;
  (* Resizing the canvas resets the 2d context, so the drawing state has to be
     set after that, not before. *)
  C2d.set_font text_ctx (Jstr.v text_font);
  C2d.set_fill_style text_ctx (C2d.color (Jstr.v "black"));
  C2d.set_stroke_style text_ctx (C2d.color (Jstr.v "white"));
  C2d.set_line_width text_ctx halo_width;
  (* Round joins: a miter shoots spikes out of the sharp corners of the glyphs
     at this stroke width. *)
  C2d.set_line_join text_ctx C2d.Line_join.round;
  let x = left +. float pad in
  let y = ascent +. float pad in
  C2d.stroke_text text_ctx text ~x ~y;
  C2d.fill_text text_ctx text ~x ~y;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  (* The labels are blended with [one, one_minus_src_alpha], which expects
     premultiplied texels. Black glyphs on their own did not care, their colour
     being zero either way; the halo does. *)
  Gl.pixel_storei ctx Gl.unpack_premultiply_alpha_webgl 1;
  Gl.tex_image2d_of_source ctx Gl.texture_2d 0 Gl.rgba w h 0 Gl.rgba
    Gl.unsigned_byte
    (Gl.Tex_image_source.of_canvas_el text_canvas);
  Gl.pixel_storei ctx Gl.unpack_premultiply_alpha_webgl 0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.bind_texture ctx Gl.texture_2d None;
  C2d.clear_rect text_ctx ~x:0. ~y:0. ~w:(float w) ~h:(float h);
  (* Callers place a box one unit tall and [glyph_w / glyph_h] wide, with its
     bottom-left corner at the origin; the padded texture has to cover slightly
     more than that on all four sides, leaving the glyphs themselves at exactly
     the size and position they had before the halo. Leftmost factor first, so
     the quad is scaled up to the texture, then slid back by the padding. *)
  let p = float pad /. float glyph_h in
  let quad =
    Matrix.(
      scale (float w /. float glyph_h) (float h /. float glyph_h) 1.
      * translate (-.p) (-.p) 0.)
  in
  (tid, quad)

let prepare_text _ctx text = { text; texture = None }

(* Scratch matrix, reused across frames to avoid allocating in the render loop.
   Must not be passed as [transform] to [draw_text]. *)
let text_transform : Matrix.t = Array.make 16 0.

let draw_text ctx (uniforms : Render_state.text_uniforms) transform buffer view
    (lazy_text : lazy_text) =
  let tid, quad =
    match lazy_text.texture with
    | Some t -> t
    | None ->
        let t = prepare_text_immediate ctx lazy_text.text in
        lazy_text.texture <- Some t;
        t
  in
  let open Brr_canvas in
  Matrix.mult_into text_transform quad transform;
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Matrix.blit text_transform buffer;
  Gl.uniform_matrix4fv ctx uniforms.transform false view;
  Gl.draw_elements ctx Gl.triangle_strip 4 Gl.unsigned_byte 0
(* Texture unbind removed - next draw_text or terrain pass rebinds anyway *)

(* Main Draw Loop *)

let scale = (*2. *. 27. /. 24.*) 3.2
let text_height = 0.07

(* Scratch matrix for the per-POI transforms (see [text_transform]). *)
let poi_transform : Matrix.t = Array.make 16 0.

(* Helper functions for orientation control *)
let get_inclination_rad q =
  let q_inv = Quaternion.conjugate q in
  let up_cam =
    Quaternion.transform_vector q_inv { x = 0.; y = 0.; z = 1.; w = 0. }
  in
  atan2 up_cam.x up_cam.y

let snap_inclination ~current_locked sensor_q =
  let q_inv = Quaternion.conjugate sensor_q in
  let up =
    Quaternion.transform_vector q_inv { x = 0.; y = 0.; z = 1.; w = 0. }
  in
  let screen_mag = sqrt ((up.x *. up.x) +. (up.y *. up.y)) in
  if screen_mag < 0.5 then current_locked
  else
    let raw = atan2 up.x up.y in
    let orientations = [ 0.; Float.pi /. 2.; -.Float.pi /. 2.; Float.pi ] in
    let angle_dist a b =
      let d = mod_float (a -. b +. Float.pi) (2. *. Float.pi) in
      abs_float (if d < 0. then d +. Float.pi else d -. Float.pi)
    in
    let dist_to_current = angle_dist raw current_locked in
    if dist_to_current < Float.pi /. 3. then current_locked
    else
      List.fold_left
        (fun best o ->
          if angle_dist raw o < angle_dist raw best then o else best)
        (List.hd orientations) orientations

let nearest_inclination sensor_q =
  let raw = get_inclination_rad sensor_q in
  let orientations = [ 0.; Float.pi /. 2.; -.Float.pi /. 2.; Float.pi ] in
  let angle_dist a b =
    let d = mod_float (a -. b +. Float.pi) (2. *. Float.pi) in
    abs_float (if d < 0. then d +. Float.pi else d -. Float.pi)
  in
  List.fold_left
    (fun best o -> if angle_dist raw o < angle_dist raw best then o else best)
    (List.hd orientations) orientations

let screen_inclination q =
  let angle = get_inclination_rad q in
  -.angle *. 180. /. Float.pi

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

  (* 4. Apply effective pitch delta around Horizon Right Axis *)
  let effective_db = clamped_theta -. theta in

  (* Compute Horizon Right Axis = Cross(Fwd, WorldUp{0,0,1}).
     Fwd x WorldUp = (fx, fy, fz) x (0,0,1) = (fy, -fx, 0). *)
  let rx = fwd.y in
  let ry = -.fwd.x in
  let len_sq = (rx ** 2.) +. (ry ** 2.) in

  let q_pitch =
    if len_sq > 0.000001 then
      let len = sqrt len_sq in
      Quaternion.from_axis_angle
        { x = rx /. len; y = ry /. len; z = 0.; w = 0. }
        effective_db
    else
      (* Fallback for gimbal lock: use Local Right *)
      let axis =
        Quaternion.transform_vector q_yawed { x = 1.; y = 0.; z = 0.; w = 0. }
      in
      Quaternion.from_axis_angle axis effective_db
  in
  (* Apply Pitch Globally (Left Multiply) since axis is in World Space *)
  Quaternion.mult q_pitch q_yawed

(* Everything that depends on the current location. [load_location] bakes a
   whole record and publishes it in [session] in a single assignment, so a frame
   sees either the previous location in full or the new one in full. The
   textures listed here are owned by the location: they are deleted when it is
   replaced. The session-wide resources (programs, geometry, detail map,
   palette, shadow map) outlive every location and live in
   [graphics_resources]. *)
type hd_relief = {
  hd_relief_texture : Gl.texture;
  hd_relief_normal_texture : Gl.texture;
}
(** The near-field high-resolution relief of a location (see [Hd_dem]), when the
    service had data for it. *)

let show_labels = ref true

type location = {
  height : float;
  points : (lazy_text * (float * float * float)) list;
  relief_texture : Gl.texture;
  relief_normal_texture : Gl.texture;
  ao_texture : Gl.texture;
  cover_map_texture : Gl.texture;
  hd : hd_relief option;
  gpx_path : (Gl.vertex_array_object * Gl.buffer * int) option;
}

let session : location option ref = ref None

(* The POI textures are created lazily by [draw_text], hence the option. *)
let delete_location ctx
    {
      relief_texture;
      relief_normal_texture;
      ao_texture;
      cover_map_texture;
      points;
      hd;
      gpx_path;
      _;
    } =
  Option.iter
    (fun (vao, vbo, _) ->
      Gl.delete_vertex_array ctx vao;
      Gl.delete_buffer ctx vbo)
    gpx_path;
  Gl.delete_texture ctx relief_texture;
  Gl.delete_texture ctx relief_normal_texture;
  Gl.delete_texture ctx ao_texture;
  Gl.delete_texture ctx cover_map_texture;
  Option.iter
    (fun { hd_relief_texture; hd_relief_normal_texture } ->
      Gl.delete_texture ctx hd_relief_texture;
      Gl.delete_texture ctx hd_relief_normal_texture)
    hd;
  List.iter
    (fun ({ texture; _ }, _) ->
      match texture with
      | Some (tid, _) -> Gl.delete_texture ctx tid
      | None -> ())
    points

let draw terrain_pid terrain_geo triangle_pid text_pid text_geo path_pid
    ~(terrain_uniforms : Render_state.terrain_uniforms)
    ~(triangle_uniforms : Render_state.triangle_uniforms)
    ~(text_uniforms : Render_state.text_uniforms)
    ~(path_uniforms : Render_state.path_uniforms) ~proj_ba ~transform_ba
    ~inv_view_ba ~proj_ta ~transform_ta ~inv_view_ta ~location ~orientation
    ~index_count ~sky_pid ~sky_uniforms
    ~(radial_params : Render_state.radial_params) canvas ctx =
  (* Field reads only: the location record must not be rebuilt per frame. *)
  let { height; points; _ } = location in
  (* Poll for completed GPU timer queries *)
  Gpu_timer.poll_results ctx;
  let canvas = Brr_canvas.Canvas.of_el canvas in
  let canvas_width = Brr_canvas.Canvas.w canvas in
  let canvas_height = Brr_canvas.Canvas.h canvas in
  Gl.viewport ctx 0 0 canvas_width canvas_height;
  let aspect = float canvas_width /. float canvas_height in
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
  (* POI labels keep a constant size relative to the smaller canvas dimension.
     [x_scale] and [y_scale] are both derived from the larger one, so without
     this correction the labels grow with the width of a landscape window: fine
     on a phone, oversized on a desktop. *)
  let text_height = text_height /. max 1. aspect in
  let proj = Matrix.project ~x_scale ~y_scale ~near_plane:1. in
  let points =
    (* Plane position and silhouette-anchored height are precomputed per
       location (see [load_location]); only the projection is per frame. *)
    List.filter_map
      (fun (pt, (px, py, z)) ->
        let r = Matrix.({ x = px; y = py; z; w = 1. } *< transform) in
        let rz = -.r.z in
        if rz > 1. && abs_float (r.x /. rz) < 1. then
          Some (pt, r.x /. rz, r.y /. rz)
        else None)
      points
  in
  (* Constant per frame: hoisted out of the POI loops below. *)
  let inclination = screen_inclination orientation *. pi /. 180. in
  let points =
    let pos = ref [] in
    let angle = inclination +. (pi /. 4.) in
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

  (* Clear color (the fog colour) is static, set once in [load_location] *)
  Gl.clear ctx (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

  Gl.depth_mask ctx true;
  Gl.depth_func ctx Gl.lequal;
  Gl.use_program ctx terrain_pid;
  Gl.enable ctx Gl.depth_test;
  Gl.enable ctx Gl.cull_face';
  (* Determine snapped alpha - changes with camera orientation.
     [grid_k] is both the angular width of one sector and the snapping step. *)
  let grid_k = radial_params.Render_state.grid_k in
  let fwd =
    Quaternion.transform_vector orientation { x = 0.; y = 0.; z = -1.; w = 0. }
  in
  let current_azimuth = atan2 (-.fwd.Matrix.x) fwd.Matrix.y in
  let snapped_alpha = floor ((current_azimuth /. grid_k) +. 0.5) *. grid_k in
  Gl.uniform1f ctx terrain_uniforms.snapped_alpha snapped_alpha;
  (* Azimuth culling and coverage: the mesh is a 90-degree wedge centred on
     [snapped_alpha]; think of a virtual 64-block circle (4 wedge rotations of
     16 blocks, 5.625 degrees each). The frustum needs blocks within [delta]
     of the forward azimuth: delta is exact and roll-aware (the frustum's
     azimuth extremes are at its four corner rays: along a frustum edge the
     direction is affine in the edge parameter, so the azimuth is monotonic),
     with two sectors of margin for the triangle span and the [snapped_alpha]
     quantisation. If the world vertical lies inside the frustum (in view
     space it is (r.z, u.z, -+f.z), so the test is |r.z| <= |f.z|/x_scale and
     |u.z| <= |f.z|/y_scale, written with [not (>)] so NaN falls back too),
     every azimuth is needed. Blocks outside the central wedge are drawn from
     the same index buffer with [snapped_alpha] rotated by quarter turns
     (exactly 512 sectors, so the seams are watertight), and only out to
     [extra_ring_cap]: off-wedge terrain exists only when pitched down, and is
     then visible only below the frustum's highest ray. *)
  let g_half, extra_ring_cap =
    let tx = 1. /. x_scale and ty = 1. /. y_scale in
    (* r and u are columns 0 and 1 of [orientation]'s rotation (column 2 is
       -[fwd]), expanded as scalars — cf. [Quaternion.to_matrix] — so that no
       vector is allocated here. *)
    let qx = orientation.Quaternion.x
    and qy = orientation.Quaternion.y
    and qz = orientation.Quaternion.z
    and qw = orientation.Quaternion.w in
    let xx = qx *. qx and yy = qy *. qy and zz = qz *. qz in
    let xy = qx *. qy and xz = qx *. qz and yz = qy *. qz in
    let wx = qw *. qx and wy = qw *. qy and wz = qw *. qz in
    let rx = 1. -. (2. *. (yy +. zz))
    and ry = 2. *. (xy +. wz)
    and rz = 2. *. (xz -. wy) in
    let ux = 2. *. (xy -. wz)
    and uy = 1. -. (2. *. (xx +. zz))
    and uz = 2. *. (yz +. wx) in
    let fx = fwd.Matrix.x and fy = fwd.Matrix.y and fz = fwd.Matrix.z in
    let afz = abs_float fz in
    let g_half =
      if (not (abs_float rz > tx *. afz)) && not (abs_float uz > ty *. afz) then
        32
      else
        (* Corner (sa, sb) points along d = sa.tx.r + sb.ty.u + f, at azimuth
           offset atan2 (f x d, f . d) = atan2 (sa.ca + sb.cb, h + sa.da +
           sb.db) from [current_azimuth] (horizontal components only). *)
        let ax = tx *. rx and ay = tx *. ry in
        let bx = ty *. ux and by = ty *. uy in
        let ca = (fx *. ay) -. (fy *. ax) and cb = (fx *. by) -. (fy *. bx) in
        let da = (fx *. ax) +. (fy *. ay) and db = (fx *. bx) +. (fy *. by) in
        let h = (fx *. fx) +. (fy *. fy) in
        let o1 = abs_float (atan2 (ca +. cb) (h +. da +. db)) in
        let o2 = abs_float (atan2 (ca -. cb) (h +. da -. db)) in
        let o3 = abs_float (atan2 (cb -. ca) (h -. da +. db)) in
        let o4 = abs_float (atan2 (-.ca -. cb) (h -. da -. db)) in
        let delta =
          Float.max (Float.max o1 o2) (Float.max o3 o4) +. (2. *. grid_k)
        in
        let n =
          int_of_float (ceil (delta /. (float index_block_size *. grid_k)))
        in
        if n < 1 then 1 else if n > 32 then 32 else n
    in
    let extra_ring_cap =
      if g_half <= n_index_blocks / 2 then 0
      else
        (* Highest frustum ray: pitch plus the cone half-angle (roll-safe).
           When extras are needed it is normally below the horizon, and a
           ground point d metres below the camera is then within
           d / tan(-e_max) plus what the curvature drop adds; the +502 spans
           the height range floor and the eye, 1.05 and the +1 ring absorb
           the approximations. NaN or a near-horizontal bound draws full
           rings. *)
        let tan_beta = sqrt ((tx *. tx) +. (ty *. ty)) in
        let e_max = asin (Float.max (-1.) (Float.min 1. fz)) +. atan tan_beta in
        if e_max < -0.001 then
          let depth = height +. 502. in
          let t = Float.tan (-.e_max) in
          let r0 = depth /. t in
          let r = (depth +. (r0 *. r0 *. 6.8306e-8)) /. t *. 1.05 in
          let gs = radial_params.Render_state.grid_scale in
          min (n_rings - 1)
            (max 1 (int_of_float (ceil (log ((r /. gs) +. 1.) /. grid_k)) + 1))
        else n_rings - 1
    in
    (g_half, extra_ring_cap)
  in
  let gmin = -g_half and gmax = g_half - 1 in
  let indices_per_block = index_count / n_index_blocks in
  let strip_indices = indices_per_block / (n_rings - 1) in
  (* Draw the blocks of one wedge rotation covering circle blocks
     [g_lo, g_hi] (their wedge-local index is g + b_off), clipped to the
     needed range: the central wedge in one call with full rings, the extra
     rotations per block with rings capped. *)
  let draw_wedge_seg rot g_lo g_hi b_off =
    let lo = max gmin g_lo and hi = min gmax g_hi in
    if lo <= hi then begin
      Gl.uniform1f ctx terrain_uniforms.snapped_alpha
        (snapped_alpha +. (float rot *. (pi /. 2.)));
      let b_lo = lo + b_off and b_hi = hi + b_off in
      if rot = 0 then
        Gl.draw_elements ctx Gl.triangle_strip
          ((b_hi - b_lo + 1) * indices_per_block)
          Gl.unsigned_int
          (b_lo * indices_per_block * 4)
      else
        for b = b_lo to b_hi do
          Gl.draw_elements ctx Gl.triangle_strip
            (extra_ring_cap * strip_indices)
            Gl.unsigned_int
            (b * indices_per_block * 4)
        done
    end
  in
  (* Matrices - change with camera orientation and aspect ratio *)
  Matrix.blit proj proj_ba;
  Gl.uniform_matrix4fv ctx terrain_uniforms.proj false proj_ta;
  Matrix.blit transform transform_ba;
  Gl.uniform_matrix4fv ctx terrain_uniforms.transform false transform_ta;
  Gl.bind_vertex_array ctx (Some terrain_geo);
  draw_wedge_seg 0 (-8) 7 8;
  draw_wedge_seg 1 8 23 (-8);
  draw_wedge_seg 3 (-24) (-9) 24;
  draw_wedge_seg 2 24 31 (-24);
  draw_wedge_seg 2 (-32) (-25) 40;
  Gl.bind_vertex_array ctx None;

  (* Draw 3D GPX Path *)
  (match location.gpx_path with
  | Some (vao, _vbo, count) ->
      Gl.use_program ctx path_pid;
      Gl.uniform_matrix4fv ctx path_uniforms.Render_state.u_transform false
        transform_ta;
      Gl.uniform_matrix4fv ctx path_uniforms.Render_state.u_proj false proj_ta;
      Gl.uniform2f ctx path_uniforms.Render_state.u_viewport
        (float canvas_width) (float canvas_height);
      Gl.bind_vertex_array ctx (Some vao);
      Gl.draw_arrays ctx Gl.triangle_strip 0 (count * 4)
  | None -> ());

  (* Draw Sky (Optimized: Z=1.0, Blit, Late Draw) *)
  Gl.depth_mask ctx false;
  (* Draw if Z <= 1.0 (Far Plane) *)
  Gl.disable ctx Gl.cull_face';
  Gl.use_program ctx sky_pid;
  (* Compute Inverse View *)
  let inv_view = Matrix.inverse transform in
  Matrix.blit inv_view inv_view_ba;
  Gl.uniform_matrix4fv ctx sky_uniforms.Render_state.inv_view false inv_view_ta;
  Gl.uniform2f ctx sky_uniforms.Render_state.sky_params x_scale y_scale;
  Gl.draw_arrays ctx Gl.triangle_strip 0 4;

  if !show_labels then begin
    (* VAO text_geo is still bound, reused for POIs *)
    Gl.disable ctx Gl.depth_test;
    Gl.enable ctx Gl.blend;
    Gl.blend_func ctx Gl.one Gl.one_minus_src_alpha;
    Gl.bind_vertex_array ctx (Some text_geo);

    (* 1. Triangles *)
    Gl.use_program ctx triangle_pid;
    (* Everything but the final translation is constant per frame. *)
    let triangle_prefix shown =
      let sx = 0.6 *. text_height *. x_scale /. text_scale in
      let sy = 0.6 *. text_height *. y_scale /. text_scale in
      let angle = if shown then -.pi /. 4. else 0. in
      Matrix.(rotate_z (angle +. inclination) * scale sx sy 1.)
    in
    let triangle_prefix_shown = triangle_prefix true in
    let triangle_prefix_hidden = triangle_prefix false in
    List.iter
      (fun (_, x, y, shown) ->
        let x = x *. x_scale in
        let y = y *. y_scale in
        Matrix.mult_into poi_transform
          (if shown then triangle_prefix_shown else triangle_prefix_hidden)
          (Matrix.translate x y 0.);
        Matrix.blit poi_transform transform_ba;
        Gl.uniform_matrix4fv ctx triangle_uniforms.transform false transform_ta;
        if shown then Gl.uniform4f ctx triangle_uniforms.color 0. 0. 0. 1.
        else Gl.uniform4f ctx triangle_uniforms.color 0. 0. 0. 0.4;
        Gl.draw_elements ctx Gl.triangles 3 Gl.unsigned_byte 0)
      points;

    (* 2. Text *)
    Gl.use_program ctx text_pid;
    let text_prefix =
      let sx = text_height *. x_scale /. text_scale in
      let sy = text_height *. y_scale /. text_scale in
      Matrix.(
        translate 0.7 (-0.5) 0.
        * rotate_z ((pi /. 4.) +. inclination)
        * scale sx sy 1.)
    in
    List.iter
      (fun (texture, x, y, shown) ->
        if shown then begin
          let x = x *. x_scale in
          let y = y *. y_scale in
          Matrix.mult_into poi_transform text_prefix (Matrix.translate x y 0.);
          draw_text ctx text_uniforms poi_transform transform_ba transform_ta
            texture
        end)
      points;

    Gl.disable ctx Gl.blend;
    Gl.bind_vertex_array ctx None
  end

(* Event loop *)

let current_orientation = ref Quaternion.identity
let target_orientation = ref Quaternion.identity
let sensor_orientation = ref Quaternion.identity
let last_screen_angle = ref 0.
let locked_inclination = ref 0.
let fab_orientation = ref 0.
let fab_els : Brr.El.t list ref = ref []
let is_dragging = ref false
let velocity = ref (0., 0.)
let last_input_time = ref 0.

(* Current position state for URL updates *)
let current_lat = ref 0.0
let current_lon = ref 0.0
let last_url_update_orientation = ref Quaternion.identity
let last_url_update_zoom = ref 1.0
let last_url_update_time = ref 0.

let format_float f =
  let s = Printf.sprintf "%.5f" f in
  let s =
    if String.contains s '.' then
      let len = String.length s in
      let rec loop i =
        if i < 0 then s
        else if s.[i] = '0' then loop (i - 1)
        else if s.[i] = '.' then String.sub s 0 i
        else String.sub s 0 (i + 1)
      in
      loop (len - 1)
    else s
  in
  s

(* The camera as a link -- position, heading, pitch and zoom, the parameters the
   app reads back on startup. Shared between the history updates and the
   [currentViewUrl] hook the share button calls: the address bar only catches up
   once the camera has settled, so reading it there would hand over a view a
   second stale, or none at all early on. *)
let current_view_uri () =
  let lat = !current_lat in
  let lon = !current_lon in
  let alpha_deg = compute_azimuth !current_orientation *. 180. /. Float.pi in
  let roll, _, _ = Quaternion.to_euler !current_orientation in
  let beta_deg = roll *. 180. /. Float.pi in
  let z = !zoom in
  let search =
    Jstr.v
      (Printf.sprintf "?lat=%s&lon=%s&alpha=%.0f&beta=%.0f&zoom=%.2f"
         (format_float lat) (format_float lon) alpha_deg beta_deg z)
  in
  Brr.Uri.with_query_params
    (Brr.Window.location Brr.G.window)
    (Brr.Uri.Params.of_jstr search)

let update_url_params ?(push = false) () =
  let uri = current_view_uri () in
  let history = Jv.get Jv.global "history" in
  let action = if push then "pushState" else "replaceState" in
  ignore
    (Jv.call history action
       [| Jv.null; Jv.of_string ""; Jv.of_jstr (Brr.Uri.to_jstr uri) |])

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
  let calculate_tau () =
    let min_tau = 0.02 in
    let max_tau = 0.15 in
    let t = (log !zoom -. log min_zoom) /. (log max_zoom -. log min_zoom) in
    let t = max 0. (min 1. t) in
    let t = t *. t in
    min_tau +. (t *. (max_tau -. min_tau))
  in
  let rec loop prev_orientation prev_zoom =
    let t = now_ms () in
    let dt = t -. !last_frame_time in
    last_frame_time := t;

    if !input_mode = Sensor then begin
      (* Sensor Mode: Adaptive Smoothing using SLERP
         Tau (time constant) varies with FOV to prevent jitter when zoomed in.
         - Wide FOV (Zoomed Out): Tau = 0.1s (Fast response)
         - Narrow FOV (Zoomed In): Tau = 0.5s (Slow, smooth response)
      *)
      let tau = calculate_tau () in
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
      (* Gravity Stabilization: Lock Camera Inclination to Device Inclination
         We align the screen-space vertical of the camera with that of the device. *)
      locked_inclination :=
        snap_inclination ~current_locked:!locked_inclination !sensor_orientation;
      let target_inclination = !locked_inclination in
      let curr_inclination = get_inclination_rad !current_orientation in

      (* Compute Error with Angle Wrapping [-pi, pi] *)
      let normalize_angle a =
        let pi = Float.pi in
        let a = mod_float (a +. pi) (2. *. pi) in
        if a < 0. then a +. pi else a -. pi
      in
      let error = normalize_angle (target_inclination -. curr_inclination) in

      (* Apply Correction (Rotation around Forward Axis) *)
      let tau = 0.2 in
      let alpha = 1. -. exp (-.dt /. (tau *. 1000.)) in
      let correction = error *. alpha in
      let q_corr =
        Quaternion.from_axis_angle { x = 0.; y = 0.; z = 1.; w = 0. } correction
      in
      (* Apply correction in Camera Space (Post-multiply) *)
      current_orientation := Quaternion.mult !current_orientation q_corr
    end;
    let orientation = !current_orientation in
    let z = !zoom in

    (* Optimization: Only draw if change is visible (> ~half a pixel).
       One pixel spans ~2 / (scale * zoom * max_dim) radians whatever the
       orientation of the canvas, so derive the threshold from the current
       zoom instead of a constant tuned for max zoom. *)
    let angle_threshold =
      let max_dim =
        max (Gl.drawing_buffer_width ctx) (Gl.drawing_buffer_height ctx)
      in
      1. /. (scale *. !zoom *. float (max 1 max_dim))
    in
    let angle_diff = Quaternion.angle_between orientation prev_orientation in
    let zoom_diff = abs_float (z -. prev_zoom) in
    let should_draw =
      angle_diff > angle_threshold || zoom_diff > 0.00001 || !force_redraw
    in

    if should_draw then (
      force_redraw := false;
      draw ~orientation ctx);

    (* URL Update Logic: Check for stability (using current orientation vs previous frame) *)
    (* Note: We use the actual current orientation for stability check, even if we didn't draw *)
    if angle_diff < 0.0001 && zoom_diff < 0.00001 then (
      (* Camera is effectively stable *)
      let change_since_update_angle =
        Quaternion.angle_between orientation !last_url_update_orientation
      in
      let change_since_update_zoom = abs_float (z -. !last_url_update_zoom) in

      (* Only update if orientation changed significantly (> ~0.1 deg) OR zoom changed, and enough time passed *)
      if
        (change_since_update_angle > 0.002 || change_since_update_zoom > 0.01)
        && now_ms () -. !last_url_update_time > 1000.
      then (
        update_url_params ();
        last_url_update_orientation := orientation;
        last_url_update_zoom := z;
        last_url_update_time := now_ms ()))
    else last_url_update_time := now_ms ();
    (* Reset timer while moving *)
    let* () = request_animation_frame () in

    (* If we drew this frame, update prev state. If not, keep old prev state to accumulate changes *)
    if should_draw then loop orientation z else loop prev_orientation prev_zoom
  in
  last_frame_time := now_ms ();
  loop !current_orientation (!zoom -. 1.)

(* Set by [run_renderer] once there is a scene to replace; switches the
   viewpoint in place. A no-op until then: the startup overlay covers the
   location UI while the first location is loading. *)
let set_orientation alpha beta =
  let alpha_rad = alpha *. pi /. 180. in
  let q_yaw =
    Quaternion.from_axis_angle { x = 0.; y = 0.; z = 1.; w = 0. } alpha_rad
  in
  let pitch_rad = beta *. pi /. 180. in
  let q_pitch =
    Quaternion.from_axis_angle { x = 1.; y = 0.; z = 0.; w = 0. } pitch_rad
  in
  (* Rotate around Z first (yaw), then around X (pitch) to maintain turntable *)
  (current_orientation := Quaternion.(q_yaw * q_pitch));
  target_orientation := !current_orientation

let switch_location :
    (push:bool ->
    camera:(float * float * float) option ->
    lat:float ->
    lon:float ->
    unit)
    ref =
  ref (fun ~push:_ ~camera:_ ~lat:_ ~lon:_ -> ())

(* Bumped by every [load_location]. Lwt cancellation is awkward, so instead a
   load that finds itself no longer at the current epoch when it comes back from
   the network has been superseded by a later switch, and silently drops out:
   the last switch wins. *)
let location_epoch = ref 0

(* POIs of the loaded CLC tiles, restricted to the DEM tile and positioned in
   DEM pixel coordinates. *)
let poi_positions ~w ~h ~lat ~lon ~tile clc_tiles =
  let d = float (w / 2) /. 3600. in
  let tile_coord = { Points.lon = lon -. d; lat = lat -. d } in
  let tile_coord' = { Points.lon = lon +. d; lat = lat +. d } in
  (* Extract POIs from all loaded tiles and convert to Points.t format *)
  let pois =
    List.concat_map
      (fun (tile, _, _) ->
        List.map
          (fun (poi : Clc_loader.poi) ->
            {
              Points.name = poi.name;
              coord = { Points.lat = poi.lat; lon = poi.lon };
              elevation =
                (if poi.elevation = 0 then None else Some poi.elevation);
            })
          tile.Clc_loader.pois)
      clc_tiles
  in
  (* Filter POIs within the visible tile bounds *)
  let filtered =
    List.filter
      (fun { Points.coord = { lat = pt_lat; lon = pt_lon }; _ } ->
        tile_coord.lat < pt_lat && pt_lat < tile_coord'.lat
        && tile_coord.lon < pt_lon && pt_lon < tile_coord'.lon)
      pois
  in
  let points =
    filtered
    |> List.map
         (let size = w in
          let center_lon_int = Web_utils.arcsec_floor lon in
          let center_lat_int = Web_utils.arcsec_floor lat in
          let min_lon_int = center_lon_int - (size / 2) in
          let min_lat_int = center_lat_int - (size / 2) in

          fun ({ Points.coord = { lat = pt_lat; lon = pt_lon }; _ } as pt) ->
            (* Sub-arcsecond grid position: the tiles carry ~0.4 m precision,
               and the high-resolution mesh has sub-texel detail for the
               marker and its silhouette anchor to line up with. Only the
               visibility test rounds to whole texels. *)
            let x =
              Float.max 0.
                (Float.min
                   (float (w - 1))
                   ((pt_lon *. 3600.) -. float min_lon_int))
            in
            let y =
              Float.max 0.
                (Float.min
                   (float (h - 1))
                   ((pt_lat *. 3600.) -. float min_lat_int))
            in
            (pt, (x, y)))
  in
  if false then (
    let r = 4 in
    let a = Array.make (((2 * r) + 1) * ((2 * r) + 1)) 0 in
    let count = ref 0 in
    let dxs = ref 0 in
    let dys = ref 0 in
    List.iter
      (fun (_, (x, y)) ->
        let x = int_of_float (Float.round x)
        and y = int_of_float (Float.round y) in
        if x > r && y > r && x < w - r - 1 && y < h - r - 1 then (
          let get_h = Dem_loader.get_height tile in
          let max_h = ref (get_h y x) in
          let dx = ref 0 in
          let dy = ref 0 in
          for i = -r to r do
            for j = -r to r do
              let y' = y + i in
              let x' = x + j in
              assert (x' >= 0 && y' >= 0 && x' < w && y' < h);
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
              assert (x' >= 0 && y' >= 0 && x' < w && y' < h);
              let h = get_h y' x' in
              if h > !max_h then is_peak := false
            done
          done;
          if !is_peak then (
            let p = (((2 * r) + 1) * (r + !dy)) + r + !dx in
            a.(p) <- a.(p) + 1;
            incr count;
            dxs := !dxs + !dx;
            dys := !dys + !dy)))
      points;
    Format.eprintf "Mean offsets: %f %f (%d)@."
      (float !dxs /. float !count)
      (float !dys /. float !count)
      !count;
    for i = 0 to 2 * r do
      for j = 0 to 2 * r do
        Format.eprintf "%d " a.((((2 * r) + 1) * i) + j)
      done;
      Format.eprintf "@."
    done);
  points

(* Height of the terrain as actually drawn at grid position (gx, gy) -- in
   texels of [tile], which need not be whole ones since the high-resolution
   grid is not aligned with the base one -- seen from [r] metres away:
   replicates the vertex shader's LOD selection and corner-convention bilinear
   fetch over the soft-max height pyramid that [compute_relief] bakes
   (downsample.frag, per-level sharpness k = 0.1 * 0.5^(level-1), +0.5/255
   encode bias). Distant mip levels cannot reach a sharp summit's
   full-resolution height, so a POI anchored there would float above the
   rendered silhouette. [inv_avg_delta] is per texel of [tile], so passing the
   finer grid's value is what shifts the LOD selection with it. *)
let rendered_height tile ~(radial_params : Render_state.radial_params)
    ~inv_avg_delta ~size ~r ~gx ~gy =
  let grid_spacing =
    radial_params.Render_state.grid_k
    *. (r +. radial_params.Render_state.grid_scale)
  in
  let lod_f = Float.max 0. (Float.log2 (grid_spacing *. inv_avg_delta)) in
  let lod = min (int_of_float lod_f) (Web_utils.log2 size) in
  begin
    let get row col =
      Dem_loader.get_height tile
        (max 0 (min (size - 1) row))
        (max 0 (min (size - 1) col))
    in
    (* Value of the level-[l] pyramid cell (cx, cy); the soft-max is
       shift-invariant, so working in metres matches the shader's encoded
       units. *)
    let rec cell l cx cy =
      if l = 0 then get cy cx
      else
        let k = downsample_k l in
        let h00 = cell (l - 1) (2 * cx) (2 * cy)
        and h10 = cell (l - 1) ((2 * cx) + 1) (2 * cy)
        and h01 = cell (l - 1) (2 * cx) ((2 * cy) + 1)
        and h11 = cell (l - 1) ((2 * cx) + 1) ((2 * cy) + 1) in
        let m = Float.max (Float.max h00 h10) (Float.max h01 h11) in
        let w h = exp (k *. (h -. m)) in
        let avg = (w h00 +. w h10 +. w h01 +. w h11) /. 4. in
        m +. (log avg /. k) +. (0.5 /. 255. *. (9500. /. 257.))
    in
    let inv = 1. /. (2. ** float lod) in
    let plx = gx *. inv and ply = gy *. inv in
    let bx = int_of_float (floor plx) and by = int_of_float (floor ply) in
    let fx = plx -. float bx and fy = ply -. float by in
    let n = size lsr lod in
    let c cx cy = cell lod (min cx (n - 1)) (min cy (n - 1)) in
    let h00 = c bx by
    and h10 = c (bx + 1) by
    and h01 = c bx (by + 1)
    and h11 = c (bx + 1) (by + 1) in
    let h0 = h00 +. (fx *. (h10 -. h00)) in
    let h1 = h01 +. (fx *. (h11 -. h01)) in
    h0 +. (fy *. (h1 -. h0))
  end

let split_gpx_segment ~(radial_params : Render_state.radial_params)
    ((x0, y0, _) as start) ((x1, y1, _) as finish) =
  let grid_k = radial_params.Render_state.grid_k in
  let grid_scale = radial_params.Render_state.grid_scale in
  let sector_angle = pi /. 1024. in
  let cross (ax, ay) (bx, by) = (ax *. by) -. (ay *. bx) in
  let radius ring = grid_scale *. (exp (grid_k *. float ring) -. 1.) in
  let barycentric (px, py) (ax, ay) (bx, by) (cx, cy) =
    let denom = ((by -. cy) *. (ax -. cx)) +. ((cx -. bx) *. (ay -. cy)) in
    let u =
      (((by -. cy) *. (px -. cx)) +. ((cx -. bx) *. (py -. cy))) /. denom
    in
    let v =
      (((cy -. ay) *. (px -. cx)) +. ((ax -. cx) *. (py -. cy))) /. denom
    in
    (u, v, 1. -. u -. v)
  in
  let triangle_at (px, py) =
    let r = sqrt ((px *. px) +. (py *. py)) in
    let ring =
      min (n_rings - 2)
        (max 0 (int_of_float (floor (log ((r /. grid_scale) +. 1.) /. grid_k))))
    in
    let angle = atan2 py px in
    let angle0 =
      (pi /. 4.)
      +. (floor ((angle -. (pi /. 4.)) /. sector_angle) *. sector_angle)
    in
    let angle1 = angle0 +. sector_angle in
    let vertex r angle = (cos angle *. r, sin angle *. r) in
    let a0 = vertex (radius ring) angle0
    and b0 = vertex (radius (ring + 1)) angle0
    and a1 = vertex (radius ring) angle1
    and b1 = vertex (radius (ring + 1)) angle1 in
    if ring = 0 then (a1, b0, b1)
    else
      let u, v, w = barycentric (px, py) a0 b0 a1 in
      if Float.min u (Float.min v w) < -0.00001 then (a1, b0, b1)
      else (a0, b0, a1)
  in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let point t = (x0 +. (t *. dx), y0 +. (t *. dy), 0.) in
  let segment_length = sqrt ((dx *. dx) +. (dy *. dy)) in
  let epsilon = 1e-6 in
  let rec walk t acc steps =
    if t >= 1. -. epsilon || steps = 4096 then List.rev (finish :: acc)
    else
      let px, py, _ = point (min 1. (t +. epsilon)) in
      let a, b, c = triangle_at (px, py) in
      let crossing edge_start edge_end =
        let ex, ey =
          (fst edge_end -. fst edge_start, snd edge_end -. snd edge_start)
        in
        let denom = cross (dx, dy) (ex, ey) in
        if abs_float denom < 1e-12 then None
        else
          let qx = fst edge_start -. x0 and qy = snd edge_start -. y0 in
          let t' = cross (qx, qy) (ex, ey) /. denom in
          let u = cross (qx, qy) (dx, dy) /. denom in
          if
            t' > t +. epsilon
            && t' < 1. -. epsilon
            && u >= -.epsilon
            && u <= 1. +. epsilon
          then Some t'
          else None
      in
      let next =
        [ crossing a b; crossing b c; crossing c a ]
        |> List.filter_map Fun.id
        |> List.fold_left Float.min 1.
      in
      if next = 1. then List.rev (finish :: acc)
      else
        let nudge =
          min
            (0.01 /. max 0.01 segment_length)
            (min ((next -. t) *. 0.25) ((1. -. next) *. 0.25))
        in
        let before = next -. nudge and after = next +. nudge in
        walk after (point after :: point before :: acc) (steps + 1)
  in
  walk 0. [ start ] 0

let split_gpx_triangle_boundaries radial_params points =
  match points with
  | [] -> []
  | first :: rest ->
      let rec aux acc previous = function
        | [] -> List.rev acc
        | next :: remaining ->
            let segment = split_gpx_segment ~radial_params previous next in
            aux (List.rev_append (List.tl segment) acc) next remaining
      in
      aux [ first ] first rest

let build_gpx_path ctx ~lat ~lon ~radial_params =
  let gpx_data =
    match !current_gpx_str with
    | Some str -> ( try Some (Gpx_loader.parse str) with _ -> None)
    | None -> None
  in
  match gpx_data with
  | None -> None
  | Some data ->
      let deltax, deltay, _ = Render_state.compute_deltas ~lat in
      let conv = Render_state.meridian_convergence ~lat in
      let max_view_dist = 20000.0 in

      let process_subsegment (pts_list : (float * float * float) list) =
        let vert_count = List.length pts_list in
        if vert_count < 2 then None
        else
          let pts_array = Array.of_list pts_list in
          let verts = Array.make (vert_count * 4 * 10) 0. in
          for i = 0 to vert_count - 1 do
            let px, py, z = pts_array.(i) in
            let p_px, p_py, p_z =
              if i > 0 then pts_array.(i - 1) else (px, py, z)
            in
            let n_px, n_py, n_z =
              if i < vert_count - 1 then pts_array.(i + 1) else (px, py, z)
            in
            let idx = i * 4 * 10 in
            verts.(idx) <- p_px;
            verts.(idx + 1) <- p_py;
            verts.(idx + 2) <- p_z;
            verts.(idx + 3) <- px;
            verts.(idx + 4) <- py;
            verts.(idx + 5) <- z;
            verts.(idx + 6) <- n_px;
            verts.(idx + 7) <- n_py;
            verts.(idx + 8) <- n_z;
            verts.(idx + 9) <- 1.0;

            verts.(idx + 10) <- p_px;
            verts.(idx + 11) <- p_py;
            verts.(idx + 12) <- p_z;
            verts.(idx + 13) <- px;
            verts.(idx + 14) <- py;
            verts.(idx + 15) <- z;
            verts.(idx + 16) <- n_px;
            verts.(idx + 17) <- n_py;
            verts.(idx + 18) <- n_z;
            verts.(idx + 19) <- -1.0;

            verts.(idx + 20) <- p_px;
            verts.(idx + 21) <- p_py;
            verts.(idx + 22) <- p_z;
            verts.(idx + 23) <- px;
            verts.(idx + 24) <- py;
            verts.(idx + 25) <- z;
            verts.(idx + 26) <- n_px;
            verts.(idx + 27) <- n_py;
            verts.(idx + 28) <- n_z;
            verts.(idx + 29) <- 2.0;

            verts.(idx + 30) <- p_px;
            verts.(idx + 31) <- p_py;
            verts.(idx + 32) <- p_z;
            verts.(idx + 33) <- px;
            verts.(idx + 34) <- py;
            verts.(idx + 35) <- z;
            verts.(idx + 36) <- n_px;
            verts.(idx + 37) <- n_py;
            verts.(idx + 38) <- n_z;
            verts.(idx + 39) <- -2.0
          done;
          Some verts
      in

      let segment_verts =
        List.concat_map
          (fun (trk : Gpx_loader.track) ->
            let points_with_pos =
              List.filter_map
                (fun (pt : Gpx_loader.trackpoint) ->
                  let dx = (pt.Gpx_loader.lon -. lon) *. 3600. in
                  let dy = (pt.Gpx_loader.lat -. lat) *. 3600. in
                  let ge = deltax *. dx in
                  let gn = deltay *. dy in
                  let px = ge /. (1. +. (gn *. conv)) in
                  let py = gn +. (px *. px *. conv /. 2.) in
                  let px = ge /. (1. +. (py *. conv)) in
                  let py = gn +. (px *. px *. conv /. 2.) in
                  let r = sqrt ((px *. px) +. (py *. py)) in
                  if r > max_view_dist then None else Some (px, py, 0.))
                trk.Gpx_loader.points
            in
            let points_with_pos =
              split_gpx_triangle_boundaries radial_params points_with_pos
            in
            let rec split_segments current acc = function
              | [] ->
                  if List.length current >= 2 then
                    List.rev (List.rev current :: acc)
                  else List.rev acc
              | pt :: rest -> split_segments (pt :: current) acc rest
            in
            let subsegs = split_segments [] [] points_with_pos in
            List.filter_map process_subsegment subsegs)
          data.Gpx_loader.tracks
      in

      if segment_verts = [] then None
      else
        let num_segs = List.length segment_verts in
        let padded_segs =
          List.mapi
            (fun idx verts ->
              let len = Array.length verts in
              if len < 40 then verts
              else
                let first_vert = Array.sub verts 0 10 in
                let last_vert = Array.sub verts (len - 10) 10 in
                let prefix =
                  if idx > 0 then Array.concat [ first_vert; first_vert ]
                  else [||]
                in
                let suffix =
                  if idx < num_segs - 1 then
                    Array.concat [ last_vert; last_vert ]
                  else [||]
                in
                Array.concat [ prefix; verts; suffix ])
            segment_verts
        in
        let total_verts = Array.concat padded_segs in
        let vert_count = Array.length total_verts / 40 in
        let ba =
          Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
            total_verts
        in
        let vbo =
          Web_utils.create_buffer ctx Gl.array_buffer (Web_utils.Buffer ba)
        in
        let vao = Gl.create_vertex_array ctx in
        Gl.bind_vertex_array ctx (Some vao);
        Gl.bind_buffer ctx Gl.array_buffer (Some vbo);
        let stride = 10 * 4 in
        Gl.enable_vertex_attrib_array ctx 0;
        Gl.vertex_attrib_pointer ctx 0 3 Gl.float false stride 0;
        Gl.enable_vertex_attrib_array ctx 1;
        Gl.vertex_attrib_pointer ctx 1 3 Gl.float false stride 12;
        Gl.enable_vertex_attrib_array ctx 2;
        Gl.vertex_attrib_pointer ctx 2 3 Gl.float false stride 24;
        Gl.enable_vertex_attrib_array ctx 3;
        Gl.vertex_attrib_pointer ctx 3 1 Gl.float false stride 36;
        Gl.bind_vertex_array ctx None;
        Gl.bind_buffer ctx Gl.array_buffer None;
        Some (vao, vbo, vert_count)

let update_gpx_path ctx ~radial_params =
  match !session with
  | None -> ()
  | Some loc ->
      Option.iter
        (fun (vao, vbo, _) ->
          Gl.delete_vertex_array ctx vao;
          Gl.delete_buffer ctx vbo)
        loc.gpx_path;
      let new_gpx_path =
        build_gpx_path ctx ~lat:!current_lat ~lon:!current_lon ~radial_params
      in
      session := Some { loc with gpx_path = new_gpx_path };
      force_redraw := true

(* Fetch the DEM and CLC tiles for [lat]/[lon], rebake every piece of
   location-dependent state and publish it in [session]. Resolves to whether
   this load is the one now on screen.

   The network part comes first and nothing is touched before it has succeeded:
   a load that fails or gets superseded leaves the previous location entirely
   intact and rendering. From the epoch check to the [session] assignment there
   is no await, so no frame observes a half-replaced location. *)
(* How long a location load waits for the near-field elevation before going to
   screen without it. Long enough to cover a hit in the persistent cache and the
   fast rejections of being offline, short enough not to be the wait it exists to
   avoid. *)
let hd_grace_s = 1.5

let load_location ctx ~graphics ~w ~h ~detail_map ~palette_texture ~lat ~lon =
  incr location_epoch;
  let epoch = !location_epoch in
  let {
    terrain_geo;
    indices;
    triangle_geo;
    terrain_pid;
    shadow_pid;
    sky_pid;
    sky_uniforms;
    terrain_uniforms;
    shadow_map;
    shadow_fbo;
    shadow_uniforms;
    normal_pid;
    downsample_pid;
    relief_uniforms;
    downsample_uniforms;
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
    hd_placeholder;
    path_pid;
    path_uniforms;
    _;
  } =
    graphics
  in
  (* The DEM and the CLC tiles (for POIs) load in parallel. The near-field
     high-resolution elevation starts with them but is deliberately not awaited
     alongside them: it is the one source that can take [Hd_dem.timeout_s] to
     answer, and awaiting it holds the whole location -- base tiles that may well
     be in cache included -- behind that ceiling. *)
  let hd_fetch = Hd_dem.fetch Hd_dem.l13 ~lat ~lon in
  let* tile, (_, _, _, _, clc_tiles) =
    Lwt.both
      (Dem_loader.load ~size:w ~lat ~lon)
      (Clc_loader.load_tiles ~lat ~lon ~size:w)
  in
  (* Wait for the near-field grid, but only for [hd_grace_s]. Publishing twice is
     not free -- the eye is anchored on the surface that is drawn, so folding a
     refinement in afterwards lifts the camera by up to a few tens of metres on a
     summit -- so the cache hits and the fast rejections of being offline, which
     is nearly every load, are kept on a single publish. Only a fetch slow enough
     that waiting for it is itself the problem gets published around. *)
  let* () =
    Lwt.choose
      [
        (let* _ = hd_fetch in
         Lwt.return_unit);
        sleep hd_grace_s;
      ]
  in
  if !location_epoch <> epoch then Lwt.return false
  else begin
    let x = w / 2 in
    let y = h / 2 in
    let poi = poi_positions ~w ~h ~lat ~lon ~tile clc_tiles in
    (* The blended near-field grid equals the base upsample wherever the
       high-resolution data is missing, so the renderer can switch to it on a
       plain extent test. *)
    let blend_hd raw = Option.bind raw (Hd_dem.blend ~lat ~base:tile) in
    (* [Hd_dem.fetch] never fails, so [Fail] is unreachable; it is folded into
       "no near-field data" rather than left to raise inside the refinement. *)
    let in_time, refine_later =
      match Lwt.state hd_fetch with
      | Lwt.Return raw -> (blend_hd raw, false)
      | Lwt.Sleep -> (None, true)
      | Lwt.Fail _ -> (None, false)
    in
    (* The previous location's textures are dead from here on: the bakes below
       overwrite every unit they were bound to. Deleting first keeps the peak
       footprint at one location's worth of DEM-sized textures. [session] is
       cleared with them so that [publish] can tell a record it supersedes from
       one already taken down; no [draw] observes the gap, everything from here
       to the publish being synchronous. *)
    Option.iter (delete_location ctx) !session;
    session := None;
    let relief_texture, relief_normal_texture =
      let tile_texture = make_tile_texture ctx tile in
      let pyramids =
        time_gpu ctx "compute_relief" (fun () ->
            compute_relief ctx w h lat triangle_geo tile_texture normal_pid
              downsample_pid relief_uniforms downsample_uniforms)
      in
      (* Nothing reads the source grid on the GPU after the bake: it is not among
         the textures [bind_terrain_textures] binds, and every consumer goes
         through the pyramids. Freeing it here rather than with the location
         keeps 32 MiB (RG8 4096^2) out of the per-location footprint, as the
         high-resolution bake below already does with its own source. *)
      Gl.delete_texture ctx tile_texture;
      pyramids
    in
    let index_count = Bigarray.Array1.dim indices in

    let deltax, deltay, _ = Render_state.compute_deltas ~lat in

    let ao_texture =
      time_gpu ctx "compute_ao" (fun () ->
          compute_ao ctx w h deltay relief_texture nearest_sampler ao_bake_pid
            ao_blur_pid ao_bake_uniforms ao_blur_uniforms)
    in

    let light_dir =
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
      Matrix.{ x = sx; y = sy; z = sz; w = 0. }
    in
    let splits_dist = [| 2000.; 8000.; 25000. |] in

    (* GPU rasterize CLC tiles to FBO *)
    let cover_map_texture =
      time_gpu ctx "rasterize_clc_tiles" (fun () ->
          rasterize_clc_tiles ctx ~lat ~lon ~w ~clc_tiles ~clc_raster_pid
            ~water_raster_pid clc_raster_uniforms water_raster_uniforms)
    in

    (* Everything downstream of the near-field grid, gathered so that the
       location can go to screen without it and be republished once it lands. The
       bakes above -- base relief pyramid, ambient occlusion, land-cover raster
       -- do not depend on it and are deliberately not among them: a refinement
       redoes only the finer relief pyramid, the shadow map that samples it, the
       eye height, and the POI replicas that are anchored on both. *)
    let publish ~hd_grid =
      (* Bilinear interpolation for height *)
      let height =
        let off_x = Render_state.compute_sub_arcsec_offset lon in
        let off_y = Render_state.compute_sub_arcsec_offset lat in
        let base_height =
          let get_h = Dem_loader.get_height tile in
          let h00 = get_h y x in
          let h10 = get_h y (x + 1) in
          let h01 = get_h (y + 1) x in
          let h11 = get_h (y + 1) (x + 1) in
          let h0 = h00 +. (off_x *. (h10 -. h00)) in
          let h1 = h01 +. (off_x *. (h11 -. h01)) in
          h0 +. (off_y *. (h1 -. h0))
        in
        (* Inside the high-resolution extent the mesh is drawn from the finer
           grid, and it resolves summits the 30 m base smooths away: an eye
           placed 2 m above the base surface would end up *inside* the terrain
           it is looking out of. Read the eye height off the grid that is
           drawn. *)
        match hd_grid with
        | Some (g : Hd_dem.t) ->
            let gx = (off_x -. g.origin_x) /. g.layer.px_arcsec in
            let gy = (off_y -. g.origin_y) /. g.layer.px_arcsec in
            if
              gx >= 0.
              && gx <= float (g.layer.size - 2)
              && gy >= 0.
              && gy <= float (g.layer.size - 2)
            then begin
              let get_h = Dem_loader.get_height g.grid in
              let bx = int_of_float (floor gx)
              and by = int_of_float (floor gy) in
              let fx = gx -. float bx and fy = gy -. float by in
              let h00 = get_h by bx in
              let h10 = get_h by (bx + 1) in
              let h01 = get_h (by + 1) bx in
              let h11 = get_h (by + 1) (bx + 1) in
              let h0 = h00 +. (fx *. (h10 -. h00)) in
              let h1 = h01 +. (fx *. (h11 -. h01)) in
              h0 +. (fy *. (h1 -. h0))
            end
            else base_height
        | None -> base_height
      in
      (* Second, finer relief pyramid over the near-field square. *)
      let hd =
        Option.map
          (fun (g : Hd_dem.t) ->
            let tex = make_tile_texture ctx g.grid in
            let hd_relief_texture, hd_relief_normal_texture =
              time_gpu ctx "compute_relief_hd" (fun () ->
                  compute_relief ~spacing_scale:g.layer.px_arcsec ~border:false
                    ctx g.layer.size g.layer.size lat triangle_geo tex
                    normal_pid downsample_pid relief_uniforms
                    downsample_uniforms)
            in
            (* Nothing reads the source grid on the GPU after the bake. *)
            Gl.delete_texture ctx tex;
            { hd_relief_texture; hd_relief_normal_texture })
          hd_grid
      in
      let hd_relief_texture =
        match hd with Some h -> h.hd_relief_texture | None -> hd_placeholder
      in
      let hd_relief_normal_texture =
        match hd with
        | Some h -> h.hd_relief_normal_texture
        | None -> hd_placeholder
      in
      let world_center =
        let center_offset_x, center_offset_y =
          Render_state.compute_center_offset ~lat ~lon ~x ~y
        in
        Matrix.
          { x = center_offset_x; y = center_offset_y; z = height +. 2.; w = 1. }
      in
      let shadow_matrices =
        (* view_proj is ignored in calculate_shadow_matrices *)
        calculate_shadow_matrices ~light_dir ~world_center
      in

      (* Upload all location-static uniforms *)
      Render_state.upload_session_static ctx terrain_pid sky_pid shadow_pid
        terrain_uniforms sky_uniforms shadow_uniforms ~w ~lat ~x ~y ~lon
        ~light_dir ~shadow_matrices ~shadow_splits:splits_dist
        ~fog_color:fog_linear ~zenith_color:zenith_linear;
      let hd_params =
        Option.map
          (fun (g : Hd_dem.t) ->
            {
              Render_state.hd_size = g.layer.size;
              hd_px_arcsec = g.layer.px_arcsec;
              hd_origin = (g.origin_x, g.origin_y);
            })
          hd_grid
      in
      Render_state.upload_hd_params ctx terrain_pid shadow_pid terrain_uniforms
        shadow_uniforms hd_params;
      Gl.use_program ctx path_pid;
      Render_state.upload_path_session ctx path_uniforms ~w ~lat ~x ~y ~lon;
      Render_state.upload_path_hd_params ctx path_uniforms hd_params;

      (* Render shadows into the session-wide shadow map (fixed 2048x2048x3) *)
      time_gpu ctx "draw_shadows" (fun () ->
          draw_shadows ~shadow_pid ~shadow_fbo ~shadow_map shadow_uniforms
            ~matrices:shadow_matrices ~terrain_geo ~index_count ~radial_params
            ~relief_texture ~hd_relief_texture ctx);

      (* Bind all terrain textures - after all textures are created *)
      bind_terrain_textures ctx ~relief_texture ~relief_normal_texture
        ~ao_texture ~detail_map ~shadow_map ~cover_map_texture ~palette_texture
        ~hd_relief_texture ~hd_relief_normal_texture;

      (* Location-static state that was needlessly re-set every frame. Must come
         after the bake passes above, which set their own clear colours. *)
      Gl.use_program ctx terrain_pid;
      Gl.uniform1f ctx terrain_uniforms.center_height (height +. 2.);
      (let r, g, b = fog_linear in
       Gl.clear_color ctx r g b 1.);

      (* Sight lines must be traced over the surface that is actually drawn: the
         eye is anchored on the high-resolution grid now, and it sits up to 60 m
         from the base DEM on a summit the 30 m grid smooths away, so a ray
         traced over the base terrain would clear ridges the viewer cannot see
         over -- or be blocked by ground that is no longer there.

         [hd_at] reads that grid at a fractional *base*-grid position.
         [ray_height] hands the whole test the same surface at base resolution
         (both endpoints, the curvature frame and the Bresenham phase), while
         [~fine] lets the near phase, where the ray hugs the terrain, sample it
         at its own 0.31 arcsec. Without HD both collapse to exactly the
         previous call. *)
      let hd_at =
        match hd_grid with
        | None -> fun _ _ -> None
        | Some (g : Hd_dem.t) ->
            let get_h = Dem_loader.get_height g.grid in
            let limit = float (g.layer.size - 2) in
            fun bx by ->
              let hx = (bx -. float x -. g.origin_x) /. g.layer.px_arcsec in
              let hy = (by -. float y -. g.origin_y) /. g.layer.px_arcsec in
              if hx >= 0. && hx <= limit && hy >= 0. && hy <= limit then
                Some (Visibility.bilinear_height get_h ~x:hx ~y:hy)
              else None
      in
      let ray_height =
        let base = Dem_loader.get_height tile in
        match hd_grid with
        | None -> base
        | Some _ -> (
            fun row col ->
              match hd_at (float col) (float row) with
              | Some h -> h
              | None -> base row col)
      in
      let fine = Option.map (fun _ -> hd_at) hd_grid in
      let points =
        let off_x = Render_state.compute_sub_arcsec_offset lon in
        let off_y = Render_state.compute_sub_arcsec_offset lat in
        List.filter
          (fun (_, (gx, gy)) ->
            let dx = (gx -. float x) *. deltax in
            let dy = (gy -. float y) *. deltay in
            let dist_sq = (dx *. dx) +. (dy *. dy) in
            if dist_sq > 4900000000. then false
            else
              (* The test walks whole texels; its summit exemption (10 texels)
                 dwarfs the half-texel rounding of the destination. *)
              let dst_x = int_of_float (Float.round gx) in
              let dst_y = int_of_float (Float.round gy) in
              Visibility.test_precise ray_height ~src_h:(height +. 2.)
                ~curvature:(deltax, deltay) ?fine ~off_x ~off_y ~src_x:x
                ~src_y:y ~dst_x ~dst_y ())
          poi
      in
      let points =
        let off_x = Render_state.compute_sub_arcsec_offset lon in
        let off_y = Render_state.compute_sub_arcsec_offset lat in
        let conv = Render_state.meridian_convergence ~lat in
        let inv_avg_delta = 2. /. (deltax +. deltay) in
        List.map
          (fun ({ Points.name; elevation; _ }, (x', y')) ->
            let texture =
              prepare_text ctx
                (match elevation with
                | None -> name
                | Some elevation -> Printf.sprintf "%s (%dm)" name elevation)
            in
            (* Plane position: inverse of the grid mapping in
               radial_common.vert (meridian convergence to second order); two
               fixed-point iterations converge to millimetres. *)
            let ge = deltax *. (x' -. float x -. off_x) in
            let gn = deltay *. (y' -. float y -. off_y) in
            let px = ge /. (1. +. (gn *. conv)) in
            let py = gn +. (px *. px *. conv /. 2.) in
            let px = ge /. (1. +. (py *. conv)) in
            let py = gn +. (px *. px *. conv /. 2.) in
            let r2 = (px *. px) +. (py *. py) in
            (* Anchor at the silhouette as drawn (mip-level height at this
               distance), with the same curvature drop as the mesh. Inside the
               high-resolution square the mesh reads the finer grid, so the
               replica must too: same walker, over the blended grid, with the
               LOD selection shifted by its refinement factor. *)
            let z =
              let r = sqrt r2 in
              let hd_pos (l : Hd_dem.layer) gc o =
                (gc -. float (w / 2) -. o) /. l.px_arcsec
              in
              match hd_grid with
              | Some (g : Hd_dem.t)
                when let hd_x = hd_pos g.layer x' g.origin_x
                     and hd_y = hd_pos g.layer y' g.origin_y in
                     hd_x >= 0.
                     && hd_x <= float (g.layer.size - 1)
                     && hd_y >= 0.
                     && hd_y <= float (g.layer.size - 1) ->
                  rendered_height g.grid ~radial_params
                    ~inv_avg_delta:(inv_avg_delta /. g.layer.px_arcsec)
                    ~size:g.layer.size ~r
                    ~gx:(hd_pos g.layer x' g.origin_x)
                    ~gy:(hd_pos g.layer y' g.origin_y)
              | _ ->
                  rendered_height tile ~radial_params ~inv_avg_delta ~size:w ~r
                    ~gx:x' ~gy:y'
            in
            let z = z -. Visibility.curvature_drop r2 in
            let h =
              let dz = z -. height in
              (* Apparent elevation angle, for label priority *)
              dz /. sqrt (r2 +. (dz *. dz))
            in
            ((texture, (px, py, z)), h))
          points
      in
      let points =
        points
        |> List.sort (fun (_, h) (_, h') : int -> Stdlib.compare h' h)
        |> List.map fst
      in
      let gpx_path = build_gpx_path ctx ~lat ~lon ~radial_params in
      (* A refinement supersedes the first publish of this same location, and
         that record shares every base texture with this one: [delete_location]
         would take those down too. Only what it owns alone goes -- the POI
         labels [draw_text] has materialised since, and its own near-field
         relief, which is [None] on the path that is actually taken. *)
      Option.iter
        (fun (prev : location) ->
          List.iter
            (fun ({ texture; _ }, _) ->
              match texture with
              | Some (tid, _) -> Gl.delete_texture ctx tid
              | None -> ())
            prev.points;
          Option.iter
            (fun { hd_relief_texture; hd_relief_normal_texture } ->
              Gl.delete_texture ctx hd_relief_texture;
              Gl.delete_texture ctx hd_relief_normal_texture)
            prev.hd;
          Option.iter
            (fun (vao, vbo, _) ->
              Gl.delete_vertex_array ctx vao;
              Gl.delete_buffer ctx vbo)
            prev.gpx_path)
        !session;
      (* Publish the new location: from the epoch check that guards this call to
         here there is no await, so the swap is atomic as far as [draw] is
         concerned. *)
      session :=
        Some
          {
            height;
            points;
            relief_texture;
            relief_normal_texture;
            ao_texture;
            cover_map_texture;
            hd;
            gpx_path;
          }
    in
    publish ~hd_grid:in_time;
    if refine_later then
      Lwt.async (fun () ->
          let* raw = hd_fetch in
          if !location_epoch <> epoch then Lwt.return_unit
          else
            match blend_hd raw with
            | None -> Lwt.return_unit
            | Some _ as hd_grid ->
                (* [Hd_dem.blend] is synchronous, so the epoch test above still
                   holds here: no [draw] has run since. *)
                publish ~hd_grid;
                let* () = Web_utils.on_gpu_finished ctx in
                force_redraw := true;
                Lwt.return_unit);
    let* () = Web_utils.on_gpu_finished ctx in
    force_redraw := true;
    (* A switch started while the GPU was draining owns the screen now. *)
    Lwt.return (!location_epoch = epoch)
  end

(* One-time renderer setup: session-wide resources, the first location load,
   then the render loop, which never returns. *)
let run_renderer ~w ~h ~lat ~lon canvas ctx ~detail_map ~graphics ~start =
  let {
    terrain_geo;
    indices;
    text_geo;
    terrain_pid;
    triangle_pid;
    text_pid;
    sky_pid;
    sky_uniforms;
    terrain_uniforms;
    triangle_uniforms;
    text_uniforms;
    radial_params;
    path_pid;
    path_uniforms;
    _;
  } =
    graphics
  in
  let index_count = Bigarray.Array1.dim indices in
  (* The CLC palette is the fixed material table: location-independent. *)
  let palette_texture = make_palette_texture ctx in
  let load ~lat ~lon =
    load_location ctx ~graphics ~w ~h ~detail_map ~palette_texture ~lat ~lon
  in
  let* _applied = load ~lat ~lon in
  start ();
  hide_startup_overlay ();

  (* From now on, locations are switched in place: the camera orientation, the
     zoom and every session-wide GPU resource are deliberately kept, so that
     heading continuity across a switch is preserved. *)
  (switch_location :=
     fun ~push ~camera ~lat ~lon ->
       Lwt.async (fun () ->
           Lwt.catch
             (fun () ->
               let* () = show_startup_overlay "Loading Terrain..." true in
               let* applied = load ~lat ~lon in
               if applied then begin
                 current_lat := lat;
                 current_lon := lon;
                 (* A camera preset (featured locations) is applied only once
                    its location owns the screen. *)
                 (match camera with
                 | Some (alpha, beta, z) ->
                     set_orientation alpha beta;
                     zoom := clamp_zoom z
                 | None -> ());
                 (* Reload and sharing must land on the new location. *)
                 update_url_params ~push ();
                 hide_startup_overlay ()
               end;
               Lwt.return_unit)
             (fun e ->
               (match e with Jv.Error e -> Brr.Console.error [ e ] | _ -> ());
               hide_startup_overlay ();
               display_temporary_message
                 (Printf.sprintf "Could not load location: %s"
                    (Printexc.to_string e));
               Lwt.return_unit)));

  Jv.set Jv.global "setGpxContent"
    (Jv.callback ~arity:1 (fun arg ->
         let str =
           if Jv.is_null arg || Jv.is_undefined arg then None
           else
             let s = Jv.to_string arg in
             if String.length s = 0 then None else Some s
         in
         current_gpx_str := str;
         update_gpx_path ctx ~radial_params));

  (* Installed alongside [setGpxContent], once a location is loaded and the
     camera means something: the share button reads the view from here. *)
  Jv.set Jv.global "currentViewUrl"
    (Jv.callback ~arity:1 (fun _ ->
         Jv.of_jstr (Brr.Uri.to_jstr (current_view_uri ()))));

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
      (* [None] only before the first location is baked, and the loop starts
         after it. *)
      match !session with
      | None -> ()
      | Some location ->
          draw terrain_pid terrain_geo triangle_pid text_pid text_geo path_pid
            ~terrain_uniforms ~triangle_uniforms ~text_uniforms ~path_uniforms
            ~proj_ba ~transform_ba ~inv_view_ba ~proj_ta ~transform_ta
            ~inv_view_ta ~location ~orientation ~index_count ~sky_pid
            ~sky_uniforms ~radial_params canvas ctx)
(* Location & UI *)

let featured_locations =
  [
    ("Col Girardin", 44.6078064, 6.8210935, 220., 90., 1.0);
    ("Col du Blainon", 44.209067, 6.9423065, -150., 96., 1.0);
    ("Lacs de Morgon", 44.336516, 6.913906, 64., 88., 1.0);
    ("Roc Diolon (Orcières)", 44.73339, 6.36308, -130., 80., 0.5);
    ("Col Fromage", 44.6896583, 6.8061028, 180., 90., 1.0);
    ("La Mortice Sud", 44.57386, 6.76926, 47., 90., 1.0);
    ("Pic de Morgon (Lac de Serre-Ponçon)", 44.4920, 6.3975, 63., 82., 1.0);
    ("Lac de Roburent", 44.424680, 6.93430, 220., 90., 1.0);
    ("Mont Ténibre", 44.28342, 6.97172, 178., 75., 0.5);
    ("Baisse de Druos", 44.191930, 7.19195, 126., 83., 1.0);
    ("Col de la Forclaz (Lac d'Annecy)", 45.80875, 6.24426, 57., 80., 0.50);
    ("Plateau d'Emparis", 45.0499, 6.2278, -149., 94., 0.6);
    ("La Chalannette (Jausiers)", 44.3950846, 6.7669714, 50., 90., 1.0);
    ("Belvédère des Saisies (Mont Blanc)", 45.77006, 6.54734, -76., 89., 0.50);
    ("Aiguille du Brévent (Mont Blanc)", 45.9334, 6.83722, -169., 95., 1.0);
    ("Le Maïdo (Cirque de Mafate)", -21.05886, 55.38188, -69., 78., 0.50);
    ("Cap Noir", -20.9929, 55.38971, -133., 85., 0.50);
    ("Fenêtre des Makes", -21.1852, 55.43295, -28., 82., 0.50);
    ("Piton des Neiges", -21.09934, 55.48, 148., 73., 0.50);
  ]

let get_preset_position () =
  match featured_locations with
  | (_, lat, lon, alpha, beta, zoom) :: _ ->
      (lat, lon, alpha, beta, clamp_zoom zoom)
  | [] -> (44.3950846, 6.7669714, 170., 90., 1.0)

(* Rejects "nan" and overflowing literals such as "1e999": a non-finite
   coordinate, angle or zoom would propagate through the whole render state and
   permanently defeat the change detection in [should_draw]. *)
let parse_float_safe s =
  match float_of_string_opt s with
  | Some f when Float.is_finite f -> Some f
  | _ -> None

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
                        let end_idx =
                          let idx_z = String.index_opt rest 'z' in
                          let idx_slash = String.index_opt rest '/' in
                          match (idx_z, idx_slash) with
                          | Some z, Some s -> min z s
                          | Some z, None -> z
                          | None, Some s -> s
                          | None, None -> String.length rest
                        in
                        String.sub rest 0 end_idx
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
        (* A coordinate token must contain a digit: this drops empty tokens
           and lone separators, so "44.13 , 5.21" parses (the comma between
           two whitespace runs splits into a token of its own). *)
        Jv.to_list Jv.to_string jv_parts
        |> List.filter (String.exists (fun c -> c >= '0' && c <= '9'))
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
          (* Leading comma, symmetrically: "44.13 ,5.21" tokenizes the
             longitude as ",5.21" *)
          let s =
            if String.length s > 0 && s.[0] = ',' then
              String.sub s 1 (String.length s - 1)
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

(* Bounds of the available DEM tiles: latitudes (min_lat, max_lat], longitudes
   [min_lon, max_lon). Sea-only cells inside the box (the Mediterranean south
   of Marseille) have no tiles; [Dem_loader.load] renders them at sea level. *)
let in_range ~size ~lat ~lon =
  Dem_loader.in_range ~size ~min_lat:42 ~max_lat:47 ~min_lon:4 ~max_lon:10 ~lat
    ~lon
  || Dem_loader.in_range ~size ~min_lat:(-22) ~max_lat:(-20) ~min_lon:54
       ~max_lon:57 ~lat ~lon

(* Rectangles the map picker confines itself to: what the renderer can load,
   which is the boxes above shrunk by the half extent [in_range] requires around
   a position (0.57 degrees at size = 4096). Deliberately not the basemap's own
   extent.

   An earlier version did clip to the basemap, measured by probing it on a grid,
   and that was a mistake twice over. The extent is roughly France-shaped, so no
   rectangle describes it: the grid had to be read conservatively, and doing that
   at a quarter of a degree cut off the whole Marseille-to-Toulon coast -- the
   Calanques included -- because the row below it sampled open sea. Worse, the
   extent shrinks as the zoom deepens, a nested set from about everything at
   level 10 down to little past the border at 18: longitude 7.4 at latitude 44.6
   has tiles at level 13 and none at 14, and 7.0 lasts until 18. The picker spans
   levels 9 to 18, so a rectangle fitted to any one of them is wrong at the rest,
   and the one fitted at level 12 was both too small in the south and too large
   in the east.

   Where the basemap has nothing the tiles simply do not appear, which is already
   handled, so nothing is lost by letting the map reach there; and the elevation
   is global, so the terrain does load in Switzerland and Italy even though the
   land cover and the near-field refinement stop at the border. The dimming
   therefore marks where a location can be loaded, which is the only limit the
   user cannot work around, and it needs no measuring to stay true.

   La Reunion is still narrowed, to the island rather than the 3 by 2 degrees of
   ocean the elevation box covers, for a different reason: not coverage but that
   opening on a hundred kilometres of empty sea helps nobody.

   The view points are where each region opens, the Ecrins and the Piton des
   Neiges: a rectangle bounding an irregular coast has much of its area at sea,
   so its middle is not necessarily land. *)
let map_regions =
  [
    {
      (* The elevation box inset, with a couple of arcseconds to spare so a
         boundary position cannot fall foul of [in_range]'s flooring. *)
      Map_picker.name = "Alps";
      min_lat = 42.58;
      max_lat = 46.42;
      min_lon = 4.58;
      max_lon = 9.42;
      view_lat = 44.85;
      view_lon = 6.35;
    };
    {
      Map_picker.name = "La R\u{00E9}union";
      min_lat = -21.43;
      max_lat = -20.80;
      min_lon = 55.12;
      max_lon = 55.94;
      view_lat = -21.09;
      view_lon = 55.48;
    };
  ]

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
      if in_range ~size ~lat ~lon then
        let alpha = Option.value (get_float "alpha") ~default:0. in
        let beta = Option.value (get_float "beta") ~default:90. in
        let z = Option.value (get_float "zoom") ~default:1.0 in
        Some (lat, lon, alpha, beta, clamp_zoom z)
      else None
  | _ -> None

let setup_popstate_listener ~size =
  let popstate = Brr.Ev.Type.create (Jstr.v "popstate") in
  ignore
    (Brr.Ev.listen popstate
       (fun _ev ->
         match get_url_position ~size with
         | Some (lat, lon, alpha, beta, z) ->
             !switch_location ~push:false
               ~camera:(Some (alpha, beta, z))
               ~lat ~lon
         | None -> ())
       (Brr.Window.as_target Brr.G.window))

(* Approximate distance in metres from a position to the covered area, 0
   inside. Uses the raw dataset boxes (the per-tile extent handled by
   [in_range] shrinks them by ~0.57 degrees, well under the margins used here)
   and an equirectangular approximation, ample for a tens-of-kilometres
   decision. *)
let distance_to_coverage ~lat ~lon =
  let box_dist (min_lat, max_lat, min_lon, max_lon) =
    let clat = Float.max min_lat (Float.min max_lat lat) in
    let clon = Float.max min_lon (Float.min max_lon lon) in
    let dlat = (lat -. clat) *. 111320. in
    let dlon = (lon -. clon) *. 111320. *. cos (lat *. pi /. 180.) in
    sqrt ((dlat *. dlat) +. (dlon *. dlon))
  in
  Float.min (box_dist (42., 47., 4., 10.)) (box_dist (-22., -20., 54., 57.))

let get_current_position ~size =
  let open Fut.Syntax in
  let open Brr_io.Geolocation in
  let geo = of_navigator Brr.G.navigator in
  let use pos =
    let lat = Pos.latitude pos in
    let lon = Pos.longitude pos in
    if in_range ~size ~lat ~lon then Some (lat, lon, 0., 90., 1.0) else None
  in
  (* Coarse probe first: a cached or network-derived fix arrives nearly
     instantly and lets a user far from the covered area fall back to the
     preset without sitting out a GPS acquisition. Coarse fixes can be off by
     tens of kilometres (IP geolocation), so this phase only ever decides
     negatively, and only when the fix lies outside coverage by more than its
     own reported accuracy plus a margin. *)
  let* coarse =
    get
      ~opts:
        (opts ~high_accuracy:false ~maximum_age_ms:600_000 ~timeout_ms:4_000 ())
      geo
  in
  let clearly_outside =
    match coarse with
    | Ok pos ->
        distance_to_coverage ~lat:(Pos.latitude pos) ~lon:(Pos.longitude pos)
        > Pos.accuracy pos +. 10_000.
    | Error _ -> false
  in
  if clearly_outside then Fut.return None
  else
    let+ precise =
      get
        ~opts:
          (opts ~high_accuracy:true ~maximum_age_ms:30_000 ~timeout_ms:15_000 ())
        geo
    in
    match precise with
    | Ok pos -> use pos
    | Error _ -> (
        (* No precise fix within the timeout: a tight coarse fix (wifi-grade,
           not an IP guess) inside the covered area still beats the preset. *)
        match coarse with
        | Ok pos when Pos.accuracy pos < 2_000. -> use pos
        | Ok _ | Error _ -> None)

(* [Fut] has no race combinator: both the future and the timer settle the same
   future, and whichever gets there first wins. *)
let with_timeout ~ms fut =
  let result, set = Fut.create () in
  let settled = ref false in
  let settle v =
    if not !settled then begin
      settled := true;
      set v
    end
  in
  Fut.await fut (fun v -> settle (Some v));
  ignore (Brr.G.set_timeout ~ms (fun () -> settle None));
  result

(* A shared GPX trace arrives in a cache the document drains, not in the URL
   (see [drainSharedFiles] in index.html), so where to start cannot be read
   synchronously. Waiting for it here is what spares a share two location
   loads -- the current position, then the trace -- which is what flying to the
   trace after startup cost.

   The page sets both halves of the contract before this script runs, which
   [defer] on the viewer guarantees: [sharedTracePending] tells us a share is
   being drained at all, so an ordinary load waits for nothing, and
   [sharedTraceStart] is a promise for the trace's first point. Setting
   [sharedTraceConsumed] tells the page not to fly there as well. The wait is
   capped: a Cache API that never answers must not keep the app on its loading
   screen for ever, and giving up just means falling back to the flight. *)
let shared_trace_timeout_ms = 3000

let get_shared_trace_position ~size =
  let pending = Jv.get Jv.global "sharedTracePending" in
  let promise = Jv.get Jv.global "sharedTraceStart" in
  if Jv.is_none pending || (not (Jv.to_bool pending)) || Jv.is_none promise then
    Fut.return None
  else
    let open Fut.Syntax in
    let+ start =
      with_timeout ~ms:shared_trace_timeout_ms
        (Fut.of_promise ~ok:Fun.id promise)
    in
    match start with
    | Some (Ok start) when not (Jv.is_none start) -> (
        let number k =
          let v = Jv.get start k in
          if Jv.is_none v then None
          else
            let x = Jv.to_float v in
            if Float.is_finite x then Some x else None
        in
        match (number "lat", number "lon") with
        | Some lat, Some lon when in_range ~size ~lat ~lon ->
            (* Face along the trace, as the flight would have turned the camera:
               alpha runs counter-clockwise from north, a bearing clockwise. *)
            let alpha =
              match number "bearing" with Some b -> -.b | None -> 0.
            in
            Jv.set Jv.global "sharedTraceConsumed" Jv.true';
            Some (lat, lon, alpha, 90., clamp_zoom 1.0)
        | _ -> None)
    | Some (Ok _) | Some (Error _) | None -> None

(* Whether offering the device position is worth it, for the one case that has
   not already found out: a startup position taken from the URL never consults
   the device, so nothing has established whether it could have answered.

   Decided without ever prompting. A refused permission settles it on its own,
   and a granted one can be read from cache for the price of a coarse fix. A
   permission still unasked is left alone -- resolving it would mean putting the
   browser's prompt up, and whether to draw a button is no reason to. Absent a
   Permissions API, likewise: leave it. Being wrong here costs at worst an entry
   that reports "out of range or unavailable" when used. *)
let geolocation_offerable () =
  let open Fut.Syntax in
  let permissions = Jv.get (Jv.get Jv.global "navigator") "permissions" in
  if Jv.is_none permissions then Fut.return true
  else
    let query =
      Jv.call permissions "query"
        [| Jv.obj [| ("name", Jv.of_string "geolocation") |] |]
    in
    let* status = Fut.of_promise ~ok:(fun v -> v) query in
    match status with
    | Error _ -> Fut.return true
    | Ok status -> (
        match Jv.to_string (Jv.get status "state") with
        | "denied" -> Fut.return false
        | "granted" -> (
            let open Brr_io.Geolocation in
            let+ coarse =
              get
                ~opts:
                  (opts ~high_accuracy:false ~maximum_age_ms:600_000
                     ~timeout_ms:4_000 ())
                (of_navigator Brr.G.navigator)
            in
            (* Same test as [get_current_position]: only a fix outside coverage
               by more than its own accuracy plus a margin is conclusive, coarse
               fixes being an IP guess often enough. *)
            match coarse with
            | Ok pos ->
                distance_to_coverage ~lat:(Pos.latitude pos)
                  ~lon:(Pos.longitude pos)
                <= Pos.accuracy pos +. 10_000.
            | Error _ -> true)
        | _ -> Fut.return true)

type location_source = Url | Geolocation | Preset

let get_position ~size =
  let open Fut.Syntax in
  (* A shared trace outranks the rest: it is why the app was opened. It reports
     itself as [Url] because it is just as explicit a request for a location,
     and wants the same handling -- no location UI, no redirect to a resolved
     position. *)
  let* shared = get_shared_trace_position ~size in
  let explicit =
    match shared with Some _ -> shared | None -> get_url_position ~size
  in
  match explicit with
  | Some loc -> Fut.return (Ok (Url, loc))
  | None -> (
      (* Only this branch queries the device: an explicit position must never
         make the overlay claim it is locating the user. *)
      update_startup_status "Getting current location..." false;
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
  let eye_on_svg =
    {|<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z"></path><circle cx="12" cy="12" r="3"></circle></svg>|}
  in
  let eye_off_svg =
    {|<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M17.94 17.94A10.07 10.07 0 0 1 12 19c-7 0-10-7-10-7a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 5c7 0 10 7 10 7a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24"></path><line x1="1" y1="1" x2="23" y2="23"></line></svg>|}
  in
  let fab_labels =
    let el = Brr.El.button ~at:Brr.At.[ class' (Jstr.v "fab-labels") ] [] in
    Jv.set (Brr.El.to_jv el) "innerHTML"
      (Jv.of_string (if !show_labels then eye_on_svg else eye_off_svg));
    Brr.El.set_at (Jstr.v "title") (Some (Jstr.v "Toggle Labels")) el;
    el
  in
  (* The GPX and share buttons live in index.html rather than being built here,
     but they sit in the same row and have to turn with it: without this they
     stayed put while the other two moved. This script is deferred, so they are
     in the document by now. *)
  let html_fab id = Brr.Document.find_el_by_id Brr.G.document (Jstr.v id) in
  fab_els :=
    [ fab; fab_labels ] @ List.filter_map html_fab [ "fab-gpx"; "fab-share" ];

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
  Brr.El.append_children body [ fab; fab_labels; overlay ];

  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun _ ->
         show_labels := not !show_labels;
         Brr.El.set_class (Jstr.v "off") (not !show_labels) fab_labels;
         Jv.set (Brr.El.to_jv fab_labels) "innerHTML"
           (Jv.of_string (if !show_labels then eye_on_svg else eye_off_svg));
         force_redraw := true)
       (Brr.El.as_target fab_labels));

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

  let close_menu () = Brr.El.set_class (Jstr.v "visible") false overlay in
  let toggle_menu () =
    let visible = Jstr.v "visible" in
    if Brr.El.class' visible overlay then Brr.El.set_class visible false overlay
    else begin
      Brr.El.set_class visible true overlay;
      Brr.El.set_has_focus true input;
      (* Select the previous contents so typing replaces them directly *)
      ignore (Jv.call (Brr.El.to_jv input) "select" [||])
    end
  in

  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun _ -> toggle_menu ())
       (Brr.El.as_target fab));
  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun e ->
         if Brr.Ev.target e == Brr.El.as_target overlay then toggle_menu ())
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

  (* Errors go in a line of their own rather than into the input's value: the
     typed text stays (selected, so it can be replaced in one keystroke) and
     nothing has to be deleted before retrying. *)
  let input_error = Brr.El.p ~at:Brr.At.[ class' (Jstr.v "input-error") ] [] in
  let clear_input_error () =
    Brr.El.set_class (Jstr.v "visible") false input_error
  in
  let show_input_error msg =
    Brr.El.set_children input_error [ Brr.El.txt (Jstr.v msg) ];
    Brr.El.set_class (Jstr.v "visible") true input_error;
    Brr.El.set_has_focus true input;
    ignore (Jv.call (Brr.El.to_jv input) "select" [||])
  in
  ignore
    (Brr.Ev.listen Brr.Ev.input
       (fun _ -> clear_input_error ())
       (Brr.El.as_target input));

  let go () =
    let text = Jstr.to_string (Brr.El.prop Brr.El.Prop.value input) in
    match parse_input_coordinates text with
    | Some (lat, lon) ->
        if in_range ~size ~lat ~lon then begin
          clear_input_error ();
          close_menu ();
          !switch_location ~push:true ~camera:None ~lat ~lon
        end
        else show_input_error "Location out of range"
    | None -> show_input_error "Invalid coordinates"
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
           | Some (lat, lon, _, _, _) ->
               close_menu ();
               !switch_location ~push:true ~camera:None ~lat ~lon;
               Fut.return ()
           | None ->
               show_input_error "Location out of range or unavailable";
               Fut.return ()
         in
         ())
       (Brr.El.as_target current_loc_btn));

  (* Pick on a map *)
  let open_map =
    Map_picker.create ~regions:map_regions
      ~in_range:(fun ~lat ~lon -> in_range ~size ~lat ~lon)
      ~traces:(fun () ->
        (* The same string the terrain draws from, so the map shows exactly the
           traces that are active. Parsed again rather than shared: it happens
           once per opening, and the parse is cheap next to fetching a screen of
           tiles. *)
        match !current_gpx_str with
        | None -> []
        | Some str -> (
            match Gpx_loader.parse str with
            | { Gpx_loader.tracks; _ } ->
                List.filter_map
                  (fun (t : Gpx_loader.track) ->
                    match t.points with
                    | [] | [ _ ] -> None
                    | points ->
                        Some
                          (Array.of_list
                             (List.map
                                (fun (p : Gpx_loader.trackpoint) ->
                                  (p.lat, p.lon))
                                points)))
                  tracks
            | exception _ -> []))
      ~landmarks:
        (List.map (fun (_, lat, lon, _, _, _) -> (lat, lon)) featured_locations)
      ~on_select:(fun ~lat ~lon ->
        (* In place, like the coordinate input: no navigation, so fullscreen
           mode survives the switch. The camera is left alone, the map saying
           nothing about which way to look. *)
        close_menu ();
        !switch_location ~push:true ~camera:None ~lat ~lon)
  in
  let map_btn =
    Brr.El.div
      ~at:Brr.At.[ class' (Jstr.v "location-item"); tabindex 0 ]
      [
        Brr.El.span
          ~at:Brr.At.[ class' (Jstr.v "location-icon") ]
          [ Brr.El.txt (Jstr.v "\u{1F5FA}\u{FE0F}") ];
        Brr.El.txt (Jstr.v "Pick on Map");
      ]
  in
  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun _ -> open_map ~lat:!current_lat ~lon:!current_lon)
       (Brr.El.as_target map_btn));

  (* Featured Locations *)
  let location_list =
    Brr.El.ul ~at:Brr.At.[ class' (Jstr.v "location-list") ] []
  in
  let featured_items =
    List.map
      (fun (name, lat, lon, alpha, beta, z) ->
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
               (* In place, with the camera preset: no navigation, so
                  fullscreen mode survives the switch. *)
               close_menu ();
               !switch_location ~push:true
                 ~camera:(Some (alpha, beta, z))
                 ~lat ~lon)
             (Brr.El.as_target item));
        Brr.El.append_children location_list [ item ];
        item)
      featured_locations
  in

  let focusables =
    [ input; btn_go; map_btn; current_loc_btn ] @ featured_items
  in
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
                 Brr.El.set_has_focus true next
             | "ArrowUp" ->
                 Brr.Ev.prevent_default e;
                 Brr.Ev.stop_propagation e;
                 let prev = List.nth focusables ((i - 1 + n) mod n) in
                 Brr.El.set_has_focus true prev
             | "Escape" ->
                 Brr.Ev.prevent_default e;
                 Brr.Ev.stop_propagation e;
                 toggle_menu ()
             | "Enter" when el != input && el != btn_go ->
                 Brr.Ev.prevent_default e;
                 Brr.Ev.stop_propagation e;
                 Brr.El.click el
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
      input_error;
      quick_select_header;
      map_btn;
      current_loc_btn;
      Brr.El.div
        ~at:Brr.At.[ class' (Jstr.v "section-title") ]
        [ Brr.El.txt (Jstr.v "Featured") ];
      location_list;
    ];

  (* Only the geolocation entry goes away when the device position was of no
     use; the section header stays, "Pick on Map" still being under it. *)
  fun visible ->
    let disp = Jstr.v (if visible then "flex" else "none") in
    Brr.El.set_inline_style (Jstr.v "display") disp current_loc_btn

let update_fab_orientation angle =
  List.iter
    (fun fab ->
      let is_rot90 = Float.abs (angle -. (-.Float.pi /. 2.)) < 0.01 in
      let is_rot180 = Float.abs (Float.abs angle -. Float.pi) < 0.01 in
      let is_rot270 = Float.abs (angle -. (Float.pi /. 2.)) < 0.01 in
      Brr.El.set_class (Jstr.v "rot90") is_rot90 fab;
      Brr.El.set_class (Jstr.v "rot180") is_rot180 fab;
      Brr.El.set_class (Jstr.v "rot270") is_rot270 fab)
    !fab_els

(* iOS 13+ does not deliver any orientation event until permission is granted
   via [DeviceOrientationEvent.requestPermission ()], which must be called from
   a user gesture. The method is absent on other browsers (Android/desktop),
   where orientation events flow without any prompt. *)
let needs_orientation_permission () =
  let doe = Jv.get Jv.global "DeviceOrientationEvent" in
  (not (Jv.is_none doe))
  && Jstr.equal (Jv.typeof (Jv.get doe "requestPermission")) (Jstr.v "function")

(* Requests permission; resolves to [true] when granted. Must be called
   synchronously from within a user-gesture handler. *)
let request_orientation_permission () =
  let doe = Jv.get Jv.global "DeviceOrientationEvent" in
  let p = Jv.call doe "requestPermission" [||] in
  Lwt.catch
    (fun () ->
      let* res = to_lwt (Fut.of_promise ~ok:Jv.to_jstr p) in
      Lwt.return (Jstr.equal res (Jstr.v "granted")))
    (fun _ -> Lwt.return false)

let compass_button = ref None
let orientation_permission_granted = ref false

let remove_compass_button () =
  match !compass_button with
  | Some b ->
      Brr.El.remove b;
      compass_button := None
  | None -> ()

(* Shows the explicit "Enable compass" button (iOS only). Tapping it requests
   motion permission from within the gesture; on grant the orientation events
   start flowing and the normal sensor flow takes over, on denial we fall back
   to Manual (touch/drag) mode. *)
let show_compass_button () =
  remove_compass_button ();
  let btn =
    Brr.El.button
      ~at:Brr.At.[ class' (Jstr.v "btn-compass") ]
      [ Brr.El.txt (Jstr.v "\u{1F9ED} Enable compass") ]
  in
  ignore
    (Brr.Ev.listen Brr.Ev.click
       (fun _ ->
         Lwt.async (fun () ->
             let* granted = request_orientation_permission () in
             remove_compass_button ();
             if granted then orientation_permission_granted := true
             else begin
               input_mode := Manual;
               display_temporary_message
                 "Sensor unavailable \u{2014} drag to look around"
             end;
             Lwt.return ()))
       (Brr.El.as_target btn));
  Brr.El.append_children (Brr.Document.body Brr.G.document) [ btn ];
  compass_button := Some btn

(* Shows the button only on iOS and only while permission is still missing. *)
let maybe_show_compass_button () =
  if needs_orientation_permission () && not !orientation_permission_granted then
    show_compass_button ()

(* Mode transitions. Entering Sensor mode (re-)prompts for permission when it is
   still missing; leaving it drops the prompt so it never lingers over the
   canvas while sensor input is unused. *)
let enter_sensor_mode () =
  input_mode := Sensor;
  maybe_show_compass_button ()

let enter_manual_mode () =
  input_mode := Manual;
  remove_compass_button ()

let setup_events canvas =
  (* Chrome/Android deliver absolute orientation through
     [deviceorientationabsolute]. iOS Safari never fires that event: it only
     fires the plain [deviceorientation] event and exposes the absolute heading
     through the non-standard [webkitCompassHeading] property. We listen to
     both. *)
  let deviceorientationabsolute =
    Brr.Ev.Type.create (Jstr.v "deviceorientationabsolute")
  in
  let deviceorientation = Brr.Ev.Type.create (Jstr.v "deviceorientation") in
  let state = ref `Init in

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

  (* Going fullscreen is best effort: [requestFullscreen] is absent on iPhone
     Safari (a synchronous [TypeError]) and rejects when the request is denied,
     and [screen.orientation.lock] is missing or rejects on the same browsers.
     Both are wrapped, or every tap in Sensor mode raises uncaught. *)
  let switch_to_fullscreen () =
    match Brr.Document.fullscreen_element Brr.G.document with
    | None ->
        Lwt.async @@ fun () ->
        Lwt.catch
          (fun () ->
            let* () =
              to_lwt
                (Brr.El.request_fullscreen
                   ~opts:
                     (Brr.El.fullscreen_opts
                        ~navigation_ui:Brr.El.Navigation_ui.hide ())
                   (Brr.Document.body Brr.G.document))
            in
            to_lwt
              (Fut.of_promise ~ok:ignore
                 (Jv.call
                    (Jv.get (Jv.get Jv.global "screen") "orientation")
                    "lock"
                    [| Jv.of_jstr (Jstr.v "natural") |])))
          (fun exn ->
            Brr.Console.(
              warn
                [
                  Jstr.v
                    (Printf.sprintf "Cannot switch to fullscreen: %s"
                       (Printexc.to_string exn));
                ]);
            Lwt.return ())
    | Some _ -> ()
  in

  let handle_tap () =
    let now = now_ms () in
    if now -. !last_tap_time < double_tap_threshold then begin
      (* Double tap - switch back to sensor mode *)
      if !input_mode = Manual then begin
        enter_sensor_mode ();
        display_temporary_message "Sensor mode"
      end;
      last_tap_time := 0.
    end
    else begin
      (* First tap - toggle fullscreen only in Sensor mode *)
      if !input_mode = Sensor then switch_to_fullscreen ();
      last_tap_time := now
    end
  in

  (* Device orientation handler. [absolute] is true for events coming from the
     [deviceorientationabsolute] event (Chrome/Android). When that event is
     available we ignore the relative [deviceorientation] event to avoid
     double-handling; iOS only delivers the latter. *)
  let got_absolute = ref false in
  let handle_orientation ~absolute ev =
    if absolute then got_absolute := true;
    let evt = Brr.Ev.as_type ev in
    (* iOS: [alpha] is relative to an arbitrary startup heading; the absolute
       heading is in [webkitCompassHeading], measured clockwise from north.
       Convert it to the W3C absolute-alpha convention (counter-clockwise from
       north) so the rest of the math is unchanged. *)
    let compass = Jv.get evt "webkitCompassHeading" in
    let has_compass = not (Jv.is_none compass) in
    (* A plain [deviceorientation] event may carry only relative data, whose
       [alpha] is measured from an arbitrary startup frame rather than north.
       Trust it only when it is flagged [absolute] or exposes the iOS compass
       heading; otherwise Sensor mode would point at a wrong but plausible
       heading. *)
    let evt_absolute =
      let a = Jv.get evt "absolute" in
      (not (Jv.is_none a)) && Jv.to_bool a
    in
    let is_absolute = absolute || has_compass || evt_absolute in
    (* Ignore relative events once a dedicated absolute source is available. *)
    if is_absolute && (absolute || not !got_absolute) then
      (* Bogus event on Chrome desktop *)
      begin if has_compass || not (Jv.is_null (Jv.get evt "alpha")) then (
        let screen =
          Jv.to_float
            (Jv.get (Jv.get (Jv.get Jv.global "screen") "orientation") "angle")
        in
        let angle nm = Jv.to_float (Jv.get evt nm) in
        let alpha =
          if has_compass then 360. -. Jv.to_float compass else angle "alpha"
        in
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
        sensor_orientation := q;
        let screen_delta = screen -. !last_screen_angle in
        if abs_float screen_delta > 1. then begin
          let q_delta =
            Quaternion.from_axis_angle
              { x = 0.; y = 0.; z = 1.; w = 0. }
              (-.screen_delta *. Float.pi /. 180.)
          in
          current_orientation := Quaternion.mult !current_orientation q_delta;
          fab_orientation := 0.;
          update_fab_orientation 0.
        end;
        last_screen_angle := screen;
        let new_fab_angle =
          snap_inclination ~current_locked:!fab_orientation !sensor_orientation
        in
        if new_fab_angle <> !fab_orientation then begin
          fab_orientation := new_fab_angle;
          update_fab_orientation new_fab_angle
        end;
        if !input_mode = Sensor then begin
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
      end
  in
  (* Device orientation listeners - only active in Sensor mode *)
  ignore
    (Brr.Ev.listen deviceorientationabsolute
       (handle_orientation ~absolute:true)
       (Brr.Window.as_target Brr.G.window));
  ignore
    (Brr.Ev.listen deviceorientation
       (handle_orientation ~absolute:false)
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
               enter_manual_mode ();
               current_orientation :=
                 apply_manual_rotation !current_orientation
                   (5. *. pi /. 180.)
                   0.
           | "ArrowRight" ->
               (* Yaw Right: -5 degrees *)
               enter_manual_mode ();
               current_orientation :=
                 apply_manual_rotation !current_orientation
                   (-5. *. pi /. 180.)
                   0.
           | "ArrowDown" ->
               (* Pitch Down: -5 degrees *)
               enter_manual_mode ();
               current_orientation :=
                 apply_manual_rotation !current_orientation 0.
                   (-5. *. pi /. 180.)
           | "ArrowUp" ->
               (* Pitch Up: 5 degrees *)
               enter_manual_mode ();
               current_orientation :=
                 apply_manual_rotation !current_orientation 0. (5. *. pi /. 180.)
           | "Equal" | "NumpadAdd" -> zoom := clamp_zoom (!zoom *. 1.1)
           | "Minus" | "NumpadSubtract" -> zoom := clamp_zoom (!zoom /. 1.1)
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
           zoom := clamp_zoom (!zoom *. factor))
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
           last_input_time := now_ms ();
           mouse_start_x := x;
           mouse_start_y := y;
           mouse_last_x := x;
           mouse_last_y := y)
         target);

    ignore
      (Brr.Ev.listen Brr.Ev.mousemove
         (fun ev ->
           if !mouse_dragging then begin
             if !input_mode = Sensor then
               locked_inclination := nearest_inclination !sensor_orientation;
             enter_manual_mode ();
             let mouse = Brr.Ev.as_type ev in
             let x = Brr.Ev.Mouse.client_x mouse in
             let y = Brr.Ev.Mouse.client_y mouse in
             let dx = x -. !mouse_last_x in
             let dy = y -. !mouse_last_y in
             let h = Brr.El.inner_h canvas in
             let w = Brr.El.inner_w canvas in
             let speed =
               2.0 /. (max w h *. scale *. !zoom) *. 180. /. Float.pi
             in
             let gamma = -. !locked_inclination in
             let c = cos gamma in
             let s = sin gamma in
             let dx_eff = (dx *. c) -. (dy *. s) in
             let dy_eff = (dx *. s) +. (dy *. c) in
             let da = dx_eff *. speed in
             let db = dy_eff *. speed in
             let t = now_ms () in
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
             last_input_time := now_ms ()
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
                 locked_inclination := nearest_inclination !sensor_orientation;
                 enter_manual_mode ();
                 display_temporary_message "Manual mode"
               end;
               Brr.Ev.prevent_default ev
             end;
             if !touch_dragging then begin
               let h = Brr.El.inner_h canvas in
               let w = Brr.El.inner_w canvas in
               let speed =
                 2.0 /. (max w h *. scale *. !zoom) *. 180. /. Float.pi
               in
               let gamma = -. !locked_inclination in
               let c = cos gamma in
               let s = sin gamma in
               let dx_eff = (dx *. c) -. (dy *. s) in
               let dy_eff = (dx *. s) +. (dy *. c) in
               let da = dx_eff *. speed in
               let db = dy_eff *. speed in
               let t = now_ms () in
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
                 zoom := clamp_zoom (!zoom *. factor);
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
             if now_ms () -. !last_input_time > 300. then velocity := (0., 0.);
             if not !touch_dragging then begin
               Brr.Ev.prevent_default ev;
               (* This was a tap, not a drag *)
               handle_tap ()
             end;
             touch_dragging := false;
             pinch_distance := 0.
           end)
         target);

    state := `Starting;
    (* On iOS, sensor input is blocked until the user grants motion permission;
       surface an explicit button to request it. *)
    if !input_mode = Sensor then maybe_show_compass_button ()

(* Offline support is a bonus: [navigator.serviceWorker] is undefined on
   insecure origins (plain http, e.g. LAN testing) and registration fails in
   Firefox private browsing or when the script is missing. Registering must
   thus not raise at initialization time. *)
let service_worker_registration =
  let open Brr_webworkers.Service_worker in
  let container = Container.of_navigator Brr.G.navigator in
  if Jv.is_none (Container.to_jv container) then None
  else
    match Container.register container (Jstr.v "service_worker.bc.js") with
    | registration -> Some (container, registration)
    | exception Jv.Error _ -> None

(* Wait for the service worker to control the page, so that the tiles loaded at
   startup do get cached. Never fails and never waits for ever: a missing or
   broken service worker must not keep the app on the loading screen. *)
let wait_for_service_worker =
  let open Brr_webworkers.Service_worker in
  let fut, set = Fut.create () in
  let settled = ref false in
  let proceed () =
    if not !settled then begin
      settled := true;
      set (Ok ())
    end
  in
  (match service_worker_registration with
  | None ->
      Brr.Console.(
        warn [ Jstr.v "Service workers unavailable: running without cache" ]);
      proceed ()
  | Some (container, registration) ->
      Fut.await
        (let open Fut.Result_syntax in
         let* _ = registration in
         Container.ready container)
        (function
          | Error e ->
              Brr.Console.error [ e ];
              proceed ()
          | Ok r -> (
              match Registration.active r with
              | None -> proceed ()
              | Some w ->
                  if state w = State.activated then proceed ()
                  else
                    ignore
                      (Brr.Ev.listen Brr.Ev.statechange
                         (fun _ -> if state w = State.activated then proceed ())
                         (as_target w))));
      (* Backstop: [Container.ready] never resolves if activation stalls. *)
      Fut.await (Fut.tick ~ms:10_000) (fun () ->
          (if not !settled then
             Brr.Console.(
               warn [ Jstr.v "Service worker not ready: proceeding without it" ]));
          proceed ()));
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
      (Brr_canvas.Gl.get_context
         ~attrs:(Gl.Attrs.v ~alpha:false ())
         (Brr_canvas.Canvas.of_el canvas))
  in
  (* iOS/Safari color-manage the canvas to the display's wide (Display-P3)
     gamut, while many Android devices send the raw sRGB values straight to a
     saturated panel, looking more vivid. Opt into the wide gamut so both render
     consistently and saturated rather than washed out on iOS. Safari and Chrome
     honour this; browsers that don't simply ignore the assignment.
     [Brr_canvas.Gl] does not expose the context as a [Jv.t], so re-fetch it
     from the canvas ([getContext] returns the same context object). *)
  Jv.set
    (Jv.call (Brr.El.to_jv canvas) "getContext"
       [| Jv.of_jstr (Jstr.v "webgl2") |])
    "drawingBufferColorSpace"
    (Jv.of_jstr (Jstr.v "display-p3"));
  (* Initialize anisotropic filtering early for the detail map *)
  init_anisotropic_filtering ctx;
  (* Start loading detail map immediately *)
  let detail_map = make_detail_map ctx in
  Worker_pool.init ();
  let graphics = init_graphics ctx in
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

  (* [get_position] switches to "Getting current location..." only if it has to
     fall back to the device position. *)
  update_startup_status "Loading Terrain..." true;
  let* () = to_lwt wait_for_service_worker in
  let* source, (lat, lon, angle, pitch, z) =
    to_lwt (get_position ~size:tile_width)
  in
  current_lat := lat;
  current_lon := lon;
  zoom := z;
  Jv.set Jv.global "flyToCoords"
    (Jv.callback ~arity:3 (fun lat_jv lon_jv bearing_jv ->
         let lat = Jv.to_float lat_jv in
         let lon = Jv.to_float lon_jv in
         let bearing =
           if Jv.is_null bearing_jv || Jv.is_undefined bearing_jv then None
           else Some (Jv.to_float bearing_jv)
         in
         Option.iter (fun _ -> enter_manual_mode ()) bearing;
         !switch_location ~push:true
           ~camera:(Option.map (fun alpha -> (-.alpha, 90., !zoom)) bearing)
           ~lat ~lon));
  (* If URL parameters were provided but we fell back to another source (e.g. out of range),
     redirect to the resolved location. *)
  (match source with
  | Preset -> set_loc_visible false
  | Url ->
      (* The device was never asked, so the entry's usefulness is still open;
         settle it off to one side rather than leave an option that cannot
         work. *)
      Fut.await (geolocation_offerable ()) (fun offerable ->
          if not offerable then set_loc_visible false)
  | _ ->
      let params = Brr.Uri.query_params (Brr.Window.location Brr.G.window) in
      if Brr.Uri.Params.mem (Jstr.v "lat") params then
        let search =
          Jstr.v
            (Printf.sprintf "?lat=%s&lon=%s&alpha=%s&beta=%s&zoom=%.2f"
               (format_float lat) (format_float lon) (format_float angle)
               (format_float pitch) z)
        in
        let uri =
          Brr.Uri.with_query_params
            (Brr.Window.location Brr.G.window)
            (Brr.Uri.Params.of_jstr search)
        in
        navigate_to uri);
  (* current_orientation := { alpha = angle; beta = 90.; gamma = 0.; screen = 0. }; *)
  set_orientation angle pitch;
  setup_popstate_listener ~size:tile_width;

  let start =
    let start = setup_events canvas in
    fun () ->
      start ();
      (* Warming the surroundings must not compete with the tiles of the view
         being displayed: only start once the first location is up. *)
      if source = Geolocation then begin
        (* Warming the surroundings is what makes the tile cache worth keeping,
           and the only moment a request to keep it means anything to the user:
           the app is visibly downloading where they are. See
           [requestPersistentStorage] in index.html, absent if that script has
           not run. *)
        let request = Jv.get Jv.global "requestPersistentStorage" in
        if not (Jv.is_none request) then ignore (Jv.apply request [||]);
        Lwt.async (fun () -> Dem_loader.prefetch ~size:7200 ~lat ~lon);
        Lwt.async (fun () -> Clc_loader.prefetch ~size:7200 ~lat ~lon);
        Lwt.async (fun () -> Hd_dem.prefetch Hd_dem.l13 ~lat ~lon)
      end
  in
  update_startup_status "Loading Terrain..." true;

  let* () =
    run_renderer ~w:tile_width ~h:tile_height ~lat ~lon canvas ctx ~detail_map
      ~graphics ~start
  in
  Lwt.return_unit

let () =
  Lwt.async (fun () ->
      Lwt.catch main (fun e ->
          (match e with Jv.Error e -> Brr.Console.error [ e ] | _ -> ());
          display_message (Printexc.to_string e);
          Lwt.fail e))
