(* render_dem.ml *)
open Tsdl
open Tgles3
open Bigarray

(* --- 1. Shader Sources --- *)
let vs_src =
  "#version 300 es\n\
   layout(location = 0) in vec2 in_pos;\n\
   layout(location = 1) in float in_height;\n\
   uniform vec2 u_view_scale;\n\
   uniform vec2 u_view_offset;\n\
   out float v_height;\n\
   out vec2 v_uv;\n\
   void main() {\n\
   vec2 p = (in_pos - u_view_offset) * u_view_scale * 2.0;\n\
   gl_Position = vec4(p, 0.0, 1.0);\n\
   v_height = in_height;\n\
   v_uv = in_pos;\n\
   }\n"

let fs_src =
  "#version 300 es\n\
   precision mediump float;\n\
   in float v_height;\n\
   in vec2 v_uv;\n\
   uniform bool u_use_lighting;\n\
   uniform bool u_use_color;\n\
   uniform sampler2D u_heightmap;\n\
   uniform vec2 u_res;\n\n\
   out vec4 out_color;\n\n\
   vec3 hypsometric_tint(float h) {\n\
   // Simple elevation color-coding\n\
   if (h < 0.0) return vec3(0.0, 0.0, 0.5); // Below sea level: deep blue\n\
   if (h < 500.0) return mix(vec3(0.1, 0.4, 0.1), vec3(0.5, 0.7, 0.2), h / \
   500.0);\n\
   if (h < 1000.0) return mix(vec3(0.5, 0.7, 0.2), vec3(0.9, 0.9, 0.4), (h - \
   500.0) / 500.0);\n\
   if (h < 2000.0) return mix(vec3(0.9, 0.9, 0.4), vec3(0.6, 0.4, 0.2), (h - \
   1000.0) / 1000.0);\n\
   if (h < 3000.0) return mix(vec3(0.6, 0.4, 0.2), vec3(0.8, 0.8, 0.8), (h - \
   2000.0) / 1000.0);\n\
   return vec3(1.0, 1.0, 1.0); // Snow\n\
   }\n\n\
   void main() {\n\
   vec3 color;\n\
   if (u_use_color) {\n\
   color = hypsometric_tint(v_height);\n\
   } else {\n\
   float h = (v_height + 500.0) / 9500.0;\n\
   color = vec3(h);\n\
   }\n\n\
   if (u_use_lighting) {\n\
   vec2 tex_coord = vec2(v_uv.x, 1.0 - v_uv.y);\n\
   float h_c = texture(u_heightmap, tex_coord).r;\n\
   float h_r = texture(u_heightmap, tex_coord + vec2(1.0/u_res.x, 0.0)).r;\n\
   float h_d = texture(u_heightmap, tex_coord + vec2(0.0, 1.0/u_res.y)).r;\n\n\
   // Approximate normals\n\
   // Note: h_d is actually South in the texture (increasing V), \n\
   // which corresponds to decreasing Y in world space.\n\
   float dx = (h_r - h_c) * 0.1;\n\
   float dy = (h_c - h_d) * 0.1;\n\
   vec3 normal = normalize(vec3(-dx, -dy, 1.0));\n\n\
   vec3 light_dir = normalize(vec3(0.5, 0.5, 1.0));\n\
   float diff = max(0.1, dot(normal, light_dir));\n\
   color *= diff;\n\
   }\n\n\
   out_color = vec4(color, 1.0);\n\
   }\n"

(* --- 2. DEM Decoding --- *)

let zigzag_decode n = (n lsr 1) lxor -(n land 1)

let uncompress_string str =
  let input_pos = ref 0 in
  let len = String.length str in
  let input_cb buf =
    let n = min (Bytes.length buf) (len - !input_pos) in
    if n > 0 then (
      Bytes.blit_string str !input_pos buf 0 n;
      input_pos := !input_pos + n);
    n
  in
  let out_buf = Buffer.create (String.length str * 4) in
  let output_cb buf n = Buffer.add_subbytes out_buf buf 0 n in
  Zlib.uncompress input_cb output_cb;
  Buffer.contents out_buf

let load_dem file =
  Printf.printf "Loading %s...\n%!" file;
  let ic = open_in_bin file in
  let magic = really_input_string ic 4 in
  if magic <> "DEM1" then failwith "Invalid magic";

  let read_u32 () =
    let b = really_input_string ic 4 in
    Int32.to_int (String.get_int32_le b 0)
  in
  let read_f32 () =
    let b = really_input_string ic 4 in
    Int32.float_of_bits (String.get_int32_le b 0)
  in

  let width = read_u32 () in
  let height = read_u32 () in
  let min_elev = read_f32 () in
  let max_elev = read_f32 () in
  let high_len = read_u32 () in
  let low_len = read_u32 () in

  Printf.printf "Header: %dx%d, elev=[%.1f, %.1f]\n%!" width height min_elev
    max_elev;

  let high_comp = really_input_string ic high_len in
  let low_comp = really_input_string ic low_len in
  close_in ic;

  let high_str = uncompress_string high_comp in
  let low_str = uncompress_string low_comp in

  let n = width * height in
  let u16_data = Array.make n 0 in

  let predict left up up_left = left + up - up_left in

  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      let idx = (row * width) + col in
      let hx = Char.code high_str.[idx] in
      let lx = Char.code low_str.[idx] in
      let zx = (hx lsl 8) lor lx in
      let residue = zigzag_decode zx in

      let predicted =
        if row = 0 && col = 0 then 0
        else if row = 0 then u16_data.(idx - 1)
        else if col = 0 then u16_data.(idx - width)
        else
          predict
            u16_data.(idx - 1)
            u16_data.(idx - width)
            u16_data.(idx - width - 1)
      in
      u16_data.(idx) <- (predicted + residue) land 0xFFFF
    done
  done;

  let scale = (max_elev -. min_elev) /. 65535.0 in
  let heights = Array1.create float32 c_layout n in
  for i = 0 to n - 1 do
    heights.{i} <- (float u16_data.(i) *. scale) +. min_elev
  done;
  (width, height, heights)

(* --- 3. GL Helpers --- *)

let get_iv get_fn obj param =
  let buf = Array1.create int32 c_layout 1 in
  get_fn obj param buf;
  Int32.to_int (Array1.get buf 0)

let get_log get_iv_fn get_log_fn obj =
  let len = get_iv get_iv_fn obj Gl.info_log_length in
  let buf = Array1.create char c_layout len in
  get_log_fn obj len None buf;
  let s = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set s i (Array1.get buf i)
  done;
  Bytes.to_string s

let compile_shader type_ src =
  let s = Gl.create_shader type_ in
  Gl.shader_source s src;
  Gl.compile_shader s;
  if get_iv Gl.get_shaderiv s Gl.compile_status = Gl.false_ then (
    Printf.printf "Shader err: %s\n"
      (get_log Gl.get_shaderiv Gl.get_shader_info_log s);
    exit 1);
  s

let create_program vs fs =
  let p = Gl.create_program () in
  Gl.attach_shader p vs;
  Gl.attach_shader p fs;
  Gl.link_program p;
  if get_iv Gl.get_programiv p Gl.link_status = Gl.false_ then (
    Printf.printf "Link err: %s\n"
      (get_log Gl.get_programiv Gl.get_program_info_log p);
    exit 1);
  p

(* --- 4. Main --- *)

let () =
  let file_ref = ref "" in
  let use_lighting = ref false in
  let use_color = ref false in

  let rec parse_args i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "-light" ->
          use_lighting := true;
          parse_args (i + 1)
      | "-color" ->
          use_color := true;
          parse_args (i + 1)
      | f ->
          file_ref := f;
          parse_args (i + 1)
  in
  parse_args 1;

  if !file_ref = "" then (
    Printf.printf "Usage: %s [-light] [-color] <file.dem>\n" Sys.argv.(0);
    exit 1);

  let file = !file_ref in
  let width, height, heights = load_dem file in

  (* Simple latitude parsing from filename Nxx_Exxx_... *)
  let lat =
    try
      let base = Filename.basename file in
      if base.[0] = 'N' then int_of_string (String.sub base 1 2)
      else if base.[0] = 'S' then -int_of_string (String.sub base 1 2)
      else 45 (* Fallback *)
    with _ -> 45
  in
  let lat_correction = cos (float lat *. Float.pi /. 180.0) in

  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Printf.printf "%s\n" e;
      exit 1
  | Ok () -> (
      ignore
        (Sdl.gl_set_attribute Sdl.Gl.context_profile_mask
           Sdl.Gl.context_profile_es);
      ignore (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
      ignore (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 0);

      match Sdl.create_window "DEM Viewer" ~w:1000 ~h:800 Sdl.Window.opengl with
      | Error (`Msg e) ->
          Printf.printf "%s\n" e;
          exit 1
      | Ok window ->
          let _ctx =
            match Sdl.gl_create_context window with
            | Ok c -> c
            | Error _ -> exit 1
          in
          ignore (Sdl.gl_set_swap_interval 1);

          let prog =
            create_program
              (compile_shader Gl.vertex_shader vs_src)
              (compile_shader Gl.fragment_shader fs_src)
          in
          let u_view_scale = Gl.get_uniform_location prog "u_view_scale" in
          let u_view_offset = Gl.get_uniform_location prog "u_view_offset" in
          let u_use_lighting = Gl.get_uniform_location prog "u_use_lighting" in
          let u_use_color = Gl.get_uniform_location prog "u_use_color" in
          let u_heightmap = Gl.get_uniform_location prog "u_heightmap" in
          let u_res = Gl.get_uniform_location prog "u_res" in

          (* Create heightmap texture *)
          let tex =
            let buf = Array1.create int32 c_layout 1 in
            Gl.gen_textures 1 buf;
            Int32.to_int buf.{0}
          in
          Gl.active_texture Gl.texture0;
          Gl.bind_texture Gl.texture_2d tex;
          Gl.tex_image2d Gl.texture_2d 0 Gl.r32f width height 0 Gl.red Gl.float
            (`Data heights);
          Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
          Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
          Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
          Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;

          (* Create grid geometry *)
          let n_verts = width * height in
          let pos_data = Array1.create float32 c_layout (n_verts * 2) in
          for row = 0 to height - 1 do
            for col = 0 to width - 1 do
              let i = (row * width) + col in
              pos_data.{i * 2} <- float col /. float (width - 1);
              pos_data.{(i * 2) + 1} <- 1.0 -. (float row /. float (height - 1))
            done
          done;

          let n_indices = (width - 1) * (height - 1) * 6 in
          let indices = Array1.create int32 c_layout n_indices in
          let ptr = ref 0 in
          for row = 0 to height - 2 do
            for col = 0 to width - 2 do
              let i = (row * width) + col in
              indices.{!ptr} <- Int32.of_int i;
              indices.{!ptr + 1} <- Int32.of_int (i + 1);
              indices.{!ptr + 2} <- Int32.of_int (i + width);
              indices.{!ptr + 3} <- Int32.of_int (i + 1);
              indices.{!ptr + 4} <- Int32.of_int (i + width + 1);
              indices.{!ptr + 5} <- Int32.of_int (i + width);
              ptr := !ptr + 6
            done
          done;

          let vao =
            let buf = Array1.create int32 c_layout 1 in
            Gl.gen_vertex_arrays 1 buf;
            Int32.to_int buf.{0}
          in
          Gl.bind_vertex_array vao;

          let vbos = Array1.create int32 c_layout 3 in
          Gl.gen_buffers 3 vbos;
          let vbo_pos = Int32.to_int vbos.{0} in
          let vbo_height = Int32.to_int vbos.{1} in
          let vbo_ebo = Int32.to_int vbos.{2} in

          Gl.bind_buffer Gl.array_buffer vbo_pos;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size pos_data)
            (Some pos_data) Gl.static_draw;
          Gl.enable_vertex_attrib_array 0;
          Gl.vertex_attrib_pointer 0 2 Gl.float false 0 (`Offset 0);

          Gl.bind_buffer Gl.array_buffer vbo_height;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size heights)
            (Some heights) Gl.static_draw;
          Gl.enable_vertex_attrib_array 1;
          Gl.vertex_attrib_pointer 1 1 Gl.float false 0 (`Offset 0);

          Gl.bind_buffer Gl.element_array_buffer vbo_ebo;
          Gl.buffer_data Gl.element_array_buffer
            (Gl.bigarray_byte_size indices)
            (Some indices) Gl.static_draw;

          let zoom = ref 1.0 in
          let view_x = ref 0.5 in
          let view_y = ref 0.5 in
          let drag = ref false in

          let rec loop () =
            let e = Sdl.Event.create () in
            while Sdl.poll_event (Some e) do
              match Sdl.Event.(enum (get e typ)) with
              | `Quit -> exit 0
              | `Mouse_wheel ->
                  let dy = Sdl.Event.(get e mouse_wheel_y) in
                  if dy > 0 then zoom := !zoom *. 1.1 else zoom := !zoom /. 1.1
              | `Mouse_button_down -> drag := true
              | `Mouse_button_up -> drag := false
              | `Mouse_motion when !drag ->
                  let dx = float Sdl.Event.(get e mouse_motion_xrel) in
                  let dy = float Sdl.Event.(get e mouse_motion_yrel) in
                  view_x := !view_x -. (dx /. (500.0 *. !zoom));
                  view_y := !view_y +. (dy /. (500.0 *. !zoom))
              | `Key_down ->
                  let k = Sdl.Event.(get e keyboard_keycode) in
                  if k = Sdl.K.l then use_lighting := not !use_lighting
                  else if k = Sdl.K.c then use_color := not !use_color
              | _ -> ()
            done;

            let w, h = Sdl.get_window_size window in
            Gl.viewport 0 0 w h;
            Gl.clear Gl.color_buffer_bit;

            Gl.use_program prog;
            let aspect = float w /. float h in

            (* Calculate scale with latitude correction *)
            let sx = !zoom /. aspect *. lat_correction in
            let sy = !zoom in

            Gl.uniform2f u_view_scale sx sy;
            Gl.uniform2f u_view_offset !view_x !view_y;
            Gl.uniform1i u_use_lighting (if !use_lighting then 1 else 0);
            Gl.uniform1i u_use_color (if !use_color then 1 else 0);
            Gl.uniform1i u_heightmap 0;
            Gl.uniform2f u_res (float width) (float height);

            Gl.bind_vertex_array vao;
            Gl.draw_elements Gl.triangles n_indices Gl.unsigned_int (`Offset 0);

            Sdl.gl_swap_window window;
            loop ()
          in
          loop ())
