(* render_clc.ml *)
(* Compile command:
   ocamlfind ocamlopt -package tsdl,tgles3,bigarray -linkpkg render_clc.ml -o render_clc
*)

open Tsdl
open Tgles3
open Bigarray

(* --- 1. Palette Definition --- *)
let raw_palette =
  [
    ("0", 255, 0, 255);
    (* Fallback Magenta *)
    ("111", 230, 0, 77);
    ("112", 255, 0, 0);
    ("121", 204, 77, 242);
    ("122", 204, 0, 0);
    ("123", 230, 204, 230);
    ("124", 230, 204, 230);
    ("131", 166, 0, 204);
    ("132", 166, 77, 0);
    ("133", 255, 77, 255);
    ("141", 255, 166, 255);
    ("142", 255, 230, 255);
    ("211", 255, 255, 168);
    ("212", 255, 255, 0);
    ("213", 230, 230, 0);
    ("221", 230, 128, 0);
    ("222", 242, 166, 77);
    ("223", 230, 166, 0);
    ("231", 230, 230, 77);
    ("241", 255, 230, 166);
    ("242", 255, 230, 77);
    ("243", 230, 204, 77);
    ("244", 242, 204, 166);
    ("311", 128, 255, 0);
    ("312", 0, 166, 0);
    ("313", 77, 255, 0);
    ("321", 204, 242, 77);
    ("322", 166, 255, 128);
    ("323", 166, 230, 77);
    ("324", 166, 242, 0);
    ("331", 230, 230, 230);
    ("332", 204, 204, 204);
    ("333", 204, 255, 204);
    ("334", 0, 0, 0);
    ("335", 166, 230, 204);
    ("411", 166, 166, 255);
    ("412", 77, 77, 255);
    ("421", 204, 204, 255);
    ("422", 230, 230, 255);
    ("423", 166, 166, 230);
    ("511", 0, 204, 242);
    ("512", 128, 242, 230);
    ("521", 0, 255, 166);
    ("522", 166, 255, 230);
    ("523", 230, 242, 255);
  ]

let code_map = Hashtbl.create 64

let () =
  List.iteri
    (fun i (code, _, _, _) ->
      Hashtbl.add code_map (try int_of_string code with _ -> 0) i)
    raw_palette

let get_code_index code =
  match Hashtbl.find_opt code_map code with Some i -> i | None -> 0

let create_palette_texture () =
  let buf = Array1.create int8_unsigned c_layout (256 * 3) in
  Array1.fill buf 0;
  List.iteri
    (fun i (_, r, g, b) ->
      let idx = i * 3 in
      Array1.set buf idx r;
      Array1.set buf (idx + 1) g;
      Array1.set buf (idx + 2) b)
    raw_palette;
  buf

let create_viewer_palette_texture () =
  let buf = Array1.create int8_unsigned c_layout (256 * 3) in
  Array1.fill buf 0;
  List.iteri
    (fun i (code_str, _, _, _) ->
      let code = try int_of_string code_str with _ -> 0 in
      let r, g, b =
        let idx = Clc_palette.get_index code in
        let mat = Clc_palette.materials.(idx) in
        mat.albedo
      in
      let idx = i * 3 in
      Array1.set buf idx r;
      Array1.set buf (idx + 1) g;
      Array1.set buf (idx + 2) b)
    raw_palette;
  buf

(* --- 2. Shaders --- *)
let vertex_shader_src =
  "#version 300 es\n\
   layout(location = 0) in vec2 in_norm_pos; // 0..1 from u16 0..65535\n\
   layout(location = 1) in int in_color_idx;\n\
   uniform vec2 u_range; // tile dim in deg\n\
   uniform vec2 u_min;   // tile origin in deg\n\
   uniform vec2 u_view_scale;\n\
   uniform vec2 u_view_offset;\n\
   flat out int v_idx;\n\
   void main() {\n\
  \    // map 0..1 to tile coords (deg)\n\
  \    vec2 world_pos = in_norm_pos * u_range + u_min;\n\
  \    // map world to screen\n\
  \    vec2 screen_pos = (world_pos - u_view_offset) * u_view_scale * 2.0;\n\
  \    // aspect ratio correction is handled in u_view_scale logic on CPU\n\
  \    gl_Position = vec4(screen_pos, 0.0, 1.0);\n\
  \    v_idx = in_color_idx;\n\
   }\n"

let fragment_shader_src =
  "#version 300 es\n\
   precision mediump float;\n\
   flat in int v_idx;\n\
   uniform mediump sampler2D u_palette;\n\
   out vec4 out_color;\n\
   void main() { out_color = texelFetch(u_palette, ivec2(v_idx, 0), 0); }\n"

(* --- Marker Shaders --- *)
let marker_vs_src =
  "#version 300 es\n\
   uniform vec2 u_pos;\n\
   uniform vec2 u_view_scale;\n\
   uniform vec2 u_view_offset;\n\
   void main() {\n\
  \    vec2 screen_pos = (u_pos - u_view_offset) * u_view_scale * 2.0;\n\
  \    gl_Position = vec4(screen_pos, 0.0, 1.0);\n\
  \    gl_PointSize = 6.0;\n\
   }\n"

let marker_fs_src =
  "#version 300 es\n\
   precision mediump float;\n\
   uniform vec3 u_color;\n\
   out vec4 out_color;\n\
   void main() { out_color = vec4(u_color, 1.0); }\n"

(* --- Water Shaders (32-bit integer positions) --- *)
let water_vs_src =
  "#version 300 es\n\
   layout(location = 0) in ivec2 in_pos; // 24-bit quantized as int32\n\
   layout(location = 1) in int in_color_idx;\n\
   uniform vec2 u_water_range; // range in degrees\n\
   uniform float u_water_scale; // quantization scale\n\
   uniform vec2 u_min;   // tile origin in deg\n\
   uniform vec2 u_view_scale;\n\
   uniform vec2 u_view_offset;\n\
   flat out int v_idx;\n\
   void main() {\n\
   // Convert quantized int to normalized, then to world coords\n\
   vec2 norm_pos = vec2(in_pos) / u_water_scale;\n\
   vec2 world_pos = norm_pos * u_water_range + u_min;\n\
   vec2 screen_pos = (world_pos - u_view_offset) * u_view_scale * 2.0;\n\
   gl_Position = vec4(screen_pos, 0.0, 1.0);\n\
   v_idx = in_color_idx;\n\
   }\n"

(* --- 3. CLC Loading --- *)

type poi_type = Peak | Saddle
type poi = { lon : float; lat : float; poi_type : poi_type }

let load_clc file =
  Printf.printf "Loading %s...\n%!" file;
  let tile = Clc_file.read file in
  let float i =
    Int64.float_of_bits (String.get_int64_le tile.Clc_file.floats (8 * i))
  in
  let min_lon = float 0 and min_lat = float 1 in
  let scale_x = float 2 and scale_y = float 3 in
  let water_scale_x = float 4 and water_scale_y = float 5 in
  let poi_scale_x = float 6 and poi_scale_y = float 7 in

  let unknown_codes = Hashtbl.create 5 in

  (* The features' vertices and triangles, one feature after another *)
  let flatten (features : Clc_codec.feature array) =
    let nv =
      Array.fold_left (fun a f -> a + Array.length f.Clc_codec.xs) 0 features
    in
    let ni =
      Array.fold_left (fun a f -> a + Array.length f.Clc_codec.tris) 0 features
    in
    let pos = Array.make (2 * nv) 0 in
    let col = Array1.create int8_unsigned c_layout nv in
    let ebo = Array1.create int32 c_layout ni in
    let v = ref 0 and i = ref 0 in
    Array.iter
      (fun (f : Clc_codec.feature) ->
        if not (Hashtbl.mem code_map f.code || Hashtbl.mem unknown_codes f.code)
        then begin
          Printf.printf "Warning: Unknown CLC Code %d \n" f.code;
          Hashtbl.add unknown_codes f.code true
        end;
        let code_idx = get_code_index f.code in
        Array.iteri
          (fun k x ->
            pos.(2 * (!v + k)) <- x;
            pos.((2 * (!v + k)) + 1) <- f.ys.(k);
            Array1.set col (!v + k) code_idx)
          f.xs;
        Array.iteri
          (fun k t -> Array1.set ebo (!i + k) (Int32.of_int (!v + t)))
          f.tris;
        v := !v + Array.length f.xs;
        i := !i + Array.length f.tris)
      features;
    (pos, col, ebo, ni)
  in
  let pos, arr_col, arr_ebo, n_indices = flatten tile.cover in
  let arr_pos = Array1.create int16_unsigned c_layout (Array.length pos) in
  Array.iteri (Array1.set arr_pos) pos;
  let wpos, water_arr_col, water_arr_ebo, water_indices = flatten tile.water in
  let water_arr_pos = Array1.create int32 c_layout (Array.length wpos) in
  Array.iteri (fun k x -> Array1.set water_arr_pos k (Int32.of_int x)) wpos;
  Printf.printf
    "Decoded CLC: %d verts, %d indices; water: %d verts, %d indices\n%!"
    (Array.length pos / 2)
    n_indices
    (Array.length wpos / 2)
    water_indices;

  let poi_count = tile.poi_count in
  let pois =
    if poi_count > 0 then begin
      let names_str = List.nth tile.pois 0 in
      let coords_str = List.nth tile.pois 1 in
      let types_str = List.nth tile.pois 3 in

      let pois = ref [] in
      let names_pos = ref 0 in
      let coords_pos = ref 0 in
      let types_pos = ref 0 in

      for _ = 1 to poi_count do
        (* Read name (length-prefixed) *)
        let name_len = Char.code names_str.[!names_pos] in
        incr names_pos;
        let _name = String.sub names_str !names_pos name_len in
        names_pos := !names_pos + name_len;

        (* Read 3-byte coords *)
        let lx = Char.code coords_str.[!coords_pos] in
        let mx = Char.code coords_str.[!coords_pos + 1] in
        let hx = Char.code coords_str.[!coords_pos + 2] in
        let qx = lx lor (mx lsl 8) lor (hx lsl 16) in
        coords_pos := !coords_pos + 3;

        let ly = Char.code coords_str.[!coords_pos] in
        let my = Char.code coords_str.[!coords_pos + 1] in
        let hy = Char.code coords_str.[!coords_pos + 2] in
        let qy = ly lor (my lsl 8) lor (hy lsl 16) in
        coords_pos := !coords_pos + 3;

        (* Convert back to degrees *)
        let lon = min_lon +. (float qx /. poi_scale_x) in
        let lat = min_lat +. (float qy /. poi_scale_y) in

        (* Read type *)
        let typ = Char.code types_str.[!types_pos] in
        incr types_pos;
        let poi_type = if typ = 0 then Peak else Saddle in

        pois := { lon; lat; poi_type } :: !pois
      done;
      Printf.printf "Decoded %d POIs\n%!" poi_count;
      List.rev !pois
    end
    else []
  in

  (* Return CLC data, water data, POI data, and metadata *)
  ( n_indices,
    arr_pos,
    arr_col,
    arr_ebo,
    min_lon,
    min_lat,
    scale_x,
    scale_y,
    water_indices,
    water_arr_pos,
    water_arr_col,
    water_arr_ebo,
    water_scale_x,
    water_scale_y,
    pois )

(* --- 4. GL Helper --- *)
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

(* --- Main --- *)
let () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Printf.printf "%s\n" e;
      exit 1
  | Ok () -> (
      ignore (Sdl.gl_set_attribute Sdl.Gl.multisamplebuffers 1);
      ignore (Sdl.gl_set_attribute Sdl.Gl.multisamplesamples 4);
      ignore
        (Sdl.gl_set_attribute Sdl.Gl.context_profile_mask
           Sdl.Gl.context_profile_es);
      ignore (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
      ignore (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 0);

      match Sdl.create_window "CLC Viewer" ~w:1000 ~h:800 Sdl.Window.opengl with
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

          (* Compile Shaders *)
          let vs = compile_shader Gl.vertex_shader vertex_shader_src in
          let fs = compile_shader Gl.fragment_shader fragment_shader_src in
          let prog = create_program vs fs in

          let m_vs = compile_shader Gl.vertex_shader marker_vs_src in
          let m_fs = compile_shader Gl.fragment_shader marker_fs_src in
          let m_prog = create_program m_vs m_fs in

          let w_vs = compile_shader Gl.vertex_shader water_vs_src in
          let w_prog = create_program w_vs fs in

          (* Parse Args *)
          let file_ref = ref "data/clc/N45E006.clc" in
          let mark_pos = ref None in

          let rec parse_args i =
            if i >= Array.length Sys.argv then ()
            else
              match Sys.argv.(i) with
              | "-mark" ->
                  if i + 2 < Array.length Sys.argv then (
                    let lat = float_of_string Sys.argv.(i + 1) in
                    let lon = float_of_string Sys.argv.(i + 2) in
                    mark_pos := Some (lon, lat);
                    parse_args (i + 3))
                  else failwith "Missing coordinates for -mark"
              | arg ->
                  if arg.[0] <> '-' then file_ref := arg;
                  parse_args (i + 1)
          in
          parse_args 1;
          let file = !file_ref in
          let ( index_count,
                data_pos,
                data_col,
                data_ebo,
                t_min_x,
                t_min_y,
                t_scale_x,
                t_scale_y,
                water_index_count,
                water_data_pos,
                water_data_col,
                water_data_ebo,
                water_scale_x,
                _,
                pois ) =
            load_clc file
          in

          (* Calculate range from scale: scale = 65535 / range => range = 65535 / scale *)
          let range_x = 65535.0 /. t_scale_x in
          let range_y = 65535.0 /. t_scale_y in

          (* CLC Uniforms - also used for water since coords are scaled to same range *)
          Gl.use_program prog;
          let u_range = Gl.get_uniform_location prog "u_range" in
          let u_min = Gl.get_uniform_location prog "u_min" in
          let u_view_scale = Gl.get_uniform_location prog "u_view_scale" in
          let u_view_offset = Gl.get_uniform_location prog "u_view_offset" in
          let u_palette = Gl.get_uniform_location prog "u_palette" in

          Gl.use_program m_prog;
          let m_u_pos = Gl.get_uniform_location m_prog "u_pos" in
          let m_u_color = Gl.get_uniform_location m_prog "u_color" in
          let m_u_view_scale = Gl.get_uniform_location m_prog "u_view_scale" in
          let m_u_view_offset =
            Gl.get_uniform_location m_prog "u_view_offset"
          in

          (* Water Uniforms *)
          Gl.use_program w_prog;
          let w_u_range = Gl.get_uniform_location w_prog "u_water_range" in
          let w_u_scale = Gl.get_uniform_location w_prog "u_water_scale" in
          let w_u_min = Gl.get_uniform_location w_prog "u_min" in
          let w_u_view_scale = Gl.get_uniform_location w_prog "u_view_scale" in
          let w_u_view_offset =
            Gl.get_uniform_location w_prog "u_view_offset"
          in
          let w_u_palette = Gl.get_uniform_location w_prog "u_palette" in

          let palette_data = create_palette_texture () in
          let viewer_palette_data = create_viewer_palette_texture () in
          let texs = Array1.create int32 c_layout 2 in
          Gl.gen_textures 2 texs;

          let setup_texture tid data =
            Gl.bind_texture Gl.texture_2d tid;
            Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
            Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
            Gl.tex_image2d Gl.texture_2d 0 Gl.rgb8 256 1 0 Gl.rgb
              Gl.unsigned_byte (`Data data)
          in

          setup_texture (Int32.to_int (Array1.get texs 0)) palette_data;
          setup_texture (Int32.to_int (Array1.get texs 1)) viewer_palette_data;

          Gl.active_texture Gl.texture0;
          Gl.bind_texture Gl.texture_2d (Int32.to_int (Array1.get texs 0));
          Gl.uniform1i u_palette 0;

          (* CLC VAO/VBOs *)
          let vaos = Array1.create int32 c_layout 2 in
          Gl.gen_vertex_arrays 2 vaos;

          (* CLC VAO *)
          Gl.bind_vertex_array (Int32.to_int (Array1.get vaos 0));

          let vbos = Array1.create int32 c_layout 6 in
          Gl.gen_buffers 6 vbos;
          let vbo_pos = Int32.to_int (Array1.get vbos 0) in
          let vbo_col = Int32.to_int (Array1.get vbos 1) in
          let vbo_ebo = Int32.to_int (Array1.get vbos 2) in
          let w_vbo_pos = Int32.to_int (Array1.get vbos 3) in
          let w_vbo_col = Int32.to_int (Array1.get vbos 4) in
          let w_vbo_ebo = Int32.to_int (Array1.get vbos 5) in

          Gl.bind_buffer Gl.array_buffer vbo_pos;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size data_pos)
            (Some data_pos) Gl.static_draw;
          Gl.enable_vertex_attrib_array 0;
          Gl.vertex_attrib_pointer 0 2 Gl.unsigned_short true 0 (`Offset 0);

          Gl.bind_buffer Gl.array_buffer vbo_col;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size data_col)
            (Some data_col) Gl.static_draw;
          Gl.enable_vertex_attrib_array 1;
          Gl.vertex_attrib_ipointer 1 1 Gl.unsigned_byte 0 (`Offset 0);

          Gl.bind_buffer Gl.element_array_buffer vbo_ebo;
          Gl.buffer_data Gl.element_array_buffer
            (Gl.bigarray_byte_size data_ebo)
            (Some data_ebo) Gl.static_draw;

          (* Water VAO *)
          Gl.bind_vertex_array (Int32.to_int (Array1.get vaos 1));

          Gl.bind_buffer Gl.array_buffer w_vbo_pos;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size water_data_pos)
            (Some water_data_pos) Gl.static_draw;
          Gl.enable_vertex_attrib_array 0;
          (* Use int32 for full 24-bit water precision *)
          Gl.vertex_attrib_ipointer 0 2 Gl.int 0 (`Offset 0);
          Gl.bind_buffer Gl.array_buffer w_vbo_col;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size water_data_col)
            (Some water_data_col) Gl.static_draw;
          Gl.enable_vertex_attrib_array 1;
          Gl.vertex_attrib_ipointer 1 1 Gl.unsigned_byte 0 (`Offset 0);

          Gl.bind_buffer Gl.element_array_buffer w_vbo_ebo;
          Gl.buffer_data Gl.element_array_buffer
            (Gl.bigarray_byte_size water_data_ebo)
            (Some water_data_ebo) Gl.static_draw;

          (* Interaction State *)
          let zoom = ref 1.0 in
          let cx = ref (t_min_x +. (range_x /. 2.0)) in
          let cy = ref (t_min_y +. (range_y /. 2.0)) in
          let drag = ref false in
          let use_viewer_palette = ref false in
          let w_width, w_height = (1000, 800) in

          Gl.enable Gl.depth_test;
          Gl.clear_color 0.1 0.1 0.1 1.0;

          let rec loop () =
            let e = Sdl.Event.create () in
            let rec drain () =
              if Sdl.poll_event (Some e) then (
                (match Sdl.Event.(enum (get e typ)) with
                | `Quit -> exit 0
                | `Key_down ->
                    let key = Sdl.Event.(get e keyboard_keycode) in
                    if key = Sdl.K.escape then exit 0
                    else if key = Sdl.K.p then
                      use_viewer_palette := not !use_viewer_palette
                | `Mouse_wheel ->
                    let y = Sdl.Event.(get e mouse_wheel_y) in
                    zoom := !zoom *. if y > 0 then 1.1 else 0.9
                | `Mouse_button_down -> drag := true
                | `Mouse_button_up -> drag := false
                | `Mouse_motion ->
                    if !drag then (
                      let dx = float Sdl.Event.(get e mouse_motion_xrel) in
                      let dy = float Sdl.Event.(get e mouse_motion_yrel) in
                      cx := !cx -. (dx /. float w_width *. range_x /. !zoom);
                      cy := !cy +. (dy /. float w_height *. range_y /. !zoom))
                | _ -> ());
                drain ())
            in
            drain ();

            Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

            let center_lat = t_min_y +. (range_y /. 2.0) in
            let lat_correction = cos (center_lat *. Float.pi /. 180.0) in

            let aspect = float w_width /. float w_height in
            let corrected_range_x = range_x *. lat_correction in
            let sx, sy =
              if corrected_range_x > range_y *. aspect then
                ( !zoom /. corrected_range_x,
                  !zoom /. corrected_range_x *. aspect )
              else
                (!zoom /. range_y /. aspect *. lat_correction, !zoom /. range_y)
            in

            (* Setup shader uniforms once *)
            Gl.use_program prog;
            Gl.active_texture Gl.texture0;
            let current_tex =
              Int32.to_int
                (Array1.get texs (if !use_viewer_palette then 1 else 0))
            in
            Gl.bind_texture Gl.texture_2d current_tex;
            Gl.uniform1i u_palette 0;
            Gl.uniform2f u_range range_x range_y;
            Gl.uniform2f u_min t_min_x t_min_y;
            Gl.uniform2f u_view_scale sx sy;
            Gl.uniform2f u_view_offset !cx !cy;

            (* Draw Water Layer FIRST (topmost - uses depth buffer for overdraw avoidance) *)
            if water_index_count > 0 then begin
              Gl.use_program w_prog;
              Gl.active_texture Gl.texture0;
              Gl.bind_texture Gl.texture_2d current_tex;
              Gl.uniform1i w_u_palette 0;
              Gl.uniform2f w_u_range range_x range_y;
              (* [water_scale_x] is quanta per degree; the shader wants
                 quanta per tile range *)
              Gl.uniform1f w_u_scale (range_x *. water_scale_x);
              Gl.uniform2f w_u_min t_min_x t_min_y;
              Gl.uniform2f w_u_view_scale sx sy;
              Gl.uniform2f w_u_view_offset !cx !cy;

              Gl.bind_vertex_array (Int32.to_int (Array1.get vaos 1));
              Gl.draw_elements Gl.triangles water_index_count Gl.unsigned_int
                (`Offset 0)
            end;

            (* Draw CLC Map (underneath water) *)
            Gl.use_program prog;
            Gl.bind_vertex_array (Int32.to_int (Array1.get vaos 0));
            Gl.draw_elements Gl.triangles index_count Gl.unsigned_int
              (`Offset 0);

            (* Draw Marker *)
            (match !mark_pos with
            | Some (mx, my) ->
                Gl.disable Gl.depth_test;
                Gl.use_program m_prog;
                Gl.uniform3f m_u_color 1.0 1.0 0.0;
                Gl.uniform2f m_u_pos mx my;
                Gl.uniform2f m_u_view_scale sx sy;
                Gl.uniform2f m_u_view_offset !cx !cy;
                Gl.draw_arrays Gl.points 0 1;
                Gl.enable Gl.depth_test
            | None -> ());

            (* Draw POI Points - red for peaks, blue for saddles *)
            if List.length pois > 0 then begin
              Gl.disable Gl.depth_test;
              Gl.use_program m_prog;
              Gl.uniform2f m_u_view_scale sx sy;
              Gl.uniform2f m_u_view_offset !cx !cy;
              List.iter
                (fun poi ->
                  (* Red for peaks, blue for saddles *)
                  (match poi.poi_type with
                  | Peak -> Gl.uniform3f m_u_color 1.0 0.0 0.0
                  | Saddle -> Gl.uniform3f m_u_color 0.0 0.5 1.0);
                  Gl.uniform2f m_u_pos poi.lon poi.lat;
                  Gl.draw_arrays Gl.points 0 1)
                pois;
              Gl.enable Gl.depth_test
            end;

            Sdl.gl_swap_window window;
            loop ()
          in
          loop ())
