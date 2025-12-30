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

(* --- 2. Shaders --- *)
let vertex_shader_src =
  "\n\
   #version 300 es\n\
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
  "\n\
   #version 300 es\n\
   precision mediump float;\n\
   flat in int v_idx;\n\
   uniform mediump sampler2D u_palette;\n\
   out vec4 out_color;\n\
   void main() { out_color = texelFetch(u_palette, ivec2(v_idx, 0), 0); }\n"

(* --- 3. CLC Loading --- *)

type tile_header = {
  count : int;
  min_lon : float;
  min_lat : float;
  scale_x : float;
  scale_y : float;
}

(* Helper to read f64 bits as float *)
let read_f64_as_float ic =
  let b0 = Int64.of_int (input_byte ic) in
  let b1 = Int64.of_int (input_byte ic) in
  let b2 = Int64.of_int (input_byte ic) in
  let b3 = Int64.of_int (input_byte ic) in
  let b4 = Int64.of_int (input_byte ic) in
  let b5 = Int64.of_int (input_byte ic) in
  let b6 = Int64.of_int (input_byte ic) in
  let b7 = Int64.of_int (input_byte ic) in
  let bits =
    Int64.logor
      (Int64.logor
         (Int64.logor
            (Int64.logor
               (Int64.logor
                  (Int64.logor
                     (Int64.logor b0 (Int64.shift_left b1 8))
                     (Int64.shift_left b2 16))
                  (Int64.shift_left b3 24))
               (Int64.shift_left b4 32))
            (Int64.shift_left b5 40))
         (Int64.shift_left b6 48))
      (Int64.shift_left b7 56)
  in
  Int64.float_of_bits bits

let load_clc file =
  Printf.printf "Loading %s...\n%!" file;
  let ic = open_in_bin file in

  let magic = really_input_string ic 4 in
  if magic <> "CLC1" then failwith "Invalid magic";

  let count = input_binary_int ic in
  let min_lon = read_f64_as_float ic in
  let min_lat = read_f64_as_float ic in
  let scale_x = read_f64_as_float ic in
  let scale_y = read_f64_as_float ic in

  Printf.printf "Header: count=%d, origin=(%.2f, %.2f) scale=(%.2f, %.2f)\n%!"
    count min_lon min_lat scale_x scale_y;

  (* Buffers to accumulate geometry *)
  let all_pos = ref [] in
  (* (u16 * u16) list *)
  let all_col = ref [] in
  (* u8 list *)
  let all_idx = ref [] in
  (* int list *)
  let total_verts = ref 0 in

  let unknown_codes = Hashtbl.create 5 in

  for i = 1 to count do
    let start_pos = pos_in ic in
    (* Read Code: Changed to u16 *)
    let c0 = input_byte ic in
    let c1 = input_byte ic in
    let code = c0 lor (c1 lsl 8) in

    (* Read u32 counts *)
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let b2 = input_byte ic in
    let b3 = input_byte ic in
    let num_tris = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in

    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let b2 = input_byte ic in
    let b3 = input_byte ic in
    let num_verts = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in

    if i <= 30 then
      Printf.printf "Reading Feature %d at 0x%X: Code %d, Tris %d, Verts %d\n%!"
        i start_pos code num_tris num_verts;

    (* Check code *)
    if not (Hashtbl.mem code_map code) then
      if not (Hashtbl.mem unknown_codes code) then (
        Printf.printf
          "Warning: Unknown CLC Code %d (0x%X) at feature start 0x%X (feature \
           %d)\n"
          code code start_pos i;
        Hashtbl.add unknown_codes code true);

    (* Read streams *)
    let high_x = Bytes.create num_verts in
    let low_x = Bytes.create num_verts in
    let high_y = Bytes.create num_verts in
    let low_y = Bytes.create num_verts in

    really_input ic high_x 0 num_verts;
    really_input ic low_x 0 num_verts;
    really_input ic high_y 0 num_verts;
    really_input ic low_y 0 num_verts;

    let indices_bytes = Bytes.create (num_tris * 3 * 2) in
    really_input ic indices_bytes 0 (num_tris * 3 * 2);

    (* Decode Vertices *)
    let base_v = !total_verts in

    for i = 0 to num_verts - 1 do
      let hx = Char.code (Bytes.get high_x i) in
      let lx = Char.code (Bytes.get low_x i) in
      let qx = lx lor (hx lsl 8) in

      let hy = Char.code (Bytes.get high_y i) in
      let ly = Char.code (Bytes.get low_y i) in
      let qy = ly lor (hy lsl 8) in

      all_pos := (qx, qy) :: !all_pos;
      all_col := code :: !all_col
    done;

    total_verts := !total_verts + num_verts;

    (* Decode Indices *)
    for i = 0 to (num_tris * 3) - 1 do
      let b0 = Char.code (Bytes.get indices_bytes (i * 2)) in
      let b1 = Char.code (Bytes.get indices_bytes ((i * 2) + 1)) in
      let idx = b0 lor (b1 lsl 8) in
      all_idx := (base_v + idx) :: !all_idx
    done
  done;
  close_in ic;

  Printf.printf "Decoded: %d verts, %d indices\n%!" !total_verts
    (List.length !all_idx);

  (* Convert to Bigarrays *)
  let n_verts = !total_verts in
  let n_indices = List.length !all_idx in

  let arr_pos = Array1.create int16_unsigned c_layout (n_verts * 2) in
  let arr_col = Array1.create int8_unsigned c_layout n_verts in
  let arr_ebo = Array1.create int32 c_layout n_indices in

  (* Lists are reversed relative to processing order. Arrays should match. *)
  (* Filling from back restores order *)
  let rec fill_pos idx lst =
    match lst with
    | [] -> ()
    | (x, y) :: tl ->
        Array1.set arr_pos (idx - 1) y;
        (* Y at odd *)
        Array1.set arr_pos (idx - 2) x;
        (* X at even *)
        fill_pos (idx - 2) tl
  in
  fill_pos (n_verts * 2) !all_pos;

  let rec fill_col idx lst =
    match lst with
    | [] -> ()
    | c :: tl ->
        Array1.set arr_col (idx - 1) (get_code_index c);
        fill_col (idx - 1) tl
  in
  fill_col n_verts !all_col;

  let rec fill_idx idx lst =
    match lst with
    | [] -> ()
    | i :: tl ->
        Array1.set arr_ebo (idx - 1) (Int32.of_int i);
        fill_idx (idx - 1) tl
  in
  fill_idx n_indices !all_idx;

  (n_indices, arr_pos, arr_col, arr_ebo, min_lon, min_lat, scale_x, scale_y)

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

          (* Load Data *)
          let file =
            if Array.length Sys.argv > 1 then Sys.argv.(1)
            else "data/clc/N45E006.clc"
          in
          let ( index_count,
                data_pos,
                data_col,
                data_ebo,
                t_min_x,
                t_min_y,
                t_scale_x,
                t_scale_y ) =
            load_clc file
          in

          (* Calculate range from scale: scale = 65535 / range => range = 65535 / scale *)
          let range_x = 65535.0 /. t_scale_x in
          let range_y = 65535.0 /. t_scale_y in

          let vs = compile_shader Gl.vertex_shader vertex_shader_src in
          let fs = compile_shader Gl.fragment_shader fragment_shader_src in
          let prog = create_program vs fs in
          Gl.use_program prog;

          (* Uniforms *)
          let u_range = Gl.get_uniform_location prog "u_range" in
          let u_min = Gl.get_uniform_location prog "u_min" in
          let u_view_scale = Gl.get_uniform_location prog "u_view_scale" in
          let u_view_offset = Gl.get_uniform_location prog "u_view_offset" in
          let u_palette = Gl.get_uniform_location prog "u_palette" in

          (* Texture *)
          let palette_data = create_palette_texture () in
          let texs = Array1.create int32 c_layout 1 in
          Gl.gen_textures 1 texs;
          Gl.active_texture Gl.texture0;
          Gl.bind_texture Gl.texture_2d (Int32.to_int (Array1.get texs 0));
          Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
          Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
          Gl.tex_image2d Gl.texture_2d 0 Gl.rgb8 256 1 0 Gl.rgb Gl.unsigned_byte
            (`Data palette_data);
          Gl.uniform1i u_palette 0;

          (* Buffers *)
          let vaos = Array1.create int32 c_layout 1 in
          Gl.gen_vertex_arrays 1 vaos;
          Gl.bind_vertex_array (Int32.to_int (Array1.get vaos 0));

          let vbos = Array1.create int32 c_layout 3 in
          Gl.gen_buffers 3 vbos;
          let vbo_pos = Int32.to_int (Array1.get vbos 0) in
          let vbo_col = Int32.to_int (Array1.get vbos 1) in
          let vbo_ebo = Int32.to_int (Array1.get vbos 2) in

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

          (* Interaction State *)
          let zoom = ref 1.0 in
          let cx = ref (t_min_x +. (range_x /. 2.0)) in
          let cy = ref (t_min_y +. (range_y /. 2.0)) in
          let drag = ref false in
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
                    if Sdl.Event.(get e keyboard_keycode) = Sdl.K.escape then
                      exit 0
                | `Mouse_wheel ->
                    let y = Sdl.Event.(get e mouse_wheel_y) in
                    zoom := !zoom *. if y > 0 then 1.1 else 0.9
                | `Mouse_button_down -> drag := true
                | `Mouse_button_up -> drag := false
                | `Mouse_motion ->
                    if !drag then (
                      let dx = float Sdl.Event.(get e mouse_motion_xrel) in
                      let dy = float Sdl.Event.(get e mouse_motion_yrel) in
                      (* Adjust pan speed based on zoom *)
                      cx := !cx -. (dx /. float w_width *. range_x /. !zoom);
                      cy := !cy +. (dy /. float w_height *. range_y /. !zoom))
                | _ -> ());
                drain ())
            in
            drain ();

            Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

            let aspect = float w_width /. float w_height in
            let sx, sy =
              if range_x > range_y *. aspect then
                (!zoom /. range_x, !zoom /. range_x *. aspect)
              else (!zoom /. range_y /. aspect, !zoom /. range_y)
            in

            Gl.uniform2f u_range range_x range_y;
            Gl.uniform2f u_min t_min_x t_min_y;
            Gl.uniform2f u_view_scale sx sy;
            Gl.uniform2f u_view_offset !cx !cy;

            Gl.draw_elements Gl.triangles index_count Gl.unsigned_int
              (`Offset 0);

            Sdl.gl_swap_window window;
            loop ()
          in
          loop ())
