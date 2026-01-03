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

(* --- Marker Shaders --- *)
let marker_vs_src =
  "\n\
   #version 300 es\n\
   uniform vec2 u_pos;\n\
   uniform vec2 u_view_scale;\n\
   uniform vec2 u_view_offset;\n\
   void main() {\n\
  \    vec2 screen_pos = (u_pos - u_view_offset) * u_view_scale * 2.0;\n\
  \    gl_Position = vec4(screen_pos, 0.0, 1.0);\n\
  \    gl_PointSize = 15.0;\n\
   }\n"

let marker_fs_src =
  "\n\
   #version 300 es\n\
   precision mediump float;\n\
   out vec4 out_color;\n\
   void main() { out_color = vec4(1.0, 0.0, 0.0, 1.0); }\n"

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
  if magic <> "CLC3" then failwith "Invalid magic";

  let count = input_binary_int ic in
  let total_verts_header = input_binary_int ic in
  let total_indices_header = input_binary_int ic in

  let min_lon = read_f64_as_float ic in
  let min_lat = read_f64_as_float ic in
  let scale_x = read_f64_as_float ic in
  let scale_y = read_f64_as_float ic in

  Printf.printf
    "Header: count=%d, verts=%d, indices=%d, origin=(%.2f, %.2f)\n%!" count
    total_verts_header total_indices_header min_lon min_lat;

  (* 
     Pre-allocate Bigarrays:
     The format header gives us exact total vertex and index counts.
     We allocate the final GL buffers (Bigarrays) immediately.
     This allows "Direct Decoding" where we write decompressed values straight into these arrays,
     avoiding the massive GC pressure of creating millions of intermediate OCaml tuples/lists.
  *)
  let n_verts = total_verts_header in
  let n_indices = total_indices_header in
  let arr_pos = Array1.create int16_unsigned c_layout (n_verts * 2) in
  let arr_col = Array1.create int8_unsigned c_layout n_verts in
  let arr_ebo = Array1.create int32 c_layout n_indices in

  let unknown_codes = Hashtbl.create 5 in

  let read_stream () =
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let b2 = input_byte ic in
    let b3 = input_byte ic in
    let comp_len = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
    let compressed = really_input_string ic comp_len in

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
    in
    uncompress_string compressed
  in

  Printf.printf "Reading Global Streams...\n%!";
  let meta_str = read_stream () in
  let high_x = read_stream () in
  let low_x = read_stream () in
  let high_y = read_stream () in
  let low_y = read_stream () in
  let high_indices = read_stream () in
  let low_indices = read_stream () in
  close_in ic;
  Printf.printf "Streams Read. Decoding directly to Bigarrays...\n%!";

  let prev_x = ref 0 in
  let prev_y = ref 0 in
  let prev_idx = ref 0 in
  let global_v_offset = ref 0 in
  let global_i_offset = ref 0 in

  (* 
     ZigZag Decoding:
     Maps positive integers back to signed integers.
     Evens -> Positive, Odds -> Negative.
     decode(n) = (n >> 1) ^ -(n & 1)
     e.g. 0->0, 1->-1, 2->1, 3->-2
  *)
  let zigzag_decode n = (n lsr 1) lxor -(n land 1) in

  let meta_pos = ref 0 in
  let v_pos = ref 0 in
  let i_pos = ref 0 in

  let read_u16_meta () =
    let b0 = Char.code meta_str.[!meta_pos] in
    let b1 = Char.code meta_str.[!meta_pos + 1] in
    meta_pos := !meta_pos + 2;
    b0 lor (b1 lsl 8)
  in

  for i = 1 to count do
    let code = read_u16_meta () in
    let v_count = read_u16_meta () in
    let t_count = read_u16_meta () in

    let code_idx = get_code_index code in

    if not (Hashtbl.mem code_map code) then
      if not (Hashtbl.mem unknown_codes code) then (
        Printf.printf "Warning: Unknown CLC Code %d \n" code;
        Hashtbl.add unknown_codes code true);

    (* Decode Vertices *)
    prev_x := 0;
    prev_y := 0;
    let base_v = !global_v_offset in

    for k = 0 to v_count - 1 do
      let idx = !v_pos + k in

      let hx = Char.code high_x.[idx] in
      let lx = Char.code low_x.[idx] in
      let zx = lx lor (hx lsl 8) in
      let sdx = zigzag_decode zx in
      (* Modular reconstruction: (prev + diff) mod 65536 *)
      let qx = (!prev_x + sdx) land 0xFFFF in
      prev_x := qx;

      let hy = Char.code high_y.[idx] in
      let ly = Char.code low_y.[idx] in
      let zy = ly lor (hy lsl 8) in
      let sdy = zigzag_decode zy in
      let qy = (!prev_y + sdy) land 0xFFFF in
      prev_y := qy;

      (* Write direct to Bigarray *)
      let out_idx = base_v + k in
      Array1.set arr_pos (out_idx * 2) qx;
      Array1.set arr_pos ((out_idx * 2) + 1) qy;
      Array1.set arr_col out_idx code_idx
    done;

    v_pos := !v_pos + v_count;
    global_v_offset := !global_v_offset + v_count;

    (* Decode Indices *)
    prev_idx := 0;
    let num_indices = t_count * 3 in

    for k = 0 to num_indices - 1 do
      let idx = !i_pos + k in
      let hi = Char.code high_indices.[idx] in
      let li = Char.code low_indices.[idx] in
      let zi = li lor (hi lsl 8) in
      let sdi = zigzag_decode zi in
      let idx_val = (!prev_idx + sdi) land 0xFFFF in
      prev_idx := idx_val;

      let final_idx = base_v + idx_val in
      Array1.set arr_ebo (!global_i_offset + k) (Int32.of_int final_idx)
    done;
    i_pos := !i_pos + num_indices;
    global_i_offset := !global_i_offset + num_indices
  done;

  Printf.printf "Decoded: %d verts, %d indices\n%!" !global_v_offset
    !global_i_offset;

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
                t_scale_y ) =
            load_clc file
          in

          (* Calculate range from scale: scale = 65535 / range => range = 65535 / scale *)
          let range_x = 65535.0 /. t_scale_x in
          let range_y = 65535.0 /. t_scale_y in

          (* Compile Shaders *)
          let vs = compile_shader Gl.vertex_shader vertex_shader_src in
          let fs = compile_shader Gl.fragment_shader fragment_shader_src in
          let prog = create_program vs fs in

          let m_vs = compile_shader Gl.vertex_shader marker_vs_src in
          let m_fs = compile_shader Gl.fragment_shader marker_fs_src in
          let m_prog = create_program m_vs m_fs in

          (* Uniforms *)
          Gl.use_program prog;
          let u_range = Gl.get_uniform_location prog "u_range" in
          let u_min = Gl.get_uniform_location prog "u_min" in
          let u_view_scale = Gl.get_uniform_location prog "u_view_scale" in
          let u_view_offset = Gl.get_uniform_location prog "u_view_offset" in
          let u_palette = Gl.get_uniform_location prog "u_palette" in

          Gl.use_program m_prog;
          let m_u_pos = Gl.get_uniform_location m_prog "u_pos" in
          let m_u_view_scale = Gl.get_uniform_location m_prog "u_view_scale" in
          let m_u_view_offset =
            Gl.get_uniform_location m_prog "u_view_offset"
          in

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

            (* 
               Geographic correction: 1° longitude is shorter than 1° latitude
               by a factor of cos(latitude). We apply this to make the map
               appear with correct proportions.
            *)
            let center_lat = t_min_y +. (range_y /. 2.0) in
            let lat_correction = cos (center_lat *. Float.pi /. 180.0) in

            (* Apply correction to view scale, not to range (range is for data reconstruction) *)
            let aspect = float w_width /. float w_height in
            let corrected_range_x = range_x *. lat_correction in
            let sx, sy =
              if corrected_range_x > range_y *. aspect then
                ( !zoom /. corrected_range_x,
                  !zoom /. corrected_range_x *. aspect )
              else
                (!zoom /. range_y /. aspect *. lat_correction, !zoom /. range_y)
            in

            (* Draw Map *)
            Gl.use_program prog;
            Gl.active_texture Gl.texture0;
            Gl.bind_texture Gl.texture_2d (Int32.to_int (Array1.get texs 0));
            Gl.uniform1i u_palette 0;

            Gl.uniform2f u_range range_x range_y;
            Gl.uniform2f u_min t_min_x t_min_y;
            Gl.uniform2f u_view_scale sx sy;
            Gl.uniform2f u_view_offset !cx !cy;

            Gl.draw_elements Gl.triangles index_count Gl.unsigned_int
              (`Offset 0);

            (* Draw Marker *)
            (match !mark_pos with
            | Some (mx, my) ->
                Gl.disable Gl.depth_test;
                Gl.use_program m_prog;
                Gl.uniform2f m_u_pos mx my;
                Gl.uniform2f m_u_view_scale sx sy;
                Gl.uniform2f m_u_view_offset !cx !cy;
                (* Draw single point *)
                Gl.draw_arrays Gl.points 0 1;
                Gl.enable Gl.depth_test
            | None -> ());

            Sdl.gl_swap_window window;
            loop ()
          in
          loop ())
