(* render_clc.ml *)
(* Compile command:
   ocamlfind ocamlopt -package tsdl,tgles3,sqlite3,str,bigarray -linkpkg wkb_decode.ml earcut.ml render_clc.ml -o render_clc
*)

open Tsdl
open Tgles3
open Bigarray

(* --- 1. Palette Definition --- *)
let raw_palette =
  [
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
    (fun i (code, _, _, _) -> Hashtbl.add code_map code (i + 1))
    raw_palette

let get_code_index code =
  match Hashtbl.find_opt code_map code with Some i -> i | None -> 0

let create_palette_texture () =
  let buf = Array1.create int8_unsigned c_layout (256 * 3) in
  Array1.set buf 0 128;
  Array1.set buf 1 128;
  Array1.set buf 2 128;
  List.iteri
    (fun i (_, r, g, b) ->
      let idx = (i + 1) * 3 in
      Array1.set buf idx r;
      Array1.set buf (idx + 1) g;
      Array1.set buf (idx + 2) b)
    raw_palette;
  buf

(* --- 2. Shaders --- *)
let vertex_shader_src =
  "\n\
   #version 300 es\n\
   layout(location = 0) in vec2 in_norm_pos;\n\
   layout(location = 1) in int in_color_idx;\n\
   uniform vec2 u_mult;\n\
   uniform vec2 u_add;\n\
   flat out int v_idx;\n\
   void main() {\n\
  \    vec2 pos = in_norm_pos * u_mult + u_add;\n\
  \    gl_Position = vec4(pos, 0.0, 1.0);\n\
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

(* --- 3. Helpers --- *)
let get_wkb_from_gpkg_blob blob_str =
  let len = String.length blob_str in
  if len < 8 || String.sub blob_str 0 2 <> "GP" then None
  else
    let flags = Char.code blob_str.[3] in
    let env = (flags lsr 1) land 0x07 in
    let header_len =
      match env with 1 -> 40 | 2 | 3 -> 56 | 4 -> 72 | _ -> 8
    in
    if len < header_len then None
    else Some (String.sub blob_str header_len (len - header_len))

let get_approx_area geom =
  let bbox = Wkb_decode.get_bbox geom in
  let w = bbox.Wkb_decode.max_x -. bbox.Wkb_decode.min_x in
  let h = bbox.Wkb_decode.max_y -. bbox.Wkb_decode.min_y in
  w *. h

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
    Printf.printf "Shader: %s\n"
      (get_log Gl.get_shaderiv Gl.get_shader_info_log s);
    exit 1);
  s

let create_program vs fs =
  let p = Gl.create_program () in
  Gl.attach_shader p vs;
  Gl.attach_shader p fs;
  Gl.link_program p;
  if get_iv Gl.get_programiv p Gl.link_status = Gl.false_ then (
    Printf.printf "Link: %s\n"
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
      ignore (Sdl.gl_set_attribute Sdl.Gl.depth_size 24);

      let w_width, w_height = (1000, 800) in
      match
        Sdl.create_window "CLC Viewer (Robust Earcut)" ~w:w_width ~h:w_height
          Sdl.Window.opengl
      with
      | Error (`Msg e) ->
          Printf.printf "%s\n" e;
          exit 1
      | Ok window ->
          let _ctx =
            match Sdl.gl_create_context window with
            | Error _ -> exit 1
            | Ok c -> c
          in
          ignore (Sdl.gl_set_swap_interval 1);

          (* CHANGED: Cache name for Earcut version *)
          let cache_file = "clc_earcut.cache" in
          let db_file = "clc2018-R93.gpkg" in

          let min_x, min_y, max_x, max_y =
            (ref 0., ref 0., ref 100., ref 100.)
          in

          let index_count, data_pos, data_idx, data_ebo =
            if Sys.file_exists cache_file then (
              Printf.printf "Loading cache %s...\n%!" cache_file;
              let ic = open_in_bin cache_file in
              min_x := input_value ic;
              min_y := input_value ic;
              max_x := input_value ic;
              max_y := input_value ic;
              let v_count : int = input_value ic in
              let d_pos : (int, int16_unsigned_elt, c_layout) Array1.t =
                input_value ic
              in
              let d_idx : (int, int8_unsigned_elt, c_layout) Array1.t =
                input_value ic
              in
              let i_count : int = input_value ic in
              let d_ebo : (int32, int32_elt, c_layout) Array1.t =
                input_value ic
              in
              close_in ic;
              Printf.printf "Cache loaded (%d indices).\n%!" i_count;
              (i_count, d_pos, d_idx, d_ebo))
            else (
              Printf.printf "Processing GeoPackage...\n%!";
              min_x := infinity;
              min_y := infinity;
              max_x := neg_infinity;
              max_y := neg_infinity;

              let db = Sqlite3.db_open db_file in
              let sql_meta =
                "SELECT table_name, column_name FROM gpkg_geometry_columns \
                 LIMIT 1"
              in
              let stmt = Sqlite3.prepare db sql_meta in
              let table, geom_col =
                match Sqlite3.step stmt with
                | Sqlite3.Rc.ROW ->
                    ( Sqlite3.Data.to_string_exn (Sqlite3.column stmt 0),
                      Sqlite3.Data.to_string_exn (Sqlite3.column stmt 1) )
                | _ -> failwith "No table"
              in
              ignore (Sqlite3.finalize stmt);

              Printf.printf "Buffering & Calculating BBox...\n%!";
              let feature_list = ref [] in
              let sql_query =
                Printf.sprintf "SELECT Code_18, %s FROM %s" geom_col table
              in
              let stmt_query = Sqlite3.prepare db sql_query in

              let rec fetch () =
                match Sqlite3.step stmt_query with
                | Sqlite3.Rc.ROW ->
                    let code =
                      Sqlite3.Data.to_string_exn (Sqlite3.column stmt_query 0)
                    in
                    (match Sqlite3.column stmt_query 1 with
                    | Sqlite3.Data.BLOB raw -> (
                        match get_wkb_from_gpkg_blob raw with
                        | Some wkb -> (
                            match Wkb_decode.decode_wkb wkb with
                            | Some geom ->
                                let b = Wkb_decode.get_bbox geom in
                                if b.Wkb_decode.min_x < !min_x then
                                  min_x := b.Wkb_decode.min_x;
                                if b.Wkb_decode.min_y < !min_y then
                                  min_y := b.Wkb_decode.min_y;
                                if b.Wkb_decode.max_x > !max_x then
                                  max_x := b.Wkb_decode.max_x;
                                if b.Wkb_decode.max_y > !max_y then
                                  max_y := b.Wkb_decode.max_y;
                                let area =
                                  (b.Wkb_decode.max_x -. b.Wkb_decode.min_x)
                                  *. (b.Wkb_decode.max_y -. b.Wkb_decode.min_y)
                                in
                                feature_list :=
                                  (area, code, geom) :: !feature_list
                            | _ -> ())
                        | _ -> ())
                    | _ -> ());
                    fetch ()
                | _ -> ()
              in
              fetch ();
              ignore (Sqlite3.finalize stmt_query);

              Printf.printf "Global Bounds: x[%.2f, %.2f] y[%.2f, %.2f]\n%!"
                !min_x !max_x !min_y !max_y;
              Printf.printf "Sorting...\n%!";
              let sorted =
                List.sort
                  (fun (a1, _, _) (a2, _, _) -> compare a1 a2)
                  !feature_list
              in

              Printf.printf
                "Quantizing, Cleaning & Triangulating (Earcut)...\n%!";
              let lst_pos = ref [] in
              let lst_col = ref [] in
              let v_count = ref 0 in
              let lst_ebo = ref [] in
              let i_count = ref 0 in

              let range_x = !max_x -. !min_x in
              let range_y = !max_y -. !min_y in

              List.iter
                (fun (_, code, geom) ->
                  let idx = get_code_index code in
                  let quant v min_v range =
                    int_of_float ((v -. min_v) /. range *. 65535.)
                  in

                  (* Robust Processing Function *)
                  let process_poly rings =
                    (* 1. Pre-process Rings: Quantize, Dedup, Close *)
                    let valid_rings =
                      List.fold_left
                        (fun acc r ->
                          (* A. Quantize *)
                          let q_pts =
                            List.map
                              (fun p ->
                                ( quant p.Wkb_decode.x !min_x range_x,
                                  quant p.Wkb_decode.y !min_y range_y ))
                              r
                          in

                          (* B. Deduplicate Adjacent *)
                          let rec dedup out = function
                            | [] -> List.rev out
                            | (x, y) :: tl -> (
                                match out with
                                | (lx, ly) :: _ when lx = x && ly = y ->
                                    dedup out tl
                                | _ -> dedup ((x, y) :: out) tl)
                          in
                          let unique = dedup [] q_pts in

                          (* C. Check Validity (at least 3 points) *)
                          match unique with
                          | [] -> acc
                          | hd :: _ ->
                              (* D. Handle Closed Rings (remove last if same as first) *)
                              let rec remove_closing = function
                                | [] -> []
                                | [ last ] -> if last = hd then [] else [ last ]
                                | h :: t -> h :: remove_closing t
                              in
                              let clean = remove_closing unique in

                              if List.length clean >= 3 then clean :: acc
                              else acc)
                        [] rings
                    in
                    (* Ensure list order corresponds to Outer -> Holes (fold_left reverses, so we reverse back) *)
                    let valid_rings = List.rev valid_rings in

                    (* 2. Triangulate if Outer ring exists *)
                    match valid_rings with
                    | [] -> ()
                    | _ ->
                        (* Prepare input for Earcut: Convert integer points to Earcut.point floats *)
                        let e_rings =
                          List.map
                            (fun r ->
                              List.map
                                (fun (x, y) ->
                                  {
                                    Earcut.x = float_of_int x;
                                    y = float_of_int y;
                                  })
                                r)
                            valid_rings
                        in

                        (* CALL EARCUT *)
                        let indices = Earcut.triangulate e_rings in

                        (* 3. Store Results *)
                        let base_index = !v_count in

                        (* Earcut indices refer to the flattened vertex list. 
                   We must iterate the rings in the exact same order to populate VBO. *)
                        List.iter
                          (fun r ->
                            List.iter
                              (fun (x, y) ->
                                lst_pos := y :: x :: !lst_pos;
                                lst_col := idx :: !lst_col;
                                incr v_count)
                              r)
                          valid_rings;

                        List.iter
                          (fun i ->
                            lst_ebo := (base_index + i) :: !lst_ebo;
                            incr i_count)
                          indices
                  in

                  match geom with
                  | Wkb_decode.Polygon rings -> process_poly rings
                  | Wkb_decode.MultiPolygon polys ->
                      List.iter process_poly polys
                  | _ -> ())
                sorted;

              let arr_pos =
                Array1.create int16_unsigned c_layout (!v_count * 2)
              in
              let arr_col = Array1.create int8_unsigned c_layout !v_count in
              let rec fill_p idx lst =
                match lst with
                | [] -> ()
                | h :: t ->
                    Array1.set arr_pos idx h;
                    fill_p (idx - 1) t
              in
              fill_p ((!v_count * 2) - 1) !lst_pos;
              let rec fill_c idx lst =
                match lst with
                | [] -> ()
                | h :: t ->
                    Array1.set arr_col idx h;
                    fill_c (idx - 1) t
              in
              fill_c (!v_count - 1) !lst_col;
              let arr_ebo = Array1.create int32 c_layout !i_count in
              let rec fill_e idx lst =
                match lst with
                | [] -> ()
                | h :: t ->
                    Array1.set arr_ebo idx (Int32.of_int h);
                    fill_e (idx - 1) t
              in
              fill_e (!i_count - 1) !lst_ebo;

              Printf.printf "Saving...\n%!";
              let oc = open_out_bin cache_file in
              output_value oc !min_x;
              output_value oc !min_y;
              output_value oc !max_x;
              output_value oc !max_y;
              output_value oc !v_count;
              output_value oc arr_pos;
              output_value oc arr_col;
              output_value oc !i_count;
              output_value oc arr_ebo;
              close_out oc;
              (!i_count, arr_pos, arr_col, arr_ebo))
          in

          let vs = compile_shader Gl.vertex_shader vertex_shader_src in
          let fs = compile_shader Gl.fragment_shader fragment_shader_src in
          let prog = create_program vs fs in
          Gl.use_program prog;

          let palette_data = create_palette_texture () in
          let texs = Array1.create int32 c_layout 1 in
          Gl.gen_textures 1 texs;
          let tex_id = Int32.to_int (Array1.get texs 0) in
          Gl.active_texture Gl.texture0;
          Gl.bind_texture Gl.texture_2d tex_id;
          Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
          Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
          Gl.tex_image2d Gl.texture_2d 0 Gl.rgb8 256 1 0 Gl.rgb Gl.unsigned_byte
            (`Data palette_data);
          Gl.uniform1i (Gl.get_uniform_location prog "u_palette") 0;

          let vaos = Array1.create int32 c_layout 1 in
          Gl.gen_vertex_arrays 1 vaos;
          let vao = Int32.to_int (Array1.get vaos 0) in
          Gl.bind_vertex_array vao;
          let vbos = Array1.create int32 c_layout 3 in
          Gl.gen_buffers 3 vbos;
          let vbo_pos, vbo_col, ebo_id =
            ( Int32.to_int (Array1.get vbos 0),
              Int32.to_int (Array1.get vbos 1),
              Int32.to_int (Array1.get vbos 2) )
          in

          Gl.bind_buffer Gl.array_buffer vbo_pos;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size data_pos)
            (Some data_pos) Gl.static_draw;
          Gl.enable_vertex_attrib_array 0;
          Gl.vertex_attrib_pointer 0 2 Gl.unsigned_short true 0 (`Offset 0);

          Gl.bind_buffer Gl.array_buffer vbo_col;
          Gl.buffer_data Gl.array_buffer
            (Gl.bigarray_byte_size data_idx)
            (Some data_idx) Gl.static_draw;
          Gl.enable_vertex_attrib_array 1;
          Gl.vertex_attrib_ipointer 1 1 Gl.unsigned_byte 0 (`Offset 0);

          Gl.bind_buffer Gl.element_array_buffer ebo_id;
          Gl.buffer_data Gl.element_array_buffer
            (Gl.bigarray_byte_size data_ebo)
            (Some data_ebo) Gl.static_draw;

          let zoom = ref 1.0 in
          let cx, cy =
            (ref ((!min_x +. !max_x) /. 2.), ref ((!min_y +. !max_y) /. 2.))
          in
          let range_x, range_y = (!max_x -. !min_x, !max_y -. !min_y) in
          let aspect = float w_width /. float w_height in
          let base_sx, base_sy =
            if range_x > range_y *. aspect then
              (2. /. range_x, 2. /. range_x *. aspect)
            else (2. /. range_y /. aspect, 2. /. range_y)
          in

          let u_mult, u_add =
            ( Gl.get_uniform_location prog "u_mult",
              Gl.get_uniform_location prog "u_add" )
          in
          let drag = ref false in

          Gl.enable Gl.depth_test;
          Gl.depth_func Gl.less;

          Printf.printf
            "Controls: Scroll/Pinch to Zoom, Drag to Pan, Space to Reset\n%!";

          let rec loop () =
            let e = Sdl.Event.create () in
            match Sdl.wait_event (Some e) with
            | Error _ -> exit 1
            | Ok () ->
                let rec process_event e =
                  match Sdl.Event.(enum (get e typ)) with
                  | `Quit -> exit 0
                  | `Mouse_wheel ->
                      let y = Sdl.Event.(get e mouse_wheel_y) in
                      zoom := !zoom *. if y > 0 then 1.1 else 0.9
                  | `Multi_gesture ->
                      zoom :=
                        !zoom
                        *. (1.0
                           +. (Sdl.Event.(get e multi_gesture_ddist) *. 2.0))
                  | `Mouse_button_down -> drag := true
                  | `Mouse_button_up -> drag := false
                  | `Mouse_motion ->
                      if !drag then (
                        let dx = float Sdl.Event.(get e mouse_motion_xrel)
                        and dy = float Sdl.Event.(get e mouse_motion_yrel) in
                        cx :=
                          !cx
                          -. (dx /. float w_width *. 2.0 /. (base_sx *. !zoom));
                        cy :=
                          !cy
                          -. -.dy /. float w_height *. 2.0 /. (base_sy *. !zoom))
                  | `Key_down -> (
                      match Sdl.Event.(get e keyboard_keycode) with
                      | k when k = Sdl.K.escape -> exit 0
                      | k when k = Sdl.K.space ->
                          zoom := 1.0;
                          cx := (!min_x +. !max_x) /. 2.;
                          cy := (!min_y +. !max_y) /. 2.
                      | _ -> ())
                  | _ -> ()
                in
                process_event e;
                let rec drain () =
                  if Sdl.poll_event (Some e) then (
                    process_event e;
                    drain ())
                in
                drain ();

                let sx, sy = (base_sx *. !zoom, base_sy *. !zoom) in
                Gl.uniform2f u_mult (range_x *. sx) (range_y *. sy);
                Gl.uniform2f u_add
                  ((!min_x -. !cx) *. sx)
                  ((!min_y -. !cy) *. sy);

                Gl.clear_color 0.1 0.1 0.1 1.0;
                Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
                Gl.draw_elements Gl.triangles index_count Gl.unsigned_int
                  (`Offset 0);
                Sdl.gl_swap_window window;
                loop ()
          in
          loop ())
