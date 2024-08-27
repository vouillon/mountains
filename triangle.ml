(*
- pass normal to fragment shader and compute color there
  (interpolated normal + fog)
- fog: use distance from origin
- perspective
  ==> we are at this position (lat, lon, height) => translate,
      looking in this direction, => rotate
      with this angle of vision. => perspective transform

http://www.songho.ca/opengl/gl_projectionmatrix.html
n = 10m (?)
n/t = 1/0.36
n/r = 1/0.36 / aspect ratio (viewport w/ viewport h)
*)

open Tsdl
open Tgles3
open Result

let str = Printf.sprintf
let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(* Helper functions. *)

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

module Proj3D = struct
  type t = float array

  let project s s' n =
    [| s; 0.; 0.; 0.; 0.; s'; 0.; 0.; 0.; 0.; -1.; -1.; 0.; 0.; -2. *. n; 0. |]

  let scale x y z : t =
    [| x; 0.; 0.; 0.; 0.; y; 0.; 0.; 0.; 0.; z; 0.; 0.; 0.; 0.; 1. |]

  let translate x y z : t =
    [| 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0.; x; y; z; 1. |]

  let rotate_x t : t =
    [|
      1.;
      0.;
      0.;
      0.;
      0.;
      cos t;
      sin t;
      0.;
      0.;
      -.sin t;
      cos t;
      0.;
      0.;
      0.;
      0.;
      1.;
    |]

  let rotate_y t : t =
    [|
      cos t;
      0.;
      -.sin t;
      0.;
      0.;
      1.;
      0.;
      0.;
      sin t;
      0.;
      cos t;
      0.;
      0.;
      0.;
      0.;
      1.;
    |]

  let c i j = (i * 4) + j
  let o i = (i / 4, i mod 4)

  let mult m1 m2 =
    let v p =
      let i, j = o p in
      (m1.(c i 0) *. m2.(c 0 j))
      +. (m1.(c i 1) *. m2.(c 1 j))
      +. (m1.(c i 2) *. m2.(c 2 j))
      +. (m1.(c i 3) *. m2.(c 3 j))
    in
    Array.init 16 v

  let array m =
    let a = bigarray_create Float32 (Array.length m) in
    for i = 0 to Array.length m - 1 do
      a.{i} <- m.(i)
    done;
    a

  let _ = (scale, translate, rotate_y, mult, project)
end

let pi = 4. *. atan 1.

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

let deltax = 40_000. /. 360. /. 3600. *. 1000.
let deltay = deltax *. cos (44. *. pi /. 180.)

let vertex_shader =
  "\n\
  \  #version 300 es\n\
  \  uniform mat4 proj;\n\
  \  uniform mat4 transform;\n\
  \  uniform int w;\n\
  \  uniform int h;\n\
  \  uniform vec2 delta;\n\
  \  in float height;\n\
  \  in vec3 normal;\n\
  \  out vec3 v_color;\n\
  \  out vec4 position;\n\
  \  void main()\n\
  \  {\n\
  \    float x = float(gl_VertexID % w) * delta.x;\n\
  \    float y = float(h - 1 - (gl_VertexID / w)) * delta.y;\n\
  \    float z = height; \n\
  \    float l = max(dot(normalize(normal), normalize(vec3(-1, 1, 2))), 0.);\n\
  \    v_color = l * vec3(0.3, 0.32, 0.19);\n\
  \    position = transform * vec4(x, y, z, 1.0);\n\
  \    gl_Position = proj * position; \n\
  \  }"

let fragment_shader =
  "\n\
  \  #version 300 es\n\
  \  precision highp float;\n\
  \  in vec3 v_color;\n\
  \  in vec4 position;\n\
  \  out vec3 color;\n\
  \  void main() {\n\
  \    float fogAmount = exp(- length(position.xyz) * 1e-4);\n\
  \    vec3  fogColor  = vec3(0.5,0.6,0.7);\n\
  \    color = mix(fogColor, v_color, fogAmount);\n\
  \  }"

(* Geometry *)

(*
let set_3d ba i x y z =
  let start = i * 3 in
  ba.{start} <- x;
  ba.{start + 1} <- y;
  ba.{start + 2} <- z
*)

let linearize2 a =
  Bigarray.(reshape_1 (genarray_of_array2 a) (Array2.dim1 a * Array2.dim2 a))

let linearize3 a =
  Bigarray.(
    reshape_1 (genarray_of_array3 a)
      (Array3.dim1 a * Array3.dim2 a * Array3.dim3 a))

let precompute tile_height tile_width tile =
  let normals =
    Bigarray.(Array3.create Int8_signed C_layout)
      (tile_height - 2) (tile_width - 2) 3
  in
  let heights =
    Bigarray.(Array2.create Float32 C_layout) (tile_height - 2) (tile_width - 2)
  in
  for y = 1 to tile_height - 2 do
    for x = 1 to tile_width - 2 do
      let nx = (tile.{y, x - 1} -. tile.{y, x + 1}) *. deltay in
      let ny = (tile.{y - 1, x} -. tile.{y + 1, x}) *. deltax in
      let nz = 2. *. deltax *. deltay in
      let n = 127. /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
      normals.{y - 1, x - 1, 0} <- truncate (nx *. n);
      normals.{y - 1, x - 1, 1} <- truncate (ny *. n);
      normals.{y - 1, x - 1, 2} <- truncate (nz *. n);
      heights.{y - 1, x - 1} <- tile.{y, x}
    done
  done;
  (linearize2 heights, linearize3 normals)

let build_indices w h =
  let is = bigarray_create Bigarray.int32 ((2 * (h - 1) * (w + 1)) - 2) in
  for i = 0 to h - 2 do
    for j = 0 to w - 1 do
      is.{(i * (w + 1) * 2) + (j * 2)} <- Int32.of_int (j + (i * w));
      is.{(i * (w + 1) * 2) + (j * 2) + 1} <- Int32.of_int (j + ((i + 1) * w))
    done;
    if i > 0 then (
      is.{(i * (w + 1) * 2) - 2} <- Int32.of_int (((i + 1) * w) - 1);
      is.{(i * (w + 1) * 2) - 1} <- Int32.of_int (i * w))
  done;
  is

(* OpenGL setup *)

let create_buffer b =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw;
  id

let delete_buffer bid = set_int (Gl.delete_buffers 1) bid

let create_geometry ~w ~h heights normals =
  let indices = build_indices w h in
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  let iid = create_buffer indices in
  let nid = create_buffer normals in
  let hid = create_buffer heights in
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0)
  in
  Gl.bind_vertex_array gid;
  Gl.bind_buffer Gl.element_array_buffer iid;
  bind_attrib hid 0 1 Gl.float;
  bind_attrib nid 1 3 Gl.byte;
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;
  Ok (gid, [ iid; hid; nid ])

let delete_geometry gid bids =
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

let create_program () =
  compile_shader vertex_shader Gl.vertex_shader >>= fun vid ->
  compile_shader fragment_shader Gl.fragment_shader >>= fun fid ->
  let pid = Gl.create_program () in
  let get_program pid e = get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid;
  Gl.delete_shader vid;
  Gl.attach_shader pid fid;
  Gl.delete_shader fid;
  Gl.bind_attrib_location pid 0 "height";
  Gl.bind_attrib_location pid 1 "normal";
  Gl.link_program pid;
  if get_program pid Gl.link_status = Gl.true_ then Ok pid
  else
    let len = get_program pid Gl.info_log_length in
    let log = get_string len (Gl.get_program_info_log pid len None) in
    Gl.delete_program pid;
    Error (`Msg log)

let delete_program pid =
  Gl.delete_program pid;
  Ok ()

let draw pid gid ~aspect ~w ~h win =
  Gl.clear_color 0.37 0.56 0.85 1.;
  Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
  Gl.use_program pid;
  Gl.enable Gl.depth_test;
  Gl.enable Gl.cull_face_enum;
  let height_loc = Gl.get_uniform_location pid "h" in
  Gl.uniform1i height_loc h;
  let width_loc = Gl.get_uniform_location pid "w" in
  Gl.uniform1i width_loc w;
  let delta_loc = Gl.get_uniform_location pid "delta" in
  Gl.uniform2f delta_loc deltax deltay;
  let transform =
    (*
    Proj3D.(
      mult (translate (-.deltax *. 511.) 0. (-2060.)) (rotate_x (-.pi /. 2.)))
*)
    Proj3D.(
      mult
        (translate (-.deltax *. 511.) (-.deltay *. 10.) (-1936.))
        (rotate_x (-.pi /. 2.)))
  in
  let proj = Proj3D.project (3. /. aspect) 3. 1. in
  let proj_loc = Gl.get_uniform_location pid "proj" in
  Gl.uniform_matrix4fv proj_loc 1 false (Proj3D.array proj);
  let transform_loc = Gl.get_uniform_location pid "transform" in
  Gl.uniform_matrix4fv transform_loc 1 false (Proj3D.array transform);
  Gl.bind_vertex_array gid;
  Gl.draw_elements Gl.triangle_strip
    ((2 * (h - 1) * (w + 1)) - 2)
    Gl.unsigned_int (`Offset 0);
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
  set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_es >>= fun () ->
  set Sdl.Gl.context_major_version maj >>= fun () ->
  set Sdl.Gl.context_minor_version min >>= fun () ->
  set Sdl.Gl.doublebuffer 1 >>= fun () ->
  Sdl.create_window ~w:640 ~h:480 w_title w_atts >>= fun win ->
  Sdl.gl_create_context win >>= fun ctx ->
  Sdl.gl_make_current win ctx >>= fun () ->
  Sdl.log "%a" pp_opengl_info ();
  Ok (win, ctx)

let destroy_window win ctx =
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window win;
  Ok ()

(* Event loop *)

let event_loop win draw =
  let e = Sdl.Event.create () in
  let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let rec loop () =
    Sdl.wait_event (Some e) >>= fun () ->
    match event e with
    | `Quit -> Ok ()
    | `Key_down when key_scancode e = `Escape -> Ok ()
    | `Window_event -> (
        match window_event e with
        | `Exposed | `Resized ->
            let w, h = Sdl.get_window_size win in
            reshape win w h;
            draw ~aspect:(float w /. float h) win;
            loop ()
        | _ -> loop ())
    | _ -> loop ()
  in
  draw ~aspect:(640. /. 400.) win;
  loop ()

(* Main *)

let tri ~gl:((_maj, _min) as gl) ~w ~h heights normals =
  Sdl.init Sdl.Init.video >>= fun () ->
  create_window ~gl >>= fun (win, ctx) ->
  create_geometry ~w ~h heights normals >>= fun (gid, bids) ->
  create_program () >>= fun pid ->
  event_loop win (fun ~aspect win -> ignore (draw pid gid ~aspect ~w ~h win))
  >>= fun () ->
  delete_program pid >>= fun () ->
  delete_geometry gid bids >>= fun () ->
  destroy_window win ctx >>= fun () ->
  Sdl.quit ();
  Ok ()

let main () =
  let ch = open_in "Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif" in
  let { Relief.tile_width; tile_height; tile_offsets; tile_byte_counts; _ } =
    Relief.read_info ch
  in
  let tile =
    Relief.read_tile ch tile_width tile_height tile_offsets tile_byte_counts 0
  in
  Format.eprintf "ZZZZ %f@." tile.{1022 - 10, 512};
  let heights, normals = precompute tile_width tile_height tile in
  let exec = Filename.basename Sys.executable_name in
  let usage = str "Usage: %s [OPTION]\n Tests Tgles3.\nOptions:" exec in
  let options = [] in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;
  match
    tri ~gl:(3, 0) ~w:(tile_width - 2) ~h:(tile_height - 2) heights normals
  with
  | Ok () -> exit 0
  | Error (`Msg msg) ->
      Sdl.log "%s@." msg;
      exit 1

let () = main ()

(*
0 1 2 3
|/|/|/|
4 5 6 7
|/|/|/|
8 9 10 11
|/|/|/|
12 13 14 15

   0  4  1  5  2  6  3  7  7
4  4  8  5  9  6 10  7 11 11
8  8 12  9 13 10 14 11 15
*)
