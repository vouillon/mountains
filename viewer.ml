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
*)

(*
- gather tiles around where we are

- draw closer triangles before farther triangles
  (split the whole grid in four quadrants and draw triangles appropriately)
*)

open Tsdl
open Tsdl_ttf
open Tgles3

let ( let* ) = Result.bind
let pi = 4. *. atan 1.
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
let deltax = deltay *. cos (44. *. pi /. 180.)

type program = {
  vertex_shader : string;
  fragment_shader : string;
  attributes : string list;
}

let terrain_program =
  {
    vertex_shader =
      {|
        #version 300 es
        uniform mat4 proj;
        uniform mat4 transform;
        uniform int w_mask;
        uniform int w_shift;
        uniform vec2 delta;
        in float height;
        in vec3 vertex_normal;
        out vec3 normal;
        out vec4 position;
        void main()
        {
          float x = float(gl_VertexID & w_mask) * delta.x;
          float y = float((gl_VertexID >> w_shift)) * delta.y;
          float z = height;
          normal = vertex_normal;
          position = transform * vec4(x, y, z, 1.0);
          gl_Position = proj * position;
        }
      |};
    fragment_shader =
      {|
        #version 300 es
        precision highp float;
        in vec4 position;
        in vec3 normal;
        out vec3 color;
        void main() {
          float l = max(dot(normalize(normal), normalize(vec3(-1, 1, 2))), 0.);
          vec3 terrain_color = l * vec3(0.3, 0.32, 0.19);
          float fog_coeff = exp(length(position.xyz) * -1e-4);
          vec3  fog_color  = vec3(0.36, 0.45, 0.59);
          color = mix(fog_color, terrain_color, fog_coeff);
        }
      |};
    attributes = [ "height"; "vertex_normal" ];
  }

let triangle_program =
  {
    vertex_shader =
      {|
        #version 300 es
        uniform mat4 transform;
        void main() {
          float x = float(gl_VertexID - 1) / 2.;
          float y = float(gl_VertexID != 1) * (sqrt(3.)/ 2.);
          gl_Position = transform * vec4(x, y, 0, 1.);
        }
      |};
    fragment_shader =
      {|
        #version 300 es
        precision highp float;
        out vec3 color;
        void main() {
          color = vec3(0,0,0);
        }
      |};
    attributes = [];
  }

let text_program =
  {
    vertex_shader =
      {|
        #version 300 es
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
      {|
        #version 300 es
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

(* OpenGL setup *)

type buffer = Buffer : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> buffer

let create_buffer (Buffer b) =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw;
  id

let delete_buffer bid = set_int (Gl.delete_buffers 1) bid

type geometry = { vertex_array : int; buffers : int list }

let create_geometry ~indices ~buffers =
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  let iid = create_buffer (Buffer indices) in
  let bind_attrib loc dim typ data =
    let id = create_buffer data in
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
    id
  in
  Gl.bind_vertex_array gid;
  Gl.bind_buffer Gl.element_array_buffer iid;
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

let linearize2 a =
  Buffer
    Bigarray.(reshape_1 (genarray_of_array2 a) (Array2.dim1 a * Array2.dim2 a))

let linearize3 a =
  Buffer
    Bigarray.(
      reshape_1 (genarray_of_array3 a)
        (Array3.dim1 a * Array3.dim2 a * Array3.dim3 a))

(* Geometry *)

let precompute tile_height tile_width tile =
  let normals =
    Bigarray.(Array3.create Int8_signed C_layout) (tile_height - 2) tile_width 3
  in
  let heights =
    Bigarray.(Array2.create Float32 C_layout) (tile_height - 2) tile_width
  in
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
  (linearize2 heights, linearize3 normals)

let build_indices w w' h =
  let is =
    Bigarray.(
      Array1.create Bigarray.int32 c_layout ((2 * (h - 1) * (w + 1)) - 2))
  in
  for i = 0 to h - 2 do
    for j = 0 to w - 1 do
      is.{(i * (w + 1) * 2) + (j * 2) + 1} <- Int32.of_int (j + (i * w'));
      is.{(i * (w + 1) * 2) + (j * 2)} <- Int32.of_int (j + ((i + 1) * w'))
    done;
    if i > 0 then (
      is.{(i * (w + 1) * 2) - 2} <- Int32.of_int (((i - 1) * w') + w - 1);
      is.{(i * (w + 1) * 2) - 1} <- Int32.of_int ((i + 1) * w'))
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
  let* () = Sdl.lock_surface surface in
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

let draw terrain_pid terrain_geo triangle_pid text_pid text_geo ~font ~aspect ~w
    ~h ~x ~y ~height ~angle ~points ~tile win =
  let points : (Points.t * _ * _) list =
    List.filter_map
      (fun (pt, (x', y')) ->
        let z = tile.{y', x'} -. tile.{y, x} in
        let x = deltax *. float (x' - x) in
        let y = deltay *. float (y' - y) in
        let angle = angle *. pi /. 180. in
        let x' = (x *. cos angle) +. (y *. sin angle) in
        let y' = (-.x *. sin angle) +. (y *. cos angle) in
        let y' = -.y' in
        if y' > 0. then Some (pt, x' /. y', z /. y') else None)
      points
    |> List.sort (fun (_, _, y) (_, _, y') : int -> Stdlib.compare y' y)
  in
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

  Gl.clear_color 0.37 0.56 0.85 1.;
  Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

  use_program terrain_pid;
  Gl.enable Gl.depth_test;
  Gl.enable Gl.cull_face_enum;
  let width_shift_loc = get_uniform_location terrain_pid "w_shift" in
  Gl.uniform1i width_shift_loc (truncate (log (float (w + 2)) /. log 2.));
  let width_mask_loc = get_uniform_location terrain_pid "w_mask" in
  Gl.uniform1i width_mask_loc (w + 1);
  let delta_loc = get_uniform_location terrain_pid "delta" in
  Gl.uniform2f delta_loc deltax deltay;
  let transform =
    Matrix.(
      translate
        (-.deltax *. float (x - 1))
        (-.deltay *. float (h - y))
        (-.height -. 2.)
      * (rotate_z (angle *. pi /. 180.) * rotate_x (-.pi /. 2.)))
  in
  let proj =
    Matrix.project ~x_scale:(scale /. aspect) ~y_scale:scale ~near_plane:1.
  in
  let proj_loc = get_uniform_location terrain_pid "proj" in
  Gl.uniform_matrix4fv proj_loc 1 false (Matrix.array proj);
  let transform_loc = get_uniform_location terrain_pid "transform" in
  Gl.uniform_matrix4fv transform_loc 1 false (Matrix.array transform);
  bind_vertex_array terrain_geo;
  Gl.draw_elements Gl.triangle_strip
    ((2 * (h - 1) * (w + 1)) - 2)
    Gl.unsigned_int (`Offset 0);
  Gl.bind_vertex_array 0;
  Gl.disable Gl.depth_test;
  Gl.disable Gl.cull_face_enum;

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
  Gl.enable Gl.texture_2d;
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
  Gl.disable Gl.texture_2d;
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

let event_loop win angle draw =
  let e = Sdl.Event.create () in
  let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let rec loop angle =
    let* () = Sdl.wait_event (Some e) in
    match event e with
    | `Quit -> Ok ()
    | `Key_down when key_scancode e = `Escape -> Ok ()
    | `Key_down when key_scancode e = `Right ->
        let angle = angle +. 3. in
        let w, h = Sdl.get_window_size win in
        draw ~aspect:(float w /. float h) ~angle win;
        loop angle
    | `Key_down when key_scancode e = `Left ->
        let angle = angle -. 3. in
        let w, h = Sdl.get_window_size win in
        draw ~aspect:(float w /. float h) ~angle win;
        loop angle
    | `Window_event -> (
        match window_event e with
        | `Exposed | `Resized ->
            let w, h = Sdl.get_window_size win in
            reshape win w h;
            draw ~aspect:(float w /. float h) ~angle win;
            loop angle
        | _ -> loop angle)
    | _ -> loop angle
  in
  draw ~aspect:(640. /. 400.) ~angle win;
  loop angle

(* Main *)

let tri ~gl:((_maj, _min) as gl) ~w ~h ~x ~y ~angle ~height ~points ~tile
    heights normals =
  let* () = Sdl.init Sdl.Init.video in
  let* font = load_font () in
  let* win, ctx = create_window ~gl in
  let* terrain_geo =
    create_geometry
      ~indices:(build_indices w (w + 2) h)
      ~buffers:[ (1, Gl.float, heights); (3, Gl.byte, normals) ]
  in
  let* text_geo =
    create_geometry
      ~indices:(Bigarray.(Array1.init int8_unsigned c_layout) 4 (fun i -> i))
      ~buffers:[]
  in
  let* terrain_pid = create_program terrain_program in
  let* triangle_pid = create_program triangle_program in
  let* text_pid = create_program text_program in
  let* () =
    event_loop win angle (fun ~aspect ~angle win ->
        ignore
          (draw terrain_pid terrain_geo triangle_pid text_pid text_geo ~font
             ~aspect ~w ~h ~x ~y ~angle ~height ~tile ~points win))
  in
  let* () = delete_program terrain_pid in
  let* () = delete_program triangle_pid in
  let* () = delete_program text_pid in
  let* () = delete_geometry terrain_geo in
  let* () = delete_geometry text_geo in
  let* () = destroy_window win ctx in
  Sdl.quit ();
  Ok ()

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
  ( tile_index,
    x,
    y,
    { Points.lon = tile_lon; lat = tile_lat },
    {
      Points.lon = tile_lon +. (float tile_width /. float width);
      lat = tile_lat +. (float tile_height /. float height);
    } )

let main () =
  let ch = open_in "Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif" in
  let ({ Tiff.width; height; tile_width; tile_height; _ } as info) =
    Tiff.read_info ch
  in
  let lat, lon, angle =
    if true then (44.209067, 6.9423065, 0.) (* Col du Blainon *)
    else if true then (44.207447, 6.906400, 40.)
      (* Auron vers est vallée de la Tinée *)
    else if true then (44.278358, 6.790589, 0.)
    else if true then (44.280097, 6.793942, 0.) (* Vallon de la Braïssa *)
    else if true then (44.336025, 6.907772, 0.) (* Lacs de Morgon *)
    else if true then (44.7333863, 6.3630684, 0.) (* Roc Diolon (Orcières) *)
    else if true then (44.6896583, 6.8061028, 180.) (* Col Fromage *)
    else (44.789628, 6.670200, 66.)
  in
  let tile_index, x, y, tile_coord, tile_coord' = coordinates info lat lon in
  let points =
    Points.find tile_coord tile_coord'
    |> List.map (fun ({ Points.coord = { lat; lon }; _ } as pt) ->
           let x = truncate ((lon -. tile_coord.lon) *. float width) in
           let y = truncate ((tile_coord'.lat -. lat) *. float height) in
           (pt, (x, y)))
  in
  let tile = Tiff.read_tile ch info tile_index in
  Format.eprintf "ZZZZ %d %d %d %f@." tile_index x y tile.{y, x};
  let points =
    List.filter
      (fun (_, (dst_x, dst_y)) ->
        Visibility.test tile ~src_x:x ~src_y:y ~dst_x ~dst_y)
      points
  in
  let height = tile.{y, x} in
  let heights, normals = precompute tile_width tile_height tile in
  let exec = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf "Usage: %s [OPTION]\n Tests Tgles3.\nOptions:" exec
  in
  let options = [] in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;
  match
    tri ~gl:(3, 0) ~w:(tile_width - 2) ~h:(tile_height - 2) ~x ~y ~angle ~height
      ~points ~tile heights normals
  with
  | Ok () -> exit 0
  | Error (`Msg msg) ->
      Sdl.log "%s@." msg;
      exit 1

let () = main ()
