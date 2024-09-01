let _ =
  Printexc.register_printer (function
    | Jv.Error e -> Some (Jstr.to_string (Jv.Error.message e))
    | _ -> None)
(* *)

module Loader = Loader.Make (Reader)

let ( let** ) = Lwt.bind
let ( let* ) = Result.bind
let pi = 4. *. atan 1.

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
      {|#version 300 es
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
      {|#version 300 es
        precision highp float;
        in vec4 position;
        in vec3 normal;
        out vec4 color;
        void main() {
          float l = max(dot(normalize(normal), normalize(vec3(-1, 1, 2))), 0.);
          vec3 terrain_color = l * vec3(0.3, 0.32, 0.19);
          float fog_coeff = exp(length(position.xyz) * -1e-4);
          vec3 fog_color = vec3(0.36, 0.45, 0.59);
          color = vec4(mix(fog_color, terrain_color, fog_coeff), 1.);
        }
      |};
    attributes = [ "height"; "vertex_normal" ];
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
  if Jv.to_bool (Gl.get_shader_parameter ctx sid Gl.compile_status) then Ok sid
  else
    let log = Gl.get_shader_info_log ctx sid in
    Gl.delete_shader ctx sid;
    Error (`Msg (Jstr.to_string log))

let create_program ctx p =
  let* vid = compile_shader ctx p.vertex_shader Gl.vertex_shader in
  let* fid = compile_shader ctx p.fragment_shader Gl.fragment_shader in
  let pid = Gl.create_program ctx in
  Gl.attach_shader ctx pid vid;
  Gl.delete_shader ctx vid;
  Gl.attach_shader ctx pid fid;
  Gl.delete_shader ctx fid;
  List.iteri
    (fun i attr -> Gl.bind_attrib_location ctx pid i (Jstr.v attr))
    p.attributes;
  Gl.link_program ctx pid;
  if Jv.to_bool (Gl.get_program_parameter ctx pid Gl.link_status) then Ok pid
  else
    let log = Gl.get_program_info_log ctx pid in
    Gl.delete_program ctx pid;
    Error (`Msg (Jstr.to_string log))

(* Geometry *)

let linearize2 a =
  Buffer
    Bigarray.(reshape_1 (genarray_of_array2 a) (Array2.dim1 a * Array2.dim2 a))

let linearize3 a =
  Buffer
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
  (linearize2 heights, linearize3 normals)

let build_indices w w' h =
  let t = Unix.gettimeofday () in
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
  Format.eprintf "BUILD INDICES %f@." (Unix.gettimeofday () -. t);
  is

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

let scale = (*2. *. 27. /. 24.*) 3.2
let text_height = 0.07

type orientation = {
  alpha : float;
  beta : float;
  gamma : float;
  screen : float;
}

let draw terrain_pid terrain_geo triangle_pid text_pid text_geo ~w ~h ~x ~y
    ~height ~orientation ~points ~tile canvas ctx =
  let canvas_width = truncate (Brr.El.inner_w canvas) in
  let canvas_height = truncate (Brr.El.inner_h canvas) in
  let canvas = Brr_canvas.Canvas.of_el canvas in
  if Brr_canvas.Canvas.w canvas <> canvas_width then
    Brr_canvas.Canvas.set_w canvas canvas_width;
  if Brr_canvas.Canvas.h canvas <> canvas_height then
    Brr_canvas.Canvas.set_h canvas canvas_height;
  Gl.viewport ctx 0 0 canvas_width canvas_height;
  let aspect = float canvas_width /. float canvas_height in
  let transform =
    Matrix.(
      translate
        (-.deltax *. float (x - 1))
        (-.deltay *. float (h - y))
        (-.height -. 2.)
      * rotate_z (-.orientation.alpha *. pi /. 180.)
      * rotate_x (-.orientation.beta *. pi /. 180.)
      * rotate_y (-.orientation.gamma *. pi /. 180.)
      * rotate_z (orientation.screen *. pi /. 180.))
  in
  let screen_inclination =
    orientation.screen
    +. 180. /. pi
       *. atan2
            (sin (orientation.gamma *. pi /. 180.)
            *. cos (orientation.beta *. pi /. 180.))
            (sin (orientation.beta *. pi /. 180.))
  in
  let proj =
    Matrix.project ~x_scale:(scale /. aspect) ~y_scale:scale ~near_plane:1.
  in
  let points =
    let vert = Matrix.({ x = 0.; y = 0.; z = 1.; w = 0. } *< transform) in
    List.filter_map
      (fun (pt, (x', y')) ->
        let x = deltax *. float (x' - 1) in
        let y = deltay *. float (h - y') in
        let z = tile.{y', x'} in
        let r = Matrix.({ x; y; z; w = 1. } *< transform) in
        let r = { r with z = -.r.z } in
        let h = (vert.x *. r.x /. r.z) +. (vert.y *. r.y /. r.z) in
        if r.z > 0. then Some (pt, r.x /. r.z, r.y /. r.z, h) else None)
      points
    |> List.sort (fun (_, _, _, h) (_, _, _, h') : int -> Stdlib.compare h' h)
  in
  let points =
    let pos = ref [] in
    let angle = (screen_inclination *. pi /. 180.) +. (pi /. 4.) in
    let ca = cos angle in
    let sa = sin angle in
    List.filter
      (fun (_, x, y, _) ->
        let p = scale *. ((y *. ca) -. (x *. sa)) in
        if
          not
            (List.exists
               (fun p' -> abs_float (p' -. p) < 0.8 (*ZZZ*) *. text_height)
               !pos)
        then (
          pos := p :: !pos;
          true)
        else false)
      points
  in

  Gl.clear_color ctx 0.37 0.56 0.85 1.;
  Gl.clear ctx (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

  Gl.use_program ctx terrain_pid;
  Gl.enable ctx Gl.depth_test;
  Gl.enable ctx Gl.cull_face';
  let width_shift_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "w_shift")
  in
  Gl.uniform1i ctx width_shift_loc (truncate (log (float w) /. log 2.));
  let width_mask_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "w_mask")
  in
  Gl.uniform1i ctx width_mask_loc (w - 1);
  let delta_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "delta") in
  Gl.uniform2f ctx delta_loc deltax deltay;
  let proj_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "proj") in
  Gl.uniform_matrix4fv ctx proj_loc false
    (Brr.Tarray.of_bigarray1 (Matrix.array proj));
  let transform_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "transform")
  in
  Gl.uniform_matrix4fv ctx transform_loc false
    (Brr.Tarray.of_bigarray1 (Matrix.array transform));
  Gl.bind_vertex_array ctx (Some terrain_geo);
  Gl.draw_elements ctx Gl.triangle_strip
    ((2 * (h - 1) * (w + 1)) - 2)
    Gl.unsigned_int 0;
  Gl.bind_vertex_array ctx None;
  Gl.disable ctx Gl.depth_test;
  Gl.disable ctx Gl.cull_face';

  Gl.use_program ctx triangle_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  let transform_loc =
    Gl.get_uniform_location ctx triangle_pid (Jstr.v "transform")
  in
  List.iter
    (fun (_, x, y, _) ->
      let x = x *. scale /. aspect in
      let y = y *. scale in
      let transform =
        Matrix.(
          rotate_z ((-.pi /. 4.) +. (screen_inclination *. pi /. 180.))
          * scale (0.6 *. text_height /. aspect) (0.6 *. text_height) 1.
          * translate x y 0.)
      in
      Gl.uniform_matrix4fv ctx transform_loc false
        (Brr.Tarray.of_bigarray1 (Matrix.array transform));
      Gl.draw_elements ctx Gl.triangles 3 Gl.unsigned_byte 0)
    points;
  Gl.bind_vertex_array ctx None;

  Gl.use_program ctx text_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  Gl.enable ctx Gl.blend;
  Gl.blend_func ctx Gl.one Gl.one_minus_src_alpha;
  let transform_loc =
    Gl.get_uniform_location ctx text_pid (Jstr.v "transform")
  in
  List.iter
    (fun (texture, x, y, _) ->
      let x = x *. scale /. aspect in
      let y = y *. scale in
      let transform =
        Matrix.(
          translate 0.7 (-0.5) 0.
          * rotate_z ((pi /. 4.) +. (screen_inclination *. pi /. 180.))
          * scale (text_height /. aspect) text_height 1.
          * translate x y 0.)
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
    let** () = request_animation_frame () in
    loop orientation
  in
  loop { !current_orientation with alpha = !current_orientation.alpha -. 1. }

(* Main *)

let tri ~w ~h ~x ~y ~orientation ~height ~points ~tile heights normals canvas
    ctx =
  current_orientation := orientation;
  let terrain_geo =
    create_geometry ctx ~indices:(build_indices w w h)
      ~buffers:[ (1, Gl.float, heights); (3, Gl.byte, normals) ]
  in
  let text_geo =
    create_geometry ctx
      ~indices:(Bigarray.(Array1.init int8_unsigned c_layout) 4 (fun i -> i))
      ~buffers:[]
  in
  let* terrain_pid = create_program ctx terrain_program in
  let* triangle_pid = create_program ctx triangle_program in
  let* text_pid = create_program ctx text_program in
  let points =
    List.map
      (fun ({ Points.name; elevation; _ }, pos) ->
        ( prepare_text ctx
            (match elevation with
            | None -> name
            | Some elevation -> Printf.sprintf "%s (%dm)" name elevation),
          pos ))
      points
  in
  ( Lwt.async @@ fun () ->
    event_loop ctx (fun ~orientation ctx ->
        draw terrain_pid terrain_geo triangle_pid text_pid text_geo ~w ~h ~x ~y
          ~orientation ~height ~tile ~points canvas ctx) );
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
  ( tile_index,
    x,
    y,
    { Points.lon = tile_lon; lat = tile_lat },
    {
      Points.lon = tile_lon +. (float tile_width /. float width);
      lat = tile_lat +. (float tile_height /. float height);
    } )
*)

let main () =
  let lat, lon, angle =
    if true then (44.607649, 6.8204019, 0.)
      (*(44.607728, 6.821075, 0.)*)
      (* Col Girardin *)
    else if true then (44.209067, 6.9423065, 0.) (* Col du Blainon *)
    else if true then (44.207447, 6.906400, 40.)
      (* Auron vers est vallée de la Tinée *)
    else if true then (44.278358, 6.790589, 0.)
    else if true then (44.280097, 6.793942, 0.) (* Vallon de la Braïssa *)
    else if true then (44.336025, 6.907772, 0.) (* Lacs de Morgon *)
    else if true then (44.73365, 6.3630684, 0.) (* Roc Diolon (Orcières) *)
    else if true then (44.6896583, 6.8061028, 180.) (* Col Fromage *)
    else (44.789628, 6.670200, 66.)
  in
  let** tile = Loader.f ~lat ~lon in
  (*ZZZ*)
  let x = 1025 in
  let y = 1025 in
  let tile_width = 2050 in
  let tile_height = 2050 in
  let tile_coord =
    { Points.lon = lon -. (1024. /. 3600.); lat = lat -. (1024. /. 3600.) }
  in
  let tile_coord' =
    { Points.lon = lon +. (1024. /. 3600.); lat = lat +. (1024. /. 3600.) }
  in
  let** points =
    let width = 3600 in
    let height = 3600 in
    let** points = Reader.read_file "data/points.geojson" in
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
             let x = truncate ((lon -. tile_coord.lon) *. float width) in
             let y = truncate ((tile_coord'.lat -. lat) *. float height) in
             (pt, (x, y))))
  in
  let points =
    List.filter
      (fun (_, (dst_x, dst_y)) ->
        Visibility.test tile ~src_x:x ~src_y:y ~dst_x ~dst_y)
      points
  in
  let height = tile.{y, x} in
  let heights, normals = precompute tile_width tile_height tile in
  let canvas =
    Option.get (Brr.Document.find_el_by_id Brr.G.document (Jstr.v "canvas"))
  in
  let ctx =
    Option.get
      (Brr_canvas.Gl.get_context ~attrs:(Gl.Attrs.v ())
         (Brr_canvas.Canvas.of_el canvas))
  in
  match
    tri ~w:(tile_width - 2) ~h:(tile_height - 2) ~x ~y
      ~orientation:{ alpha = angle; beta = 90.; gamma = 0.; screen = 0. }
      ~height ~points ~tile heights normals canvas ctx
  with
  | Ok () -> Lwt.return ()
  | Error (`Msg msg) ->
      Brr.Console.log [ Jstr.v msg ];
      Lwt.return ()

let () =
  let deviceorientation =
    Brr.Ev.Type.create (Jstr.v "deviceorientationabsolute")
  in
  ignore
    (Brr.Ev.listen deviceorientation
       (fun ev ->
         let angle nm = Jv.to_float (Jv.get (Brr.Ev.as_type ev) nm) in
         let alpha = angle "alpha" in
         let beta = angle "beta" in
         let gamma = angle "gamma" in
         (*
         Brr.Console.log
           [ Jv.get (Jv.get (Jv.get Jv.global "screen") "orientation") "angle" ];
*)
         let screen =
           Jv.to_float
             (Jv.get (Jv.get (Jv.get Jv.global "screen") "orientation") "angle")
         in
         current_orientation := { alpha; beta; gamma; screen })
       (Brr.Window.as_target Brr.G.window))

let () = Lwt.async main
