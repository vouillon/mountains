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
let message = ref None

let remove_message () =
  match !message with
  | Some msg ->
      Brr.El.remove msg;
      message := None
  | None -> ()

let display_message msg =
  remove_message ();
  let msg = Brr.El.(v (Jstr.v "div") [ txt (Jstr.v msg) ]) in
  Brr.El.append_children (Brr.Document.body Brr.G.document) [ msg ];
  message := Some msg

let display_temporary_message msg =
  display_message msg;
  ignore (Brr.G.set_timeout ~ms:10000 remove_message)

module Loader = Loader.Make (Reader)

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
        uniform mediump int w;
        uniform int w_mask;
        uniform int w_shift;
        uniform mediump vec2 delta;
        uniform sampler2D tile;
        out highp vec3 position;
        out highp vec2 gradCoord;
        void main()
        {
          mediump ivec2 coord =
            ivec2(gl_VertexID & w_mask, gl_VertexID >> w_shift);
          mediump ivec2 tileCoord = ivec2(coord.x + 1, w - coord.y);
          gradCoord = vec2(tileCoord);
          float z = texelFetch(tile, tileCoord, 0).r;
          vec4 pos = transform * vec4(vec2(coord) * delta, z, 1.0);
          position = pos.xyz;
          gl_Position = proj * pos;
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision mediump float;
        uniform vec2 delta;
        uniform mediump sampler2D gradient;
        uniform mediump int w;
        in highp vec2 gradCoord;
        in highp vec3 position;
        out lowp vec4 color;

        void main() {
          mediump vec2 tangent =
            vec2(texture(gradient, (2. * gradCoord - 1.) * (1. / (2. * float(w)))));
          highp vec3 normal =
            vec3(tangent * delta.yx,
                 2. * delta.x * delta.y);
          lowp float l =
            max(dot(normalize(normal), normalize(vec3(-1, 1, 2))), 0.);
          lowp vec3 terrain_color = vec3(0.3, 0.32, 0.19);
          lowp vec3 fog_color = vec3(0.36, 0.45, 0.59);
          float fog_coeff = exp(length(position) * -1e-4);
          color = vec4(mix(fog_color, l * terrain_color, fog_coeff), 1.);
        }
      |};
    attributes = [];
  }

let triangle_program =
  {
    vertex_shader =
      {|#version 300 es
        uniform mat4 transform;
        uniform vec4 color;
        out vec4 fragment_color;
        void main() {
          float x = float(gl_VertexID - 1) / 2.;
          float y = float(gl_VertexID != 1) * (sqrt(3.)/ 2.);
          fragment_color = color;
          gl_Position = transform * vec4(x, y, 0, 1.);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        in vec4 fragment_color;
        out vec4 color;
        void main() {
          color = fragment_color;
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
  if Jv.to_bool (Gl.get_shader_parameter ctx sid Gl.compile_status) then sid
  else
    let log = Gl.get_shader_info_log ctx sid in
    Gl.delete_shader ctx sid;
    failwith (Jstr.to_string log)

let create_program ctx p =
  let vid = compile_shader ctx p.vertex_shader Gl.vertex_shader in
  let fid = compile_shader ctx p.fragment_shader Gl.fragment_shader in
  let pid = Gl.create_program ctx in
  Gl.attach_shader ctx pid vid;
  Gl.delete_shader ctx vid;
  Gl.attach_shader ctx pid fid;
  Gl.delete_shader ctx fid;
  List.iteri
    (fun i attr -> Gl.bind_attrib_location ctx pid i (Jstr.v attr))
    p.attributes;
  Gl.link_program ctx pid;
  if Jv.to_bool (Gl.get_program_parameter ctx pid Gl.link_status) then pid
  else
    let log = Gl.get_program_info_log ctx pid in
    Gl.delete_program ctx pid;
    failwith (Jstr.to_string log)

(* Geometry *)

let linearize2 a =
  Buffer
    Bigarray.(reshape_1 (genarray_of_array2 a) (Array2.dim1 a * Array2.dim2 a))

let linearize3 a =
  Buffer
    Bigarray.(
      reshape_1 (genarray_of_array3 a)
        (Array3.dim1 a * Array3.dim2 a * Array3.dim3 a))

let instantiate ~size =
  let sz = (size + 65535) lsr 16 in
  let _WebAssembly = Jv.(get global "WebAssembly") in
  let file = Jstr.v "compute.wasm" in
  let memory =
    Jv.(new' (get _WebAssembly "Memory") [| obj [| ("initial", of_int sz) |] |])
  in
  Fut.of_promise
    ~ok:(fun e ->
      ( Brr.Tarray.Buffer.of_jv (Jv.get memory "buffer"),
        Jv.(get (get e "instance") "exports") ))
    Jv.(
      call _WebAssembly "instantiateStreaming"
        [|
          call global "fetch" [| Jv.of_jstr file |];
          obj [| ("env", obj [| ("memory", memory) |]) |];
        |])

let _precompute tile_height tile_width tile =
  let normals =
    Bigarray.(Array3.create Int8_signed C_layout)
      (tile_height - 2) (tile_width - 2) 3
  in
  let heights =
    Bigarray.(Array2.create Float32 C_layout) (tile_height - 2) (tile_width - 2)
  in
  if true then (
    to_lwt
    @@
    let tile_size = tile_height * tile_width * 4 in
    let heights_size = (tile_height - 2) * (tile_width - 2) * 4 in
    let normals_size = (tile_height - 2) * (tile_width - 2) * 3 in
    let size = tile_size + heights_size + normals_size in
    let open Fut.Result_syntax in
    let+ memory, funcs = instantiate ~size in
    let t = Unix.gettimeofday () in
    Brr.Tarray.set_tarray
      Brr.Tarray.(of_buffer Float32 memory)
      ~dst:0
      (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array2 tile));
    let t' = Unix.gettimeofday () in
    ignore
      (Jv.call funcs "precompute"
         [|
           Jv.of_int tile_width;
           Jv.of_int tile_height;
           Jv.of_float deltax;
           Jv.of_float deltay;
           Jv.of_int 0;
           Jv.of_int tile_size;
           Jv.of_int (tile_size + heights_size);
         |]);
    Format.eprintf "precompute (kernel) %f@." (Unix.gettimeofday () -. t');
    Brr.Tarray.set_tarray
      (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array2 heights))
      ~dst:0
      Brr.Tarray.(
        of_buffer ~byte_offset:tile_size ~length:(heights_size / 4) Float32
          memory);
    Brr.Tarray.set_tarray
      (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array3 normals))
      ~dst:0
      Brr.Tarray.(
        of_buffer ~byte_offset:(tile_size + heights_size) ~length:normals_size
          Int8 memory);
    Format.eprintf "precompute %f@." (Unix.gettimeofday () -. t);
    (linearize2 heights, linearize3 normals))
  else
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
    Lwt.return (linearize2 heights, linearize3 normals)

let build_indices w w' h =
  let t = Unix.gettimeofday () in
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
  Format.eprintf "BUILD INDICES %f@." (Unix.gettimeofday () -. t);
  is

let make_tile_texture ctx tile =
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.r32f
    (Bigarray.Array2.dim1 tile)
    (Bigarray.Array2.dim2 tile)
    0 Gl.red Gl.float
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array2 tile))
    0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.bind_texture ctx Gl.texture_2d None;
  tid

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
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
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

let draw terrain_pid terrain_geo tile_texture gradient_texture triangle_pid
    text_pid text_geo ~w ~w' ~h ~x ~y ~height ~orientation ~points ~tile canvas
    ctx =
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
  let x_scale, y_scale =
    if aspect < 1. then (scale /. aspect, scale) else (scale, scale *. aspect)
  in
  let proj = Matrix.project ~x_scale ~y_scale ~near_plane:0.1 in
  let points =
    List.filter_map
      (fun (pt, (x', y')) ->
        let x = deltax *. float (x' - 1) in
        let y = deltay *. float (h - y') in
        let z = tile.{y', x'} in
        let r = Matrix.({ x; y; z; w = 1. } *< transform) in
        let r = { r with z = -.r.z } in
        if r.z > 1. then Some (pt, r.x /. r.z, r.y /. r.z) else None)
      points
  in
  let points =
    let pos = ref [] in
    let angle = (screen_inclination *. pi /. 180.) +. (pi /. 4.) in
    let ca = cos angle in
    let sa = sin angle in
    List.map
      (fun (texture, x, y) ->
        let p = scale *. ((y *. ca) -. (x *. sa)) in
        let shown =
          if
            not
              (List.exists
                 (fun p' -> abs_float (p' -. p) < 0.8 (*ZZZ*) *. text_height)
                 !pos)
          then (
            pos := p :: !pos;
            true)
          else false
        in
        (texture, x, y, shown))
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
  Gl.uniform1i ctx width_shift_loc (truncate (log (float w') /. log 2.));
  let width_mask_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "w_mask")
  in
  Gl.uniform1i ctx width_mask_loc (w' - 1);
  let width_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "w") in
  Gl.uniform1i ctx width_loc w;
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
  let tile_loc = Gl.get_uniform_location ctx terrain_pid (Jstr.v "tile") in
  let gradient_loc =
    Gl.get_uniform_location ctx terrain_pid (Jstr.v "gradient")
  in
  Gl.uniform1i ctx tile_loc 0;
  Gl.uniform1i ctx gradient_loc 1;
  Gl.bind_vertex_array ctx (Some terrain_geo);
  Gl.active_texture ctx Gl.texture0;
  Gl.bind_texture ctx Gl.texture_2d (Some tile_texture);
  Gl.active_texture ctx Gl.texture1;
  Gl.bind_texture ctx Gl.texture_2d (Some gradient_texture);
  Gl.draw_elements ctx Gl.triangle_strip
    (((h - 1) * ((2 * w) + 1)) - 1)
    Gl.unsigned_int 0;
  Gl.bind_vertex_array ctx None;
  Gl.bind_texture ctx Gl.texture_2d None;
  Gl.disable ctx Gl.depth_test;
  Gl.disable ctx Gl.cull_face';
  Gl.active_texture ctx Gl.texture0;

  Gl.use_program ctx triangle_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  let transform_loc =
    Gl.get_uniform_location ctx triangle_pid (Jstr.v "transform")
  in
  let color_loc = Gl.get_uniform_location ctx triangle_pid (Jstr.v "color") in
  Gl.enable ctx Gl.blend;
  Gl.blend_func ctx Gl.one Gl.one_minus_src_alpha;
  List.iter
    (fun (_, x, y, shown) ->
      let x = x *. x_scale in
      let y = y *. y_scale in
      let angle = if shown then -.pi /. 4. else 0. in
      let transform =
        let sx = 0.6 *. text_height *. x_scale /. scale in
        let sy = 0.6 *. text_height *. y_scale /. scale in
        Matrix.(
          rotate_z (angle +. (screen_inclination *. pi /. 180.))
          * scale sx sy 1. * translate x y 0.)
      in
      Gl.uniform_matrix4fv ctx transform_loc false
        (Brr.Tarray.of_bigarray1 (Matrix.array transform));
      if shown then Gl.uniform4f ctx color_loc 0. 0. 0. 1.
      else Gl.uniform4f ctx color_loc 0. 0. 0. 0.4;
      Gl.draw_elements ctx Gl.triangles 3 Gl.unsigned_byte 0)
    points;
  Gl.bind_vertex_array ctx None;
  Gl.disable ctx Gl.blend;

  Gl.use_program ctx text_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  Gl.enable ctx Gl.blend;
  Gl.blend_func ctx Gl.one Gl.one_minus_src_alpha;
  let transform_loc =
    Gl.get_uniform_location ctx text_pid (Jstr.v "transform")
  in
  List.iter
    (fun (texture, x, y, shown) ->
      if shown then
        let x = x *. x_scale in
        let y = y *. y_scale in
        let transform =
          let sx = text_height *. x_scale /. scale in
          let sy = text_height *. y_scale /. scale in
          Matrix.(
            translate 0.7 (-0.5) 0.
            * rotate_z ((pi /. 4.) +. (screen_inclination *. pi /. 180.))
            * scale sx sy 1. * translate x y 0.)
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
    let* () = request_animation_frame () in
    loop orientation
  in
  loop { !current_orientation with alpha = !current_orientation.alpha -. 1. }

(* Main *)

let compute_gradient_cpu ctx width height tile =
  let gradient = Bigarray.(Array3.create Float16 C_layout) width height 2 in
  for i = 1 to height do
    for j = 1 to width do
      gradient.{i - 1, j - 1, 0} <- tile.{i, j - 1} -. tile.{i, j + 1};
      gradient.{i - 1, j - 1, 1} <- tile.{i - 1, j} -. tile.{i + 1, j}
    done
  done;
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_image2d ctx Gl.texture_2d 0 Gl.rg16f width height 0 Gl.rg Gl.half_float
    (Brr.Tarray.of_bigarray (Bigarray.genarray_of_array3 gradient))
    0;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.bind_texture ctx Gl.texture_2d None;
  tid

let rec next_power_of_two n p = if n <= p then p else next_power_of_two n (p + p)

let gradient_program =
  {
    vertex_shader =
      {|#version 300 es
        out vec2 tileCoord;
        uniform vec2 size;
        void main() {
          float x = float(gl_VertexID & 1);
          float y = float(gl_VertexID >> 1);
          tileCoord = vec2(x, y) * (size - 1.) + vec2(1.5, 1.5);
          gl_Position = vec4(2. * vec2(x, y) - 1., 0, 1.);
        }
      |};
    fragment_shader =
      {|#version 300 es
        precision highp float;
        uniform vec2 size;
        in vec2 tileCoord;
        uniform sampler2D tile;
        out mediump vec2 color;
        void main() {
          highp vec2 coeff = 1. / (size + 2.);
          mediump float tx =
            (texture(tile, (tileCoord + vec2(-1,0)) * coeff).r -
             texture(tile, (tileCoord + vec2(1,0)) * coeff).r);
          mediump float ty =
            (texture(tile, (tileCoord + vec2(0,-1)) * coeff).r -
             texture(tile, (tileCoord + vec2(0,1)) * coeff).r);
          color = vec2(tx, ty);
        }
      |};
    attributes = [];
  }

let compute_gradient_gpu ctx width height text_geo tile_texture =
  assert (width = height);

  let gradient_pid = create_program ctx gradient_program in
  let tid = Gl.create_texture ctx in
  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  Gl.tex_parameteri ctx Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  let levels =
    truncate ((log (float (next_power_of_two width 1)) /. log 2.) +. 0.5)
  in
  Gl.tex_storage2d ctx Gl.texture_2d levels Gl.rg16f width height;
  let fb = Gl.create_framebuffer ctx in
  Gl.bind_framebuffer ctx Gl.framebuffer (Some fb);
  let attachmentPoint = Gl.color_attachment0 in
  Gl.framebuffer_texture2d ctx Gl.framebuffer attachmentPoint Gl.texture_2d tid
    0;
  Gl.bind_texture ctx Gl.texture_2d (Some tile_texture);
  Gl.viewport ctx 0 0 width height;
  Gl.use_program ctx gradient_pid;
  Gl.bind_vertex_array ctx (Some text_geo);
  let size_loc = Gl.get_uniform_location ctx gradient_pid (Jstr.v "size") in
  Gl.uniform2f ctx size_loc (float width) (float height);
  Gl.draw_elements ctx Gl.triangle_strip 4 Gl.unsigned_byte 0;
  Gl.bind_framebuffer ctx Gl.framebuffer None;
  Gl.bind_texture ctx Gl.texture_2d None;
  Gl.bind_vertex_array ctx None;

  Gl.bind_texture ctx Gl.texture_2d (Some tid);
  Gl.generate_mipmap ctx Gl.texture_2d;
  Gl.bind_texture ctx Gl.texture_2d None;

  tid

let compute_gradient ctx width height text_geo tile tile_texture =
  ignore (Gl.get_extension ctx (Jstr.v "EXT_color_buffer_half_float") : Jv.t);
  let accelerated_gradient =
    Jv.is_some (Gl.get_extension ctx (Jstr.v "EXT_color_buffer_float") : Jv.t)
  in
  Format.eprintf "COMPUTE GRADIENT ON GPU: %b@." accelerated_gradient;
  if accelerated_gradient then
    compute_gradient_gpu ctx width height text_geo tile_texture
  else compute_gradient_cpu ctx width height tile

let tri ~w ~h ~x ~y ~height ~points ~tile canvas ctx =
  let w' = next_power_of_two w 1 in
  let terrain_geo =
    create_geometry ctx ~indices:(build_indices w w' h) ~buffers:[]
  in
  let text_geo =
    create_geometry ctx
      ~indices:(Bigarray.(Array1.init int8_unsigned c_layout) 4 (fun i -> i))
      ~buffers:[]
  in
  let terrain_pid = create_program ctx terrain_program in
  let triangle_pid = create_program ctx triangle_program in
  let text_pid = create_program ctx text_program in
  let tile_texture = make_tile_texture ctx tile in
  let gradient_texture = compute_gradient ctx w h text_geo tile tile_texture in
  let points =
    List.map
      (fun ({ Points.name; elevation; _ }, ((x', y') as pos)) ->
        let texture =
          prepare_text ctx
            (match elevation with
            | None -> name
            | Some elevation -> Printf.sprintf "%s (%dm)" name elevation)
        in
        let h =
          let height' = tile.{y', x'} in
          let dist =
            sqrt
              ((float (x' - x) ** 2.)
              +. (float (y' - y) ** 2.)
              +. ((height' -. height) ** 2.))
          in
          (height' -. height) /. dist
        in
        ((texture, pos), h))
      points
  in
  let points =
    points
    |> List.sort (fun (_, h) (_, h') : int -> Stdlib.compare h' h)
    |> List.map fst
  in
  event_loop ctx (fun ~orientation ctx ->
      draw terrain_pid terrain_geo tile_texture gradient_texture triangle_pid
        text_pid text_geo ~w ~w' ~h ~x ~y ~orientation ~height ~tile ~points
        canvas ctx)

let wait_for_service_worker =
  let open Fut.Result_syntax in
  let open Brr_webworkers.Service_worker in
  let* r = Container.ready (Container.of_navigator Brr.G.navigator) in
  match Registration.active r with
  | None -> assert false
  | Some r ->
      if state r = State.activated then Fut.return (Ok ())
      else
        let fut, set = Fut.create () in
        ignore
          (Brr.Ev.listen Brr.Ev.statechange
             (fun _ -> if state r = State.activated then set (Ok ()))
             (as_target r));
        fut

let get_preset_position () =
  if true then (44.3950846, 6.7669714, 170.) (* La Chalannette, Jausiers *)
  else if true then (48.849418, 2.3674101, 0.) (* Paris *)
  else if true then (44.607649, 6.8204019, 220.) (* Col Girardin *)
  else if true then (44.209067, 6.9423065, 0.) (* Col du Blainon *)
  else if true then (44.207447, 6.906400, 40.)
    (* Auron vers est vallée de la Tinée *)
  else if true then (44.278358, 6.790589, 0.)
  else if true then (44.280097, 6.793942, 0.) (* Vallon de la Braïssa *)
  else if true then (44.336025, 6.907772, 0.) (* Lacs de Morgon *)
  else if true then (44.73365, 6.3630684, 0.) (* Roc Diolon (Orcières) *)
  else if true then (44.6896583, 6.8061028, 180.) (* Col Fromage *)
  else (44.789628, 6.670200, 66.)

let get_current_position ~size =
  let open Fut.Syntax in
  let open Brr_io.Geolocation in
  let opts = opts ~high_accuracy:true () in
  let+ pos = get ~opts (of_navigator Brr.G.navigator) in
  match pos with
  | Ok pos ->
      let lat = Pos.latitude pos in
      let lon = Pos.longitude pos in
      if
        Loader.in_range ~size ~min_lat:43 ~max_lat:46 ~min_lon:5 ~max_lon:9 ~lat
          ~lon
      (*
        || Loader.in_range ~size ~min_lat:48 ~max_lat:49 ~min_lon:2 ~max_lon:2
             ~lat ~lon
*)
      then Some (lat, lon, 0.)
      else None
  | Error _ -> None

let get_position ~size =
  let open Fut.Syntax in
  let+ loc = get_current_position ~size in
  match loc with
  | Some loc -> Ok (true, loc)
  | None -> Ok (false, get_preset_position ())

let setup_events () =
  let deviceorientation =
    Brr.Ev.Type.create (Jstr.v "deviceorientationabsolute")
  in
  let state = ref `Init in
  ignore
    (Brr.Ev.listen deviceorientation
       (fun ev ->
         let angle nm = Jv.to_float (Jv.get (Brr.Ev.as_type ev) nm) in
         let alpha = angle "alpha" in
         let beta = angle "beta" in
         let gamma = angle "gamma" in
         (match !state with
         | `Init -> ()
         | `Starting ->
             state := `Started;
             if beta < 90. then display_temporary_message "Raise your phone!"
         | `Started -> if beta >= 90. then remove_message ());
         let screen =
           Jv.to_float
             (Jv.get (Jv.get (Jv.get Jv.global "screen") "orientation") "angle")
         in
         current_orientation := { alpha; beta; gamma; screen })
       (Brr.Window.as_target Brr.G.window));
  ignore
    (Brr.Ev.listen Brr.Ev.keydown
       (fun ev ->
         match Jstr.to_string (Brr.Ev.Keyboard.code (Brr.Ev.as_type ev)) with
         | "ArrowLeft" ->
             current_orientation :=
               {
                 !current_orientation with
                 alpha = !current_orientation.alpha +. 5.;
               }
         | "ArrowRight" ->
             current_orientation :=
               {
                 !current_orientation with
                 alpha = !current_orientation.alpha -. 5.;
               }
         | "ArrowDown" ->
             current_orientation :=
               {
                 !current_orientation with
                 beta = max 60. (!current_orientation.beta -. 5.);
               }
         | "ArrowUp" ->
             current_orientation :=
               {
                 !current_orientation with
                 beta = min 120. (!current_orientation.beta +. 5.);
               }
         | _ -> ())
       (Brr.Window.as_target Brr.G.window));
  fun () -> state := `Starting

let main () =
  let tile_width = 2048 in
  let tile_height = tile_width in
  (* Check that we are close to a power of two *)
  assert (next_power_of_two tile_width 1 - tile_width < 16);
  display_message "Getting current location...";
  let* () = to_lwt wait_for_service_worker in
  let* use_geoloc, (lat, lon, angle) = to_lwt (get_position ~size:tile_width) in
  current_orientation := { alpha = angle; beta = 90.; gamma = 0.; screen = 0. };
  let start = setup_events () in
  display_message "Loading...";
  let* tile = Loader.f ~size:tile_width ~lat ~lon in
  if use_geoloc then Lwt.async (fun () -> Loader.prefetch ~size:6144 ~lat ~lon);
  let x = tile_width / 2 in
  let y = tile_height / 2 in
  let d = float (x - 1) /. 3600. in
  let tile_coord = { Points.lon = lon -. d; lat = lat -. d } in
  let tile_coord' = { Points.lon = lon +. d; lat = lat +. d } in
  let* points =
    let width = 3600 in
    let height = 3600 in
    let* points = Reader.read_file "data/points.geojson" in
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
             let x =
               truncate (((lon -. tile_coord.lon) *. float width) +. 0.5)
             in
             let y =
               truncate (((tile_coord'.lat -. lat) *. float height) +. 0.5)
             in
             (pt, (x, y))))
  in
  let points =
    List.filter
      (fun (_, (dst_x, dst_y)) ->
        Visibility.test tile ~src_x:x ~src_y:y ~dst_x ~dst_y)
      points
  in
  let height = tile.{y, x} in
  let canvas =
    Option.get (Brr.Document.find_el_by_id Brr.G.document (Jstr.v "canvas"))
  in
  let toggle_fullscreen _ =
    match Brr.Document.fullscreen_element Brr.G.document with
    | None ->
        ignore
          (Brr.El.request_fullscreen
             ~opts:
               (Brr.El.fullscreen_opts ~navigation_ui:Brr.El.Navigation_ui.hide
                  ())
             canvas)
    | Some _ -> ignore (Brr.Document.exit_fullscreen Brr.G.document)
  in
  ignore Brr.(Ev.listen Ev.click toggle_fullscreen (El.as_target canvas));
  let ctx =
    Option.get
      (Brr_canvas.Gl.get_context ~attrs:(Gl.Attrs.v ())
         (Brr_canvas.Canvas.of_el canvas))
  in
  remove_message ();
  start ();
  tri ~w:(tile_width - 2) ~h:(tile_height - 2) ~x ~y ~height ~points ~tile
    canvas ctx

let () =
  let open Brr_webworkers.Service_worker in
  ignore
    (Container.register
       (Container.of_navigator Brr.G.navigator)
       (Jstr.v "service_worker.bc.js"))

let () =
  Lwt.async (fun () ->
      Lwt.catch main (fun e ->
          (match e with Jv.Error e -> Brr.Console.error [ e ] | _ -> ());
          display_message (Printexc.to_string e);
          Lwt.fail e))
