module Gl = Brr_canvas.Gl

let pi = 4.0 *. atan 1.0

let next_power_of_two n min_val =
  let rec loop x = if x >= n then x else loop (x * 2) in
  loop min_val

let log2 n =
  let rec loop x acc = if x <= 1 then acc else loop (x / 2) (acc + 1) in
  loop n 0

(* Rounds towards -infinity, unlike [truncate], so that a position's offset from
   the arcsecond its tile is anchored on stays inside that cell south of the
   equator. *)
let arcsec_floor coord = int_of_float (floor (coord *. 3600.))

type buffer = Buffer : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> buffer

let linearize2 a =
  Buffer
    Bigarray.(reshape_1 (genarray_of_array2 a) (Array2.dim1 a * Array2.dim2 a))

let linearize3 a =
  Buffer
    Bigarray.(
      reshape_1 (genarray_of_array3 a)
        (Array3.dim1 a * Array3.dim2 a * Array3.dim3 a))

let on_gpu_finished ctx =
  let t, u = Lwt.task () in
  let sync = Gl.fence_sync ctx Gl.sync_gpu_commands_complete 0 in
  let start_time = Brr.(Performance.now_ms G.performance) in
  let first = ref true in
  let rec check () =
    let flags = if !first then Gl.sync_flush_commands_bit else 0 in
    first := false;
    let status = Gl.client_wait_sync ctx sync flags 0 in
    if status = Gl.already_signaled || status = Gl.condition_satisfied then begin
      Gl.delete_sync ctx sync;
      Lwt.wakeup u ()
    end
    else if
      status = Gl.wait_failed
      || Brr.(Performance.now_ms G.performance) -. start_time > 3000.
    then begin
      Gl.delete_sync ctx sync;
      Lwt.wakeup u ()
    end
    else ignore (Brr.G.request_animation_frame (fun _ -> check ()))
  in
  check ();
  t

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
  List.iter (fun (loc, dim, typ, data) -> bind_attrib loc dim typ data) buffers;
  Gl.bind_vertex_array ctx None;
  Gl.bind_buffer ctx Gl.array_buffer None;
  Gl.bind_buffer ctx Gl.element_array_buffer None;
  gid

(* A compile or link failure is otherwise invisible: the draw calls are simply
   ignored and the canvas stays black. Report it and carry on. *)
let compile_shader ctx src typ =
  let sid = Gl.create_shader ctx typ in
  Gl.shader_source ctx sid (Jstr.v src);
  Gl.compile_shader ctx sid;
  if not (Jv.to_bool (Gl.get_shader_parameter ctx sid Gl.compile_status)) then
    Brr.Console.error
      [ Jstr.v "Shader compilation failed:"; Gl.get_shader_info_log ctx sid ];
  sid

type program_spec = {
  vertex_shader : string;
  fragment_shader : string;
  attributes : string list;
}

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
  if not (Jv.to_bool (Gl.get_program_parameter ctx pid Gl.link_status)) then
    Brr.Console.error
      [ Jstr.v "Shader program link failed:"; Gl.get_program_info_log ctx pid ];
  pid

let intersects (t_min_lon, t_min_lat, t_max_lon, t_max_lat)
    (v_min_lon, v_min_lat, _, _, v_max_lon, v_max_lat) =
  not
    (t_min_lon > v_max_lon || t_max_lon < v_min_lon || t_min_lat > v_max_lat
   || t_max_lat < v_min_lat)

let set_texture_params_nearest_clamp ctx target =
  Gl.tex_parameteri ctx target Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri ctx target Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri ctx target Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx target Gl.texture_wrap_t Gl.clamp_to_edge

let set_texture_params_linear_clamp ctx target =
  Gl.tex_parameteri ctx target Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri ctx target Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx target Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri ctx target Gl.texture_wrap_t Gl.clamp_to_edge

let set_texture_params_mipmap_repeat ctx target =
  Gl.tex_parameteri ctx target Gl.texture_min_filter Gl.linear_mipmap_linear;
  Gl.tex_parameteri ctx target Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri ctx target Gl.texture_wrap_s Gl.repeat;
  Gl.tex_parameteri ctx target Gl.texture_wrap_t Gl.repeat
