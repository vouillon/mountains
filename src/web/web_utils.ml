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

(* Not [Brr.Performance.measure], which never passes the measure's name to
   JavaScript (brr 0.0.9, and master as of 2026-11): its start mark becomes the
   name instead. The options form takes the [now_ms] readings the code already
   has, where marks would need a unique name per span. [detail.devtools] is
   Chrome's extension that puts the span on a track of its own. *)
let measure ~track name start =
  let perf = Brr.Performance.to_jv Brr.G.performance in
  let devtools =
    Jv.obj
      [|
        ("dataType", Jv.of_string "track-entry");
        ("track", Jv.of_string track);
        ("trackGroup", Jv.of_string "Mountains");
      |]
  in
  ignore
    (Jv.call perf "measure"
       [|
         Jv.of_string name;
         Jv.obj
           [|
             ("start", Jv.of_float start);
             ("end", Jv.of_float (Brr.Performance.now_ms Brr.G.performance));
             ("detail", Jv.obj [| ("devtools", devtools) |]);
           |];
       |])

let timed ~track name f =
  let start = Brr.Performance.now_ms Brr.G.performance in
  let r = f () in
  measure ~track name start;
  r

let timed_lwt ~track name f =
  let start = Brr.Performance.now_ms Brr.G.performance in
  Lwt.map
    (fun r ->
      measure ~track name start;
      r)
    (f ())

(* The data cache, opened once: [None] where there is no Cache Storage. *)
let data_cache =
  lazy
    (if Jv.is_none (Jv.get Jv.global "caches") then Fut.return None
     else
       Fut.map
         (function Ok c -> Some c | Error _ -> None)
         (Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ())
            (Jstr.v Data_cache.name)))

let fetch_tile ?(serve_not_found = false) url =
  let open Fut.Syntax in
  let* cached =
    let* cache = Lazy.force data_cache in
    match cache with
    | None -> Fut.return None
    | Some cache ->
        let+ r = Brr_io.Fetch.Cache.match' cache (Brr_io.Fetch.Request.v url) in
        (* A failed lookup is a miss, as it is for the service worker. *)
        Result.value ~default:None r
  in
  match cached with
  | Some r
    when Brr_io.Fetch.Response.ok r
         || (serve_not_found && Brr_io.Fetch.Response.status r = 404) ->
      Fut.return (Ok r)
  | Some _ | None ->
      let* r = Brr_io.Fetch.url url in
      (* No service worker in charge -- a first visit, before it has installed
         and claimed the page, which is no longer waited for: the tile is
         stored here as it would have stored it, so that it is there next
         time. A 404 too, which it serves back for the tiles that ask. *)
      let controlled =
        let c = Jv.get (Jv.get Jv.global "navigator") "serviceWorker" in
        Jv.is_some c && Jv.is_some (Jv.get c "controller")
      in
      (if not controlled then
         match r with
         | Ok resp
           when Brr_io.Fetch.Response.ok resp
                || Brr_io.Fetch.Response.status resp = 404 ->
             (* Cloned now, before the caller reads the body. *)
             let copy = Brr_io.Fetch.Response.of_response resp in
             Fut.await (Lazy.force data_cache) (function
               | Some cache ->
                   Fut.await
                     (Brr_io.Fetch.Cache.put cache
                        (Brr_io.Fetch.Request.v url)
                        copy)
                     ignore
               | None -> ())
         | Ok _ | Error _ -> ());
      Fut.return r

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

(* Compile and link, asking nothing: every status query waits for the GPU
   process to be done with the program, so asked one program at a time, the
   programs compile one after the other. The shaders are kept for [check]'s
   logs. *)
let submit ctx p =
  let shader src typ =
    let sid = Gl.create_shader ctx typ in
    Gl.shader_source ctx sid (Jstr.v src);
    Gl.compile_shader ctx sid;
    sid
  in
  let vid = shader p.vertex_shader Gl.vertex_shader in
  let fid = shader p.fragment_shader Gl.fragment_shader in
  let pid = Gl.create_program ctx in
  Gl.attach_shader ctx pid vid;
  Gl.attach_shader ctx pid fid;
  List.iteri
    (fun i attr -> Gl.bind_attrib_location ctx pid i (Jstr.v attr))
    p.attributes;
  Gl.link_program ctx pid;
  (pid, vid, fid)

(* Waits for the program, says why it failed if it did, and lets its shaders
   go. *)
let check ctx (pid, vid, fid) =
  if not (Jv.to_bool (Gl.get_program_parameter ctx pid Gl.link_status)) then begin
    List.iter
      (fun sid ->
        if not (Jv.to_bool (Gl.get_shader_parameter ctx sid Gl.compile_status))
        then
          Brr.Console.error
            [
              Jstr.v "Shader compilation failed:";
              Gl.get_shader_info_log ctx sid;
            ])
      [ vid; fid ];
    Brr.Console.error
      [ Jstr.v "Shader program link failed:"; Gl.get_program_info_log ctx pid ]
  end;
  Gl.detach_shader ctx pid vid;
  Gl.detach_shader ctx pid fid;
  Gl.delete_shader ctx vid;
  Gl.delete_shader ctx fid;
  pid

(* Programs submitted ahead of their first use, by spec. *)
let prepared = ref []

let prepare_programs ctx specs =
  (* Where the driver can, it links them side by side. *)
  ignore (Gl.get_extension ctx (Jstr.v "KHR_parallel_shader_compile"));
  (* Any left are from a context lost since. *)
  prepared := List.map (fun p -> (p, submit ctx p)) specs;
  (* On their way to the GPU process, which links them while the page goes
     on: on a desktop's first visit, 17 programs took it 5.6 s. *)
  Gl.flush ctx

let programs_ready ctx =
  (* A fence after them, polled without waiting, as [on_gpu_finished] does.
     Not COMPLETION_STATUS_KHR: where the GPU process links them one after
     the other, any query waits behind every link queued before it -- 6 s on a
     desktop's first visit, which the first poll spent blocked. *)
  let t, u = Lwt.wait () in
  let sync = Gl.fence_sync ctx Gl.sync_gpu_commands_complete 0 in
  Gl.flush ctx;
  let rec poll () =
    let status = Gl.client_wait_sync ctx sync 0 0 in
    if
      status = Gl.already_signaled
      || status = Gl.condition_satisfied
      || status = Gl.wait_failed || Gl.is_context_lost ctx
    then begin
      Gl.delete_sync ctx sync;
      Lwt.wakeup u ()
    end
    else ignore (Brr.G.set_timeout ~ms:16 poll)
  in
  poll ();
  t

let create_program ctx p =
  match List.assq_opt p !prepared with
  | Some submitted ->
      prepared := List.filter (fun (q, _) -> q != p) !prepared;
      check ctx submitted
  | None -> check ctx (submit ctx p)

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
