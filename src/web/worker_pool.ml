(* Shared Worker Pool *)
open Bigarray

type request_data =
  | DEM of Jv.t (* ArrayBuffer *)
  | CLC of Jv.t (* ArrayBuffer *)

type request =
  | Decode of request_data
  | Blend of { msg : Jv.t; transfer : Jv.t list }

(* Result types *)
type dem_result = { heights : (int, int8_unsigned_elt, c_layout) Array1.t }
type clc_result = Clc_data of Jv.t (* The result buffer/object from worker *)

type response =
  | ResultDEM of dem_result
  | ResultHDT of Brr.Tarray.float32
  | ResultCLC of clc_result
  | ResultBlend of Jv.t
  | BlendUnavailable
  | Error of string

type worker_handle = {
  worker : Brr_webworkers.Worker.t;
  pending : response Lwt.u Queue.t;
  mutable in_use : bool;
}

let pool_size = 4
let pool = ref [||]
let waiting_queue = Queue.create ()

(* A worker for the ring blends alone, and the blends waiting for it. In the
   pool they waited for a free worker behind the canopy's tiles, which nothing
   waits for -- 183 ms of a warm reload on a Pixel 8a, for a chain that takes
   150. *)
let blender = ref None
let blend_queue = Queue.create ()

let create_worker () =
  let worker = Brr_webworkers.Worker.create (Jstr.v "worker.bc.js") in
  let pending = Queue.create () in

  (* Listener *)
  let on_msg e =
    if Queue.is_empty pending then ()
    else
      let resolver = Queue.pop pending in
      let data = Brr.Ev.as_type e |> Brr_io.Message.Ev.data in
      let type_ = Jv.get data "type" |> Jv.to_string in
      match type_ with
      | "result_dem" ->
          let buffer_jv = Jv.get data "data" in
          let ta =
            Brr.Tarray.of_buffer Brr.Tarray.Uint8
              (Brr.Tarray.Buffer.of_jv buffer_jv)
          in
          let ba = Brr.Tarray.to_bigarray1 ta in
          Lwt.wakeup resolver (ResultDEM { heights = ba })
      | "result_hdt" ->
          let buffer_jv = Jv.get data "data" in
          Lwt.wakeup resolver
            (ResultHDT
               (Brr.Tarray.of_buffer Brr.Tarray.Float32
                  (Brr.Tarray.Buffer.of_jv buffer_jv)))
      | "result_clc" -> Lwt.wakeup resolver (ResultCLC (Clc_data data))
      | "result_blend" -> Lwt.wakeup resolver (ResultBlend (Jv.get data "data"))
      | "blend_unavailable" -> Lwt.wakeup resolver BlendUnavailable
      | "error" ->
          let msg = Jv.get data "message" |> Jv.to_string in
          Lwt.wakeup resolver (Error msg)
      | _ ->
          (* The resolver has been popped: dropping it here would leave the
             request pending for ever and shift every later reply by one. *)
          Lwt.wakeup resolver
            (Error (Printf.sprintf "Unexpected worker message %S" type_))
  in
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message on_msg
       (Brr_webworkers.Worker.as_target worker));
  { worker; pending; in_use = false }

let init () =
  if Array.length !pool = 0 then begin
    pool := Array.init pool_size (fun _ -> create_worker ());
    let w = create_worker () in
    (* No reply: nothing is queued for it. *)
    Brr_webworkers.Worker.post w.worker
      (Jv.obj [| ("type", Jv.of_string "preload_blend") |]);
    blender := Some w
  end

let acquire () =
  let open Lwt.Syntax in
  init ();

  (* Find available worker *)
  let available = Array.find_opt (fun w -> not w.in_use) !pool in
  match available with
  | Some w ->
      w.in_use <- true;
      Lwt.return w
  | None ->
      (* Wait for a worker *)
      let t, u = Lwt.task () in
      Queue.push u waiting_queue;
      let* w = t in
      Lwt.return w

let acquire_blender () =
  init ();
  let w = Option.get !blender in
  if not w.in_use then begin
    w.in_use <- true;
    Lwt.return w
  end
  else begin
    let t, u = Lwt.task () in
    Queue.push u blend_queue;
    t
  end

let release w =
  let queue =
    match !blender with Some b when b == w -> blend_queue | _ -> waiting_queue
  in
  if not (Queue.is_empty queue) then
    (* Pass worker directly to next waiter *)
    let u = Queue.pop queue in
    Lwt.wakeup u w
  else w.in_use <- false

let post w req =
  let msg, transfer =
    let open Jv in
    match req with
    | Decode (DEM data) ->
        ( obj [| ("type", of_string "decode_dem"); ("data", data) |],
          [ Obj.magic data ] )
    | Decode (CLC data) ->
        ( obj [| ("type", of_string "decode_clc"); ("data", data) |],
          [ Obj.magic data ] )
    | Blend { msg; transfer } ->
        ( obj [| ("type", of_string "blend"); ("data", msg) |],
          List.map Obj.magic transfer )
  in
  (* Replies are matched to requests by order, so the resolver is enqueued only
     once the message is on its way: a synchronous post failure (a
     [DataCloneError], say) would otherwise leave an orphan at the head of the
     queue and shift every later reply by one. *)
  Brr_webworkers.Worker.post w.worker msg
    ~opts:(Brr_io.Message.opts ~transfer ());
  let t, u = Lwt.task () in
  Queue.push u w.pending;
  t
