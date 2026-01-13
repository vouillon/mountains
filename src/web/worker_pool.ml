(* Shared Worker Pool *)
open Bigarray

type request_data =
  | DEM of Jv.t (* ArrayBuffer *)
  | CLC of Jv.t (* ArrayBuffer *)

type request = Decode of request_data

(* Result types *)
type dem_result = { heights : (int, int8_unsigned_elt, c_layout) Array1.t }
type clc_result = Clc_data of Jv.t (* The result buffer/object from worker *)

type response =
  | ResultDEM of dem_result
  | ResultCLC of clc_result
  | Error of string

type worker_handle = {
  worker : Brr_webworkers.Worker.t;
  pending : response Lwt.u Queue.t;
  mutable in_use : bool;
}

let pool_size = 4
let pool = ref [||]
let waiting_queue = Queue.create ()

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
      | "result_clc" -> Lwt.wakeup resolver (ResultCLC (Clc_data data))
      | "error" ->
          let msg = Jv.get data "message" |> Jv.to_string in
          Lwt.wakeup resolver (Error msg)
      | _ -> ()
  in
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message on_msg
       (Brr_webworkers.Worker.as_target worker));
  { worker; pending; in_use = false }

let init () =
  if Array.length !pool = 0 then
    pool := Array.init pool_size (fun _ -> create_worker ())

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

let release w =
  if not (Queue.is_empty waiting_queue) then
    (* Pass worker directly to next waiter *)
    let u = Queue.pop waiting_queue in
    Lwt.wakeup u w
  else w.in_use <- false

let post w req =
  let t, u = Lwt.task () in
  Queue.push u w.pending;
  let msg, transfer =
    let open Jv in
    match req with
    | Decode (DEM data) ->
        ( obj [| ("type", of_string "decode_dem"); ("data", data) |],
          [ Obj.magic data ] )
    | Decode (CLC data) ->
        ( obj [| ("type", of_string "decode_clc"); ("data", data) |],
          [ Obj.magic data ] )
  in
  Brr_webworkers.Worker.post w.worker msg
    ~opts:(Brr_io.Message.opts ~transfer ());
  t
