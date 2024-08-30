type t = { mutable pos : int; buffer : Brr.Tarray.uint8 }

let ( let* ) = Lwt.bind

let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
  t

let select ~lat ~lon f =
  let open Brr_io.Fetch in
  let* resp =
    to_lwt
    @@ url
         (Jstr.v
            (Printf.sprintf
               "data/Copernicus_DSM_COG_10_N%02d_00_E%03d_00_DEM.tif" lat lon))
  in
  let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
  f { pos = 0; buffer = Brr.Tarray.(of_buffer Uint8 buf) }

let seek b pos = b.pos <- pos

let read_string b n =
  let pos = b.pos in
  b.pos <- pos + n;
  Lwt.return Brr.Tarray.(to_string (sub ~start:pos ~stop:(pos + n) b.buffer))

type chunk = Brr.Tarray.uint8

let read_chunk b n =
  let pos = b.pos in
  b.pos <- pos + n;
  Lwt.return Brr.Tarray.(sub ~start:pos ~stop:(pos + n) b.buffer)

external inflate_impl : Brr.Tarray.uint8 -> Jv.Promise.t = "inflate"

let inflate s b =
  let* s' =
    to_lwt
      (Fut.of_promise
         ~ok:(Obj.magic : Jv.t -> Brr.Tarray.uint8)
         (inflate_impl s))
  in
  let s' = Brr.Tarray.(to_string s') in
  assert (String.length s' = Bytes.length b);
  String.blit s' 0 b 0 (String.length s');
  Lwt.return ()

let read_file f =
  let open Brr_io.Fetch in
  let* resp = to_lwt @@ url (Jstr.v f) in
  let* buf = to_lwt (Body.array_buffer (Response.as_body resp)) in
  Lwt.return Brr.Tarray.(to_string (of_buffer Uint8 buf))
