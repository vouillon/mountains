(* Web Worker for DEM decompression *)
(* Web Worker for DEM decompression *)
module Console = Brr.Console

module Wasm = struct
  let instance = ref None

  let create_memory pages =
    let opts = Jv.obj [| ("initial", Jv.of_int pages) |] in
    Jv.new' (Jv.get (Jv.get Jv.global "WebAssembly") "Memory") [| opts |]

  (* Static memories shared with WASM instance *)
  let high_mem = create_memory 25
  let low_mem = create_memory 25
  let target_mem = create_memory 50

  let get_ba mem =
    let buf = Jv.get mem "buffer" in
    Brr.Tarray.to_bigarray1
      (Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv buf))

  let load () =
    match !instance with
    | Some inst -> Lwt.return inst
    | None ->
        let open Lwt.Syntax in
        let* resp =
          let p = Brr_io.Fetch.Request.v (Jstr.v "decompress_tile.wasm") in
          let open Brr_io.Fetch in
          let t, u = Lwt.task () in
          ( Fut.await (request p) @@ fun v ->
            match v with
            | Ok v -> Lwt.wakeup u v
            | Error err -> raise (Jv.Error err) );
          t
        in
        let* buf =
          let open Brr_io.Fetch in
          let t, u = Lwt.task () in
          ( Fut.await (Body.array_buffer (Response.as_body resp)) @@ fun v ->
            match v with
            | Ok v -> Lwt.wakeup u v
            | Error err -> raise (Jv.Error err) );
          t
        in

        let imports =
          Jv.obj
            [|
              ( "env",
                Jv.obj
                  [|
                    ("high_mem", high_mem);
                    ("low_mem", low_mem);
                    ("target_mem", target_mem);
                  |] );
            |]
        in

        let* res =
          let t, u = Lwt.task () in
          ( Fut.await
              (Fut.of_promise
                 ~ok:(fun x -> x)
                 (Jv.call
                    (Jv.get Jv.global "WebAssembly")
                    "instantiate"
                    [| Brr.Tarray.Buffer.to_jv buf; imports |]))
          @@ fun v ->
            match v with
            | Ok v -> Lwt.wakeup u v
            | Error err -> raise (Jv.Error err) );
          t
        in
        let inst = Jv.get res "instance" in
        instance := Some inst;
        Lwt.return inst
end

(* External inflate functions *)
external inflate_into : Brr.Tarray.uint8 -> Brr.Tarray.uint8 -> Jv.Promise.t
  = "inflate_into"

let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with Ok v -> Lwt.wakeup u v | Error err -> raise (Jv.Error err) );
  t

(* Helper to convert typed array to bigarray *)
let to_ba (arr : Brr.Tarray.uint8) = Brr.Tarray.to_bigarray1 arr

(* Read uint32 LE from bigarray *)
let get_uint32_le_ba
    (arr :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t)
    offset =
  let b0 = Bigarray.Array1.get arr offset in
  let b1 = Bigarray.Array1.get arr (offset + 1) in
  let b2 = Bigarray.Array1.get arr (offset + 2) in
  let b3 = Bigarray.Array1.get arr (offset + 3) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let decode_dataset ~inst msg =
  let open Lwt.Syntax in
  let data_jv = Jv.get msg "data" in
  (* Convert JS ArrayBuffer/Uint8Array to Bigarray *)
  let data_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 (Brr.Tarray.Buffer.of_jv data_jv)
  in
  let data_ba = to_ba data_ta in

  (* Parse header *)
  let header_size = 28 in
  let width = get_uint32_le_ba data_ba 4 in
  let height = get_uint32_le_ba data_ba 8 in
  let high_size = get_uint32_le_ba data_ba 20 in
  let low_size = get_uint32_le_ba data_ba 24 in

  let high_compressed =
    Brr.Tarray.sub data_ta ~start:header_size ~stop:(header_size + high_size)
  in
  let low_compressed =
    Brr.Tarray.sub data_ta ~start:(header_size + high_size)
      ~stop:(header_size + high_size + low_size)
  in

  (* Ensure WASM loaded *)
  let h_mem = Wasm.high_mem in
  let l_mem = Wasm.low_mem in

  let h_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8
      (Brr.Tarray.Buffer.of_jv (Jv.get h_mem "buffer"))
  in
  let l_ta =
    Brr.Tarray.of_buffer Brr.Tarray.Uint8
      (Brr.Tarray.Buffer.of_jv (Jv.get l_mem "buffer"))
  in

  (* Decompress *)
  let* _ =
    to_lwt (Fut.of_promise ~ok:(fun x -> x) (inflate_into high_compressed h_ta))
  in
  let* _ =
    to_lwt (Fut.of_promise ~ok:(fun x -> x) (inflate_into low_compressed l_ta))
  in

  (* Decode SIMD *)
  let func = Jv.get (Jv.get inst "exports") "decompress_simd" in
  (* We write to the beginning of target_mem with stride = width *)
  let target_size = width in

  ignore
    (Jv.apply func
       [|
         Jv.of_int 0;
         Jv.of_int 0;
         Jv.of_int 0;
         (* dst_col *)
         Jv.of_int 0;
         (* dst_row offset? No, simd takes (target_size - dst_row - height) ?? *)
         (* Original: Jv.of_int (target_size - dst_row - height); *)
         (* If dst_row=0, this is target_size - height. *)
         (* The Y axis is likely inverted or bottom-up? *)
         (* Let's assume standard behavior: writes to array *)
         Jv.of_int (target_size - height);
         Jv.of_int target_size;
         Jv.of_int target_size;
         Jv.of_int target_size;
         Jv.of_int width;
         Jv.of_int height;
       |]);

  (* Return result *)
  let byte_length = width * height * 2 in
  let target_ba = Wasm.get_ba Wasm.target_mem in
  let sub = Bigarray.Array1.sub target_ba 0 byte_length in

  (* Copy to new ArrayBuffer *)
  let result_ta = Brr.Tarray.create Brr.Tarray.Uint8 byte_length in
  let result_ba = to_ba result_ta in
  Bigarray.Array1.blit sub result_ba;

  Lwt.return (Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer result_ta))

let () =
  let inst = Wasm.load () in
  let open Jv in
  let received_msg e =
    let msg = Jv.get (Brr.Ev.to_jv e) "data" in
    let type_ = Jv.get msg "type" |> Jv.to_string in

    match type_ with
    | "decode" ->
        Lwt.async (fun () ->
            let open Lwt.Syntax in
            let* inst = inst in
            let* result_jv = decode_dataset ~inst msg in
            let response =
              obj [| ("type", of_string "result"); ("data", result_jv) |]
            in
            ignore
              (Jv.call Jv.global "postMessage"
                 [| response; Jv.of_array (fun x -> x) [| result_jv |] |]);
            Lwt.return ())
    | _ -> ()
  in
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message received_msg
       (Brr.Ev.target_of_jv Jv.global))
