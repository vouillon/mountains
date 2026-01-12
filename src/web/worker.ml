(* Web Worker for DEM decompression *)

module Wasm = struct
  let instance = ref None

  let create_memory pages =
    let opts = Jv.obj [| ("initial", Jv.of_int pages) |] in
    Jv.new' (Jv.get (Jv.get Jv.global "WebAssembly") "Memory") [| opts |]

  (* Static memories shared with WASM instance *)
  let high_mem = create_memory 25
  let low_mem = create_memory 25
  let target_mem = create_memory 526

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

let decode_dataset msg =
  let open Lwt.Syntax in
  let data = Jv.get msg "data" in
  let _dataset_id = Jv.get msg "id" in
  let target_size = Jv.get msg "size" |> Jv.to_int in

  (* Convert Jv to Tarray via buffer *)
  let data_ta =
    let buf = Jv.get data "buffer" in
    let buf = Brr.Tarray.Buffer.of_jv buf in
    Brr.Tarray.of_buffer Brr.Tarray.Uint8 buf
  in
  let data_ba = to_ba data_ta in

  (* Parse header *)
  let header_size = 28 in
  let width = get_uint32_le_ba data_ba 4 in
  let height = get_uint32_le_ba data_ba 8 in
  let high_size = get_uint32_le_ba data_ba 20 in
  let low_size = get_uint32_le_ba data_ba 24 in
  let dst_row = Jv.get msg "dst_row" |> Jv.to_int in
  let dst_col = Jv.get msg "dst_col" |> Jv.to_int in

  let high_compressed =
    Brr.Tarray.sub data_ta ~start:header_size ~stop:(header_size + high_size)
  in
  let low_compressed =
    Brr.Tarray.sub data_ta ~start:(header_size + high_size)
      ~stop:(header_size + high_size + low_size)
  in

  (* Ensure WASM loaded *)
  let* inst = Wasm.load () in
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
  ignore
    (Jv.apply func
       [|
         Jv.of_int 0;
         Jv.of_int 0;
         Jv.of_int 0;
         Jv.of_int dst_col;
         Jv.of_int (target_size - dst_row - height);
         Jv.of_int target_size;
         Jv.of_int target_size;
         Jv.of_int target_size;
         Jv.of_int width;
         Jv.of_int height;
       |]);

  (* Send back result - we don't send the bigarray, just a notification that it's done 
     and the worker keeps the state? 
     Wait, the worker has its own WASM memories. The main thread has different ones.
     We need to send the data back.
     
     Actually, the original implementation wrote directly into `target_mem`.
     Here, we are in a worker. We should copy the result from `target_mem` and transfer it back.
  *)
  (* Result is in target_mem *)
  (* We only need the relevant part, but the logic in dem_loader.ml accumulated
     multiple tiles into one big target_mem.
     
     If we want the worker to handle the *full* tile assembly (1200x1200x2 bytes = ~2.8MB), 
     we can do that.
     
     But wait, `decode_tile` in `dem_loader.ml` was accumulating into `target_mem` which was 
     shared across sub-tile calls.
     
     If we move to a worker, we should probably just return the decoded sub-tile? 
     OR, we can have the worker handle the full 1-degree tile assembly.
     
     The `load` function in `dem_loader.ml` iterates over sub-tiles and calls `decode_tile`.
     
     Alternative: The worker returns the decoded sub-tile data (width * height * 2 bytes).
     Then the main thread blits it?
     
     Better: The worker handles the full target buffer.
     The `load` function waits for all sub-tiles, then returns.
     
     So we can have the worker accumulate? 
     But the worker is stateless between messages? Or stateful?
     Web Workers maintain state.
     
     So we can have:
     1. "Start Job" -> allocates buffer in worker
     2. "Process Subtile" -> writes to worker buffer
     3. "Finish Job" -> transfers buffer back to main thread
     
     Let's implement a stateless "Process Subtile" first for simplicity?
     No, copying 2.8MB back and forth is expensive.
     
     Best approach: 
     Pass the *sub-tile* decoded data back?
     The sub-tiles are small segments.
     
     Actually, `decompress_simd` writes specifically to `target_mem` at offsets.
     
     If we use the worker, we want the worker to return the *Result* of the decompression.
     
     Let's try to replicate the `dem_loader` logic:
     The `target_mem` in `dem_loader.ml` was 526 pages (~33MB). It was used as a scratchpad 
     to assemble the final heightmap (3600x3600 pixels).
     
     If we do this in a worker, the worker needs that scratchpad.
     And then it transfers the *result* (Float32Array or Uint16Array) back.
     
     The `dem_loader.ml` `load` function:
     1. Allocates `target_mem` (reused).
     2. Clears it.
     3. Decoding loops write into it.
     4. Finally returns `heights` which is a reshape of `target_mem`.
     
     So the worker needs to:
     1. Accept a "Reset/Init" message for a new lat/lon/size query.
     2. Accept "Decode Subtile" messages.
     3. Accept "Get Result" message.
     
     Or simpler: 
     The `load` in `dem_loader` creates a promise for the whole job.
     It can spawn a worker (or use a persistent one).
     
     Let's use a persistent worker.
     
     Message types:
     { type: "init", size: int }
     { type: "decode", data: ArrayBuffer, dst_row: int, dst_col: int, ... }
     { type: "finish" } -> returns ArrayBuffer
     
  *)

  (* For now, let's keep it simple. We assume sequential usage or simple concurrency. 
     The `decode_simd` reads from compressed memories and writes to `target_mem`.
     
     We can add a simple protocol.
  *)
  Lwt.return ()

let () =
  let open Jv in
  let received_msg e =
    let msg = Jv.get (Brr.Ev.to_jv e) "data" in
    let type_ = Jv.get msg "type" |> Jv.to_string in

    match type_ with
    | "init" ->
        (* Reset target memory if needed, or just acknowledge *)
        let target_ba = Wasm.get_ba Wasm.target_mem in
        Bigarray.Array1.fill target_ba 0;
        ignore
          (Jv.call Jv.global "postMessage"
             [| obj [| ("type", of_string "init_done") |] |])
    | "decode" ->
        Lwt.async (fun () ->
            let open Lwt.Syntax in
            let* () = decode_dataset msg in
            ignore
              (Jv.call Jv.global "postMessage"
                 [| obj [| ("type", of_string "decode_done") |] |]);
            Lwt.return ())
    | "finish" ->
        let size = Jv.get msg "size" |> Jv.to_int in
        let byte_length = size * size * 2 in
        let target_ba = Wasm.get_ba Wasm.target_mem in
        let sub = Bigarray.Array1.sub target_ba 0 byte_length in

        (* Copy to a new ArrayBuffer to transfer it *)
        let result_ta = Brr.Tarray.create Brr.Tarray.Uint8 byte_length in
        let result_ba = to_ba result_ta in
        Bigarray.Array1.blit sub result_ba;

        let response =
          obj
            [|
              ("type", of_string "result");
              ("data", Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer result_ta));
            |]
        in
        ignore
          (Jv.call Jv.global "postMessage"
             [|
               response;
               Jv.of_array
                 (fun x -> x)
                 [| Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer result_ta) |];
             |])
    | _ -> ()
  in
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message received_msg
       (Brr.Ev.target_of_jv Jv.global))
