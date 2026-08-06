(* Drives [blend.wat], the hand-written wasm port of [Blend_core.run].

   The viewer itself is compiled to wasm, so this is not "moving the work into
   wasm" -- it was already there. What it buys is linear memory: bigarray element
   access is not compiled to a plain load by wasm_of_ocaml today, and the blend
   touches some fifty million elements per l13 block. See PLAN.md.

   Same shape as [Worker.Wasm]: one imported memory that the caller carves up,
   integer pointers passed to the export, results copied back out. *)

open Bigarray

let exports = ref None

(* The blend needs ~29 MB for an l13 block over the base tile; start small and
   let [ensure_memory_bytes] grow it, exactly as the worker does. *)
let memory =
  let opts =
    Jv.obj [| ("initial", Jv.of_int 16); ("maximum", Jv.of_int 4096) |]
  in
  Jv.new' (Jv.get (Jv.get Jv.global "WebAssembly") "Memory") [| opts |]

let ensure_memory_bytes needed =
  let current_bytes =
    Jv.get (Jv.get memory "buffer") "byteLength" |> Jv.to_int
  in
  if needed > current_bytes then begin
    let current_pages = (current_bytes + 65535) / 65536 in
    let needed_pages = (needed + 65535) / 65536 in
    let delta = needed_pages - current_pages in
    if delta > 0 then ignore (Jv.call memory "grow" [| Jv.of_int delta |])
  end

(* Rebuilt after every possible growth: growing the memory detaches the old
   buffer, and every view over it with it. *)
let view kind =
  Brr.Tarray.of_buffer kind (Brr.Tarray.Buffer.of_jv (Jv.get memory "buffer"))

let u8 () = Brr.Tarray.to_bigarray1 (view Brr.Tarray.Uint8)
let f32 () = Brr.Tarray.to_bigarray1 (view Brr.Tarray.Float32)
let f64 () = Brr.Tarray.to_bigarray1 (view Brr.Tarray.Float64)

let to_lwt f =
  let t, u = Lwt.task () in
  ( Fut.await f @@ fun v ->
    match v with
    | Ok v -> Lwt.wakeup u v
    | Error err -> Lwt.wakeup_exn u (Jv.Error err) );
  t

let load () =
  let open Lwt.Syntax in
  let imports = Jv.obj [| ("env", Jv.obj [| ("memory", memory) |]) |] in
  let* resp = to_lwt @@ Brr_io.Fetch.url (Jstr.v "blend.wasm") in
  let* res =
    to_lwt
      (Fut.of_promise
         ~ok:(fun x -> x)
         (Jv.call
            (Jv.get Jv.global "WebAssembly")
            "instantiateStreaming"
            [| Brr_io.Fetch.Response.to_jv resp; imports |]))
  in
  exports := Some (Jv.get (Jv.get res "instance") "exports");
  Lwt.return_unit

(* Kicked off at startup so the module is ready well before the first blend,
   which cannot happen until a DEM tile and a refinement block have both been
   fetched over the network. [run] falls back to [Blend_core] if it somehow is
   not, so nothing depends on winning that race. *)
let start () =
  Lwt.dont_wait load (fun e ->
      Brr.Console.error
        [ Jstr.v "blend.wasm failed to load: "; Jstr.v (Printexc.to_string e) ])

let align8 x = (x + 7) land lnot 7

let run (p : Blend_core.params) ~samples ~win =
  (* The wat hoists one source row pair per source row and precomputes the
     column indices once, both of which assume the two grids share axes. A ring
     on a projected grid over the graticule-aligned surface beneath it does not,
     and takes the OCaml path -- 1024^2, which measured ~65 ms there before it
     was ported, against a fetch of several seconds. *)
  match if Blend_core.axis_aligned p then !exports else None with
  | None -> Blend_core.run p ~samples ~win
  | Some exports ->
      let g = Blend_core.geometry p in
      let size = p.size in
      let n = size * size in
      (* [samples] first, so that a float32 view of the whole buffer has it at
         element zero. *)
      let samples_ptr = 0 in
      let win_len = g.win_rows * g.win_cols * 2 in
      let win_ptr = align8 (samples_ptr + (n * 4)) in
      let out_ptr = align8 (win_ptr + win_len) in
      let dist_ptr = align8 (out_ptr + (n * 2)) in
      let aux_ptr = align8 (dist_ptr + n) in
      let aux_len = (size * 20) + ((g.n_cols + 1) * 24) in
      let result_ptr = align8 (aux_ptr + aux_len) in
      ensure_memory_bytes (result_ptr + 24);
      Array1.blit samples (Array1.sub (f32 ()) 0 n);
      Array1.blit win (Array1.sub (u8 ()) win_ptr win_len);
      let ok =
        Jv.to_int
          (Jv.apply (Jv.get exports "blend")
             [|
               Jv.of_int samples_ptr;
               Jv.of_int win_ptr;
               Jv.of_int out_ptr;
               Jv.of_int dist_ptr;
               Jv.of_int aux_ptr;
               Jv.of_int result_ptr;
               Jv.of_int size;
               Jv.of_int p.src_size;
               Jv.of_int g.win_cols;
               Jv.of_int g.win_rows;
               Jv.of_int g.col_lo;
               Jv.of_int g.row_lo;
               Jv.of_int g.n_cols;
               Jv.of_int g.n_rows;
               Jv.of_float p.to_src.Affine.a;
               Jv.of_float p.to_src.Affine.c;
               Jv.of_float p.to_src.Affine.e;
               Jv.of_float p.to_src.Affine.f;
               Jv.of_float p.src_height_scale;
               Jv.of_float p.src_height_offset;
               Jv.of_float p.fade_x;
               Jv.of_float p.fade_y;
             |])
      in
      if ok = 0 then None
      else begin
        let r = f64 () in
        let i = result_ptr / 8 in
        let data = Array1.create int8_unsigned c_layout (n * 2) in
        Array1.blit (Array1.sub (u8 ()) out_ptr (n * 2)) data;
        Some
          {
            Blend_core.data;
            height_scale = Array1.unsafe_get r i;
            height_offset = Array1.unsafe_get r (i + 1);
            range = Array1.unsafe_get r (i + 2);
          }
      end
