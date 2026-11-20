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

(* Where everything goes in the module's memory, and the arguments of its two
   exports: shared by [run] and the decode workers ([run_in_worker]), which
   make the same call on a memory of their own. *)
type plan = {
  n : int;  (** samples *)
  win_len : int;
  samples_ptr : int;
  win_ptr : int;
  out_ptr : int;
  result_ptr : int;
  total : int;  (** bytes of memory needed *)
  warp : float array option;  (** for [set_warp], on turned axes *)
  args : float array;  (** for [blend] *)
}

let plan (p : Blend_core.params) =
  (* The wat hoists one source row pair per source row and precomputes the
     column indices once, both of which assume the two grids share axes. A
     ring on a projected grid over the graticule-aligned surface beneath it
     does not: it is given the whole map, and pays a full bilinear per sample
     -- 1024^2, which took 140 ms in OCaml. *)
  let warp =
    if Blend_core.axis_aligned p then None
    else
      let w = p.to_src in
      let m = w.Warp.lin in
      Some
        [|
          m.Affine.a;
          m.b;
          m.c;
          m.d;
          m.e;
          m.f;
          w.xuu;
          w.xuv;
          w.xvv;
          w.yuu;
          w.yuv;
          w.yvv;
        |]
  in
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
  {
    n;
    win_len;
    samples_ptr;
    win_ptr;
    out_ptr;
    result_ptr;
    total = result_ptr + 24;
    warp;
    args =
      [|
        float samples_ptr;
        float win_ptr;
        float out_ptr;
        float dist_ptr;
        float aux_ptr;
        float result_ptr;
        float size;
        float p.src_size;
        float g.win_cols;
        float g.win_rows;
        float g.col_lo;
        float g.row_lo;
        float g.n_cols;
        float g.n_rows;
        p.to_src.Warp.lin.Affine.a;
        p.to_src.Warp.lin.Affine.c;
        p.to_src.Warp.lin.Affine.e;
        p.to_src.Warp.lin.Affine.f;
        p.src_height_scale;
        p.src_height_offset;
        p.fade_x;
        p.fade_y;
      |];
  }

let run (p : Blend_core.params) ~samples ~win =
  match !exports with
  (* A module from before it took turned axes, out of a stale cache, is no
     better than none for them. *)
  | Some exports
    when Blend_core.axis_aligned p || Jv.is_some (Jv.get exports "set_warp") ->
      let pl = plan p in
      Option.iter
        (fun w ->
          ignore
            (Jv.apply (Jv.get exports "set_warp") (Array.map Jv.of_float w)))
        pl.warp;
      ensure_memory_bytes pl.total;
      Array1.blit samples (Array1.sub (f32 ()) 0 pl.n);
      Array1.blit win (Array1.sub (u8 ()) pl.win_ptr pl.win_len);
      let ok =
        Jv.to_int
          (Jv.apply (Jv.get exports "blend") (Array.map Jv.of_float pl.args))
      in
      if ok = 0 then None
      else begin
        let r = f64 () in
        let i = pl.result_ptr / 8 in
        let data = Array1.create int8_unsigned c_layout (pl.n * 2) in
        Array1.blit (Array1.sub (u8 ()) pl.out_ptr (pl.n * 2)) data;
        Some
          {
            Blend_core.data;
            height_scale = Array1.unsafe_get r i;
            height_offset = Array1.unsafe_get r (i + 1);
            range = Array1.unsafe_get r (i + 2);
          }
      end
  | Some _ | None -> Blend_core.run p ~samples ~win

(* The chain of a location's rings, blended in a decode worker in one request:
   the main thread is busy baking the base meanwhile, and a request per ring
   left each answer waiting behind that bake before the next ring could even be
   asked for (the first ring's took 330 ms on a Pixel 8a, against a 90 ms
   call). Each plan after the first stands on its predecessor's output: the
   worker cuts its window from that output and puts its height scale and offset
   into the plan's arguments (18 and 19). *)
let run_chain_in_worker rings ~win =
  let open Lwt.Syntax in
  (* Copied here, a plain memcpy, and handed over: a block may be blended
     again, onto a surface that changed, so its own samples stay. Cloned by
     postMessage instead, the copies took the worker 60 ms to receive. *)
  let copies = ref [] in
  let ring ((p : Blend_core.params), samples) =
    let pl = plan p in
    let floats l = Jv.of_array Jv.of_float l in
    let copy =
      Jv.call (Brr.Tarray.to_jv (Brr.Tarray.of_bigarray1 samples)) "slice" [||]
    in
    copies := Jv.get copy "buffer" :: !copies;
    Jv.obj
      [|
        ("samples", copy);
        ( "layout",
          floats
            (Array.map float
               [|
                 pl.n;
                 pl.win_len;
                 pl.samples_ptr;
                 pl.win_ptr;
                 pl.out_ptr;
                 pl.result_ptr;
                 pl.total;
               |]) );
        ("warp", match pl.warp with Some w -> floats w | None -> Jv.null);
        ("args", floats pl.args);
      |]
  in
  (* Handed over: it was cut for this request. *)
  let win_buf =
    Brr.Tarray.Buffer.to_jv (Brr.Tarray.buffer (Brr.Tarray.of_bigarray1 win))
  in
  let msg = Jv.obj [| ("rings", Jv.of_list ring rings); ("win", win_buf) |] in
  let* w = Worker_pool.acquire_blender () in
  let* res =
    Lwt.finalize
      (fun () ->
        Worker_pool.post w
          (Worker_pool.Blend { msg; transfer = win_buf :: !copies }))
      (fun () ->
        Worker_pool.release w;
        Lwt.return_unit)
  in
  match res with
  | Worker_pool.ResultBlend results ->
      Lwt.return
        (Some
           (List.map
              (fun r ->
                if Jv.is_none r then None
                else
                  Some
                    ( {
                        Blend_core.data =
                          Brr.Tarray.to_bigarray1
                            (Brr.Tarray.of_buffer Brr.Tarray.Uint8
                               (Brr.Tarray.Buffer.of_jv (Jv.get r "data")));
                        height_scale = Jv.to_float (Jv.get r "height_scale");
                        height_offset = Jv.to_float (Jv.get r "height_offset");
                        range = Jv.to_float (Jv.get r "range");
                      },
                      Jv.to_float (Jv.get r "ms") ))
              (Jv.to_jv_list results)))
  (* No module in the worker, or one too old for these axes. *)
  | Worker_pool.BlendUnavailable | Worker_pool.ResultDEM _
  | Worker_pool.ResultHDT _ | Worker_pool.ResultCLC _ | Worker_pool.Error _ ->
      Lwt.return None
