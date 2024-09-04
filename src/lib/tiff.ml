let ( let* ) = Lwt.bind

module type READER = sig
  type t

  val select : lat:int -> lon:int -> (t -> 'a Lwt.t) -> 'a Lwt.t
  val seek : t -> int -> unit
  val read_string : t -> int -> string Lwt.t

  type chunk

  val read_chunk : t -> int -> chunk Lwt.t
  val inflate : chunk -> bytes -> unit Lwt.t
end

type t = {
  width : int;
  height : int;
  tile_width : int;
  tile_height : int;
  tile_offsets : int32 array;
  tile_byte_counts : int32 array;
}

module Make (R : READER) = struct
  let read_int32 ch =
    let* s = R.read_string ch 4 in
    Lwt.return (String.get_int32_le s 0)

  let read_uint16 ch =
    let* s = R.read_string ch 2 in
    Lwt.return (String.get_uint16_le s 0)

  type entry = { typ : int; count : int32; offset : int32 }

  let debug = false

  let read_ifd_entry ch =
    let* tag = read_uint16 ch in
    let* typ = read_uint16 ch in
    let* count = read_int32 ch in
    let* offset = read_int32 ch in
    if debug then Format.eprintf "%d %d %ld %ld@." tag typ count offset;
    Lwt.return (tag, { typ; count; offset })

  module IntMap = Map.Make (Int)

  let rec iter min max f =
    if min = max then Lwt.return ()
    else
      let* () = f min in
      iter (min + 1) max f

  let read_ifd ch offset =
    R.seek ch (Int32.to_int offset);
    let* n = read_uint16 ch in
    if debug then Format.eprintf "%d@." n;
    let m = ref IntMap.empty in
    let* () =
      iter 0 n @@ fun _ ->
      let* tag, v = read_ifd_entry ch in
      m := IntMap.add tag v !m;
      Lwt.return ()
    in
    Lwt.return !m

  let ifd_uint16 tag ifd =
    let v = IntMap.find tag ifd in
    assert (v.typ = 3);
    assert (v.count = 1l);
    Int32.to_int v.offset

  let ifd_int32s tag ifd ch =
    let v = IntMap.find tag ifd in
    let count = Int32.to_int v.count in
    assert (v.typ = 4);
    assert (count > 1);
    let a = Array.make count 0l in
    R.seek ch (Int32.to_int v.offset);
    let* () =
      iter 0 count @@ fun i ->
      let* d = read_int32 ch in
      a.(i) <- d;
      Lwt.return ()
    in
    Lwt.return a

  let read_info ch =
    let* s = R.read_string ch 4 in
    assert (s = "II\042\000");
    let* offset = read_int32 ch in
    if debug then Format.eprintf "%ld@." offset;
    let* ifd = read_ifd ch offset in
    let width = ifd_uint16 256 ifd in
    let height = ifd_uint16 257 ifd in
    if debug then Format.eprintf "size: %dx%d@." width height;
    let tile_width = ifd_uint16 322 ifd in
    let tile_height = ifd_uint16 323 ifd in
    if debug then Format.eprintf "tile size: %dx%d@." tile_width tile_height;
    let* tile_offsets = ifd_int32s 324 ifd ch in
    let* tile_byte_counts = ifd_int32s 325 ifd ch in
    assert (ifd_uint16 258 ifd = 32);
    (* 32 bits per sample *)
    assert (ifd_uint16 259 ifd = 8);
    (* Deflate *)
    assert (ifd_uint16 277 ifd = 1);
    (* 1 sample per pixel *)
    assert (ifd_uint16 317 ifd = 3);
    (* Prediction: floating points *)
    Lwt.return
      { width; height; tile_width; tile_height; tile_offsets; tile_byte_counts }

  let decode_delta b w h =
    for i = 0 to h - 1 do
      let i = i * w in
      for j = 1 to w - 1 do
        Bytes.set b (i + j)
          (Char.chr
             ((Char.code (Bytes.get b (i + j))
              + Char.code (Bytes.get b (i + j - 1)))
             land 0xff))
      done
    done

  let decode_fp b w h =
    let t = Unix.gettimeofday () in
    decode_delta b (w * 4) h;
    Format.eprintf "DECODING DELTA %f@." (Unix.gettimeofday () -. t);
    let t = Unix.gettimeofday () in
    let b' = Bytes.copy b in
    for i = 0 to h - 1 do
      let i = i * w * 4 in
      for j = 0 to w - 1 do
        for k = 0 to 3 do
          Bytes.set b'
            (i + (4 * j) + k)
            (Bytes.get b (i + ((4 - k - 1) * w) + j))
        done
      done
    done;
    Format.eprintf "DECODING FP %f@." (Unix.gettimeofday () -. t);
    b'

  let read_tile ch
      { tile_width; tile_height; tile_offsets; tile_byte_counts; _ } i =
    Format.eprintf "READING TILE: %ld %ld@." tile_offsets.(i)
      tile_byte_counts.(i);
    let t = Unix.gettimeofday () in
    R.seek ch (Int32.to_int tile_offsets.(i));
    let* s = R.read_chunk ch (Int32.to_int tile_byte_counts.(i)) in
    let b = Bytes.create (tile_width * tile_height * 4) in
    let* () = R.inflate s b in
    Format.eprintf "READING TILE %f@." (Unix.gettimeofday () -. t);
    let b = decode_fp b tile_width tile_height in
    let tile =
      Bigarray.(Array2.create Float32 C_layout) tile_height tile_width
    in
    let t = Unix.gettimeofday () in
    for i = 0 to tile_height - 1 do
      for j = 0 to tile_width - 1 do
        tile.{i, j} <-
          Int32.float_of_bits
            (Bytes.get_int32_le b (4 * ((i * tile_width) + j)))
      done
    done;
    Format.eprintf "DECODING TILE %f@." (Unix.gettimeofday () -. t);
    Lwt.return tile
end
