let ( let* ) = Lwt.bind

type t = in_channel

let select ~lat ~lon f =
  let ch =
    open_in
      (Printf.sprintf "data/Copernicus_DSM_COG_10_N%02d_00_E%03d_00_DEM.tif" lat
         lon)
  in
  let* res = f ch in
  close_in ch;
  Lwt.return res

let seek = seek_in
let read_string ch n = Lwt.return (really_input_string ch n)

type chunk = string

let read_chunk = read_string

let inflate s b =
  let st = Zlib.inflate_init true in
  let ok, i, j =
    Zlib.inflate_string st s 0 (String.length s) b 0 (Bytes.length b)
      Zlib.Z_FINISH
  in
  assert ok;
  assert (i = String.length s);
  assert (j = Bytes.length b);
  Zlib.inflate_end st;
  Lwt.return ()
