let ( let* ) = Lwt.bind

type t = in_channel

let select ~lat ~lon f =
  let lat_c = if lat >= 0 then 'N' else 'S' in
  let lon_c = if lon >= 0 then 'E' else 'W' in
  let ch =
    open_in
      (Printf.sprintf "data/Copernicus_DSM_COG_10_%c%02d_00_%c%03d_00_DEM.tif"
         lat_c (abs lat) lon_c (abs lon))
  in
  let* res = f ch in
  close_in ch;
  Lwt.return res

let prefetch ~lat:_ ~lon:_ = Lwt.return_unit
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
