let ( // ) x y =
  let q = x / y in
  let r = x mod y in
  if r >= 0 then q else q - 1

let ( let* ) = Lwt.bind

let rec iter min max f =
  if min > max then Lwt.return ()
  else
    let* () = f min in
    iter (min + 1) max f

let rec iter_rev min max f =
  if min > max then Lwt.return ()
  else
    let* () = f max in
    iter_rev min (max - 1) f

module Make (R : Tiff.READER) = struct
  module Tiff = Tiff.Make (R)

  let f ~size ~lat ~lon =
    let tile_count = (3600 + 1024) / 1024 in
    let min_lat = truncate (lat *. 3600.) - (size / 2) in
    let min_lon = truncate (lon *. 3600.) - (size / 2) in
    let max_lat = min_lat + size - 1 in
    let max_lon = min_lon + size - 1 in
    let heights = Bigarray.(Array2.create Float32 C_layout) size size in

    Format.eprintf "RANGE: %d %d %d %d@." (min_lat // 3600) (max_lat // 3600)
      (min_lon // 3600) (max_lon // 3600);

    let* () =
      iter_rev (min_lat // 3600) (max_lat // 3600) @@ fun lat ->
      Format.eprintf "LAT: %d@." lat;
      iter (min_lon // 3600) (max_lon // 3600) @@ fun lon ->
      R.select ~lat ~lon @@ fun ch ->
      let* info = Tiff.read_info ch in
      let min_tile_x = max 0 ((min_lon - (3600 * lon)) // 1024) in
      let max_tile_x =
        min (tile_count - 1) ((max_lon - (3600 * lon)) // 1024)
      in
      let min_tile_y = max 0 (((3600 * (lat + 1)) - 1 - max_lat) // 1024) in
      let max_tile_y =
        min (tile_count - 1) (((3600 * (lat + 1)) - 1 - min_lat) // 1024)
      in
      iter min_tile_y max_tile_y @@ fun tile_y ->
      iter min_tile_x max_tile_x @@ fun tile_x ->
      let delta_x = (3600 * lon) + (1024 * tile_x) in
      let delta_y = (3600 * (lat + 1)) - (1024 * (tile_y + 1)) in
      let min_x = min_lon - delta_x in
      let max_x = max_lon - delta_x in
      let min_y = min_lat - delta_y in
      let max_y = max_lat - delta_y in
      Format.printf "    %d %d %d %d %d %d@." tile_x tile_y min_x max_x min_y
        max_y;
      assert (min_x < 1024 && max_x >= 0);
      assert (min_y < 1024 && max_y >= 0);
      let* tile = Tiff.read_tile ch info (tile_x + (tile_y * tile_count)) in
      Format.eprintf "TILE %d %d - %d %d@." lat tile_y
        (max 0 min_y - min_y)
        (min 1023 max_y - min_y);
      let t = Unix.gettimeofday () in
      for y = max 0 min_y to min 1023 max_y do
        let x = max 0 min_x in
        let w = min 1023 max_x - max 0 min_x + 1 in
        Bigarray.Array1.blit
          (Bigarray.Array1.sub Bigarray.Array2.(slice_left tile (1023 - y)) x w)
          (Bigarray.Array1.sub
             Bigarray.Array2.(slice_left heights (size - 1 - y + min_y))
             (x - min_x) w)
        (*
        for x = max 0 min_x to min 1023 max_x do
          heights.{size - 1 - y + min_y, x - min_x} <- tile.{1023 - y, x}
        done
        *)
      done;
      Format.eprintf "COPYING TILE %f@." (Unix.gettimeofday () -. t);
      Lwt.return ()
    in
    Lwt.return heights
end
