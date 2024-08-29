let target_size = 2048
let size = target_size + 2

let ( // ) x y =
  let q = x / y in
  let r = x mod y in
  if r >= 0 then q else q - 1

let f ~lat ~lon =
  let tile_count = (3600 + 1024) / 1024 in
  let min_lat = truncate (lat *. 3600.) - (size / 2) in
  let min_lon = truncate (lon *. 3600.) - (size / 2) in
  let max_lat = min_lat + size - 1 in
  let max_lon = min_lon + size - 1 in
  let heights = Bigarray.(Array2.create Float32 C_layout) size size in

  Format.eprintf "RANGE: %d %d %d %d@." (min_lat // 3600) (max_lat // 3600)
    (min_lon // 3600) (max_lon // 3600);

  for lat = max_lat // 3600 downto min_lat // 3600 do
    Format.eprintf "LAT: %d@." lat;
    for lon = min_lon // 3600 to max_lon // 3600 do
      let ch =
        open_in
          (Printf.sprintf "data/Copernicus_DSM_COG_10_N%02d_00_E%03d_00_DEM.tif"
             lat lon)
      in
      let info = Tiff.read_info ch in
      let min_tile_x = max 0 ((min_lon - (3600 * lon)) // 1024) in
      let max_tile_x =
        min (tile_count - 1) ((max_lon - (3600 * lon)) // 1024)
      in
      let min_tile_y = max 0 (((3600 * (lat + 1)) - 1 - max_lat) // 1024) in
      let max_tile_y =
        min (tile_count - 1) (((3600 * (lat + 1)) - 1 - min_lat) // 1024)
      in
      for tile_y = min_tile_y to max_tile_y do
        for tile_x = min_tile_x to max_tile_x do
          let delta_x = (3600 * lon) + (1024 * tile_x) in
          let delta_y = (3600 * (lat + 1)) - (1024 * (tile_y + 1)) in
          let min_x = min_lon - delta_x in
          let max_x = max_lon - delta_x in
          let min_y = min_lat - delta_y in
          let max_y = max_lat - delta_y in
          Format.printf "    %d %d %d %d %d %d@." tile_x tile_y min_x max_x
            min_y max_y;
          assert (min_x < 1024 && max_x >= 0);
          assert (min_y < 1024 && max_y >= 0);
          let tile = Tiff.read_tile ch info (tile_x + (tile_y * tile_count)) in
          Format.eprintf "TILE %d %d - %d %d@." lat tile_y
            (max 0 min_y - min_y)
            (min 1023 max_y - min_y);
          for y = max 0 min_y to min 1023 max_y do
            for x = max 0 min_x to min 1023 max_x do
              heights.{2049 - y + min_y, x - min_x} <- tile.{1023 - y, x}
            done
          done
        done
      done;
      close_in ch
    done
  done;
  heights
