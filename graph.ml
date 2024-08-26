let () =
  let ch = open_in "Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif" in
  let { Relief.tile_width; tile_height; tile_offsets; tile_byte_counts; _ } =
    Relief.read_info ch
  in
  Graphics.open_graph " 1024x1024";
  for i = 0 to Array.length tile_offsets - 1 do
    let tile =
      Relief.read_tile ch tile_width tile_height tile_offsets tile_byte_counts i
    in
    for y = 1 to tile_height - 2 do
      for x = 1 to tile_width - 2 do
        (*
        let l = truncate (256. -. (tile.{y, x} /. 4000. *. 256.)) in
*)
        let nx = tile.{y, x - 1} -. tile.{y, x + 1} in
        let ny = tile.{y - 1, x} -. tile.{y + 1, x} in
        let nz = 2. *. 30. in
        (*
(-dhx, -dhy, d)

compute normal
scalar product with light direction
negative ==> black
*)
        let u =
          max 0.
            (-.(nx +. ny -. nz)
            /. sqrt 3.
            /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)))
          (*            (nz /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)))*)
        in
        let l = truncate (u *. 255.9) in
        let c = Graphics.rgb l l l in
        Graphics.set_color c;
        Graphics.plot x (tile_width - y - 1)
      done
    done;
    ignore (Graphics.read_key ())
  done
