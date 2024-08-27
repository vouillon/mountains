let () =
  let ch = open_in "Copernicus_DSM_COG_10_N44_00_E006_00_DEM.tif" in
  let {
    Relief.width;
    height;
    tile_width;
    tile_height;
    tile_offsets;
    tile_byte_counts;
    _;
  } =
    Relief.read_info ch
  in
  Graphics.open_graph " 1024x1024";
  for i = 0 to Array.length tile_offsets - 1 do
    let tile =
      Relief.read_tile ch tile_width tile_height tile_offsets tile_byte_counts i
    in
    let tile_per_line = (width + tile_width - 1) / tile_width in
    let tx = i mod tile_per_line in
    let ty = i / tile_per_line in
    let tile_coord =
      {
        Points.lon = 6. +. (float tx *. float tile_width /. float width);
        lat = 45. -. (float (ty + 1) *. float tile_height /. float height);
      }
    in
    let tile_coord' =
      {
        Points.lon = tile_coord.lon +. (float tile_width /. float width);
        Points.lat = tile_coord.lat +. (float tile_height /. float height);
      }
    in
    Format.eprintf "%d %d@." tx ty;
    Format.eprintf "%g %g - %g %g@." tile_coord.lat tile_coord.lon
      tile_coord'.lat tile_coord'.lon;
    let points =
      Points.find tile_coord tile_coord'
      |> List.map (fun (nm, { Points.lat; lon }) ->
             let x = truncate ((lon -. tile_coord.lon) *. float width) in
             let y = truncate ((tile_coord'.lat -. lat) *. float height) in
             Format.eprintf "%s - %g %g - %d %d - %g@." nm lat lon x y
               tile.{y, x};
             (nm, (x, y)))
    in
    (*
    List.iter
      (fun (nm, (x, y)) -> Format.eprintf "%s %d %d %g@." nm x y tile.{y, x})
      points;
*)
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
    List.iter
      (fun (_, (x, y)) ->
        Format.eprintf "%d %d@." x y;
        Graphics.set_color Graphics.red;
        Graphics.plot x (tile_width - y - 1);
        Graphics.draw_rect (x - 2) (tile_width - y - 1 - 2) 4 4)
      points;
    ignore (Graphics.read_key ())
  done
