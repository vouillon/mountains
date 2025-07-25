module Tiff' = Tiff.Make (Reader)
module Loader = Loader.Make (Reader)

let ( let* ) = Lwt.bind

let rec iter min max f =
  if min > max then Lwt.return ()
  else
    let* () = f min in
    iter (min + 1) max f

let () =
  Lwt_main.run
  @@
  if false then (
    Reader.select ~lat:44 ~lon:6 @@ fun ch ->
    let* ({ Tiff.width; height; tile_width; tile_height; tile_offsets; _ } as
          info) =
      Tiff'.read_info ch
    in
    Graphics.open_graph " 1024x1024";
    iter 0 (Array.length tile_offsets - 1) @@ fun i ->
    let* tile = Tiff'.read_tile ch info i in
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
      |> List.map (fun ({ Points.coord = { lat; lon }; _ } as pt) ->
             let x = truncate ((lon -. tile_coord.lon) *. float width) in
             let y = truncate ((tile_coord'.lat -. lat) *. float height) in
             Format.eprintf "%s - %g %g - %d %d - %g@." pt.Points.name lat lon x
               y
               tile.{y, x};
             (pt, (x, y)))
    in
    for y = 1 to tile_height - 2 do
      for x = 1 to tile_width - 2 do
        let nx = tile.{y, x - 1} -. tile.{y, x + 1} in
        let ny = tile.{y - 1, x} -. tile.{y + 1, x} in
        let nz = 2. *. 30. in
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
        Graphics.set_color Graphics.red;
        Graphics.plot x (tile_width - y - 1);
        Graphics.draw_rect (x - 2) (tile_width - y - 1 - 2) 4 4)
      points;
    ignore (Graphics.read_key ());
    Lwt.return ())
  else
    let lat = 44.209067 in
    let lon = 6.9423065 in
    let* tile = Loader.f ~lat ~lon in
    Graphics.open_graph " 1024x1024";
    let tile_height = 2050 in
    let tile_width = 2050 in
    for y = 1 to tile_height - 2 do
      for x = 1 to tile_width - 2 do
        let nx = tile.{y, x - 1} -. tile.{y, x + 1} in
        let ny = tile.{y - 1, x} -. tile.{y + 1, x} in
        let nz = 2. *. 30. in
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
        Graphics.plot (x / 2) ((tile_width - y - 1) / 2)
      done
    done;
    let tile_coord =
      { Points.lon = lon -. (1024. /. 3600.); lat = lat -. (1024. /. 3600.) }
    in
    let tile_coord' =
      { Points.lon = lon +. (1024. /. 3600.); lat = lat +. (1024. /. 3600.) }
    in
    let points =
      let width = 3600 in
      let height = 3600 in
      Points.find tile_coord tile_coord'
      |> List.map (fun ({ Points.coord = { lat; lon }; _ } as pt) ->
             let x = truncate ((lon -. tile_coord.lon) *. float width) in
             let y = truncate ((tile_coord'.lat -. lat) *. float height) in
             Format.eprintf "%s - %g %g - %d %d - %g@." pt.Points.name lat lon x
               y
               tile.{y, x};
             (pt, (x, y)))
    in
    List.iter
      (fun (_, (x, y)) ->
        Graphics.set_color Graphics.red;
        (*        Graphics.plot x (tile_width - y - 1);*)
        Graphics.draw_rect ((x - 2) / 2) ((tile_width - y - 1 - 2) / 2) 4 4)
      points;
    ignore (Graphics.read_key ());
    Lwt.return ()
