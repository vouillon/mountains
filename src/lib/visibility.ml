let follow_line plot x0 y0 x1 y1 =
  let dx = abs (x1 - x0) in
  let sx = if x0 < x1 then 1 else -1 in
  let dy = -abs (y1 - y0) in
  let sy = if y0 < y1 then 1 else -1 in
  let rec follow error x y =
    let ok = plot x y in
    ok
    &&
    if x <> x1 || y <> y1 then
      let e2 = 2 * error in
      let x, error = if e2 >= dy then (x + sx, error + dy) else (x, error) in
      let y, error = if e2 <= dx then (y + sy, error + dx) else (y, error) in
      follow error x y
    else true
  in
  follow (dx + dy) x0 y0

(* get_height: row -> col -> float *)
let test (get_height : int -> int -> float) ?src_h ~src_x ~src_y ~dst_x ~dst_y
    () =
  let src_h =
    max
      (get_height src_y src_x +. 2.)
      (Option.value ~default:(get_height src_y src_x +. 2.) src_h)
  in
  let dst_h = get_height dst_y dst_x in
  let dx = dst_x - src_x in
  let dy = dst_y - src_y in
  let d = sqrt ((float dx ** 2.) +. (float dy ** 2.)) in
  let check x y =
    let dx' = x - src_x in
    let dy' = y - src_y in
    let d' = sqrt ((float dx' ** 2.) +. (float dy' ** 2.)) in
    let h = get_height y x in
    let h' =
      ((dst_h -. src_h) *. float ((dx * dx') + (dy * dy')) /. d /. d) +. src_h
    in
    (*
    let res = d' > 0.94 *. d || (h -. h') /. d' < 0.2 in
*)
    (*
    let res = d' > 0.94 *. d || h -. h' < min 0. (d' *. 0.1) in
    (*    Format.eprintf "%g - %g %g - %b@." (d' /. d) h h' res;*)
    res
*)
    ignore d';
    h < h' +. 1. || d' > 0.94 *. d
  in
  follow_line check src_x src_y dst_x dst_y

(** Bilinear interpolation of height at fractional coordinates.
    [get_height row col] returns height at integer grid coordinates. Note: Y
    axis follows OpenGL convention where y+1 is "up" (increasing lat). *)
let bilinear_height (get_height : int -> int -> float) ~x ~y =
  let x0 = int_of_float (floor x) in
  let y0 = int_of_float (floor y) in
  (*  Format.eprintf "%d %d@." x0 y0;*)
  let fx = x -. float x0 in
  let fy = y -. float y0 in
  (* h00 is at (x0, y0), h10 at (x0+1, y0), h01 at (x0, y0-1), h11 at (x0+1, y0-1) *)
  let h00 = get_height y0 x0 in
  let h10 = get_height y0 (x0 + 1) in
  let h01 = get_height (y0 + 1) x0 in
  let h11 = get_height (y0 + 1) (x0 + 1) in
  let h0 = h00 +. (fx *. (h10 -. h00)) in
  let h1 = h01 +. (fx *. (h11 -. h01)) in
  h0 +. (fy *. (h1 -. h0))

let debug = false

(** Precise visibility test for short distances using bilinear interpolation.
    Tests visibility from (src_x + off_x, src_y + off_y) to (dst_x, dst_y).

    Uses small fixed steps (~30m) with bilinear interpolation for the first
    100m, then switches to the faster Bresenham-based algorithm for the rest.

    @param get_height function to get height at (row, col)
    @param src_h
      optional override for source elevation (defaults to terrain + 2m)
    @param off_x fractional X offset from src_x (in pixels, ~30m each)
    @param off_y fractional Y offset from src_y (in pixels, ~30m each)
    @param src_x source column (integer)
    @param src_y source row (integer)
    @param dst_x destination column
    @param dst_y destination row
    @return true if destination is visible from source *)
let test_precise (get_height : int -> int -> float) ?src_h ~off_x ~off_y ~src_x
    ~src_y ~dst_x ~dst_y () =
  let src_x_f = float src_x +. off_x in
  let src_y_f = float src_y +. off_y in
  let dst_x_f = float dst_x in
  let dst_y_f = float dst_y in
  let dx = dst_x_f -. src_x_f in
  let dy = dst_y_f -. src_y_f in
  let d = sqrt ((dx *. dx) +. (dy *. dy)) in
  if d < 0.1 then true (* Source and destination are the same point *)
  else
    let src_terrain = bilinear_height get_height ~x:src_x_f ~y:src_y_f in
    let src_h = Option.value ~default:(src_terrain +. 2.) src_h in
    if debug then Format.eprintf "%f %f@." src_terrain src_h;
    let dst_h = get_height dst_y dst_x in
    (* Use bilinear for first ~150m (3-4 pixels at 30m/pixel), then Bresenham *)
    let precise_distance = 6.0 in
    let step_len = 0.02 in
    let num_precise_steps =
      int_of_float (ceil (min d precise_distance /. step_len))
    in
    let dx_step = dx /. d *. step_len in
    let dy_step = dy /. d *. step_len in
    let dh_per_unit = (dst_h -. src_h) /. d *. step_len in
    (* Precise bilinear check for short range *)
    let rec check_precise i x y h_line =
      if i > num_precise_steps then
        (* Switch to Bresenham for the rest *)
        let switch_x = int_of_float (floor x) in
        let switch_y = int_of_float (floor y) in
        if switch_x = dst_x && switch_y = dst_y then true
        else
          test get_height ~src_h:h_line ~src_x:switch_x ~src_y:switch_y ~dst_x
            ~dst_y ()
      else
        let terrain_h = bilinear_height get_height ~x ~y in
        if debug then Format.eprintf "%f %f@." terrain_h h_line;
        if terrain_h +. 0.1 > h_line then false
        else
          check_precise (i + 1) (x +. dx_step) (y +. dy_step)
            (h_line +. dh_per_unit)
    in
    check_precise 1 (src_x_f +. dx_step) (src_y_f +. dy_step)
      (src_h +. dh_per_unit)
