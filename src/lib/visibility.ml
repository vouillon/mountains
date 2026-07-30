(* Drop of the Earth's surface below the observer's tangent plane, [d2]
   square metres from the observer, with standard atmospheric refraction
   folded in: sight lines bend back towards the ground by ~13%, equivalent to
   an effective radius R / (1 - 0.13) ~ 7320 km. In the observer-anchored
   frame the renderer works in (terrain lowered by this drop), light rays are
   straight lines. Must match the constant in radial_common.vert. *)
let curvature_drop d2 = 6.8306e-8 *. d2

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
(* Terrain within this many pixels (~300 m) of the target cannot occlude it,
   so that a summit sticks out of its own massif. The exemption is capped
   proportionally for nearby targets, where a fixed 300 m would swallow most
   of the path; without the absolute cap, the old purely proportional zone
   (6% of the distance) exempted the last 4 km in front of a 70 km peak. *)
let summit_exemption = 10.

let test (get_height : int -> int -> float) ?src_h ~src_x ~src_y ~dst_x ~dst_y
    () =
  (* [src_h] is used as given: [test_precise] delegates mid-ray with the exact
     ray height at the hand-off point. Observer eye height is the entry
     points' concern, not this function's. *)
  let src_h = Option.value ~default:(get_height src_y src_x +. 2.) src_h in
  let dst_h = get_height dst_y dst_x in
  let dx = dst_x - src_x in
  let dy = dst_y - src_y in
  let d = sqrt (float ((dx * dx) + (dy * dy))) in
  let check x y =
    let dx' = x - src_x in
    let dy' = y - src_y in
    let d' = sqrt (float ((dx' * dx') + (dy' * dy'))) in
    let h = get_height y x in
    let h' =
      ((dst_h -. src_h) *. float ((dx * dx') + (dy * dy')) /. d /. d) +. src_h
    in
    (* The +1 m tolerance is deliberately looser than [test_precise]'s +0.1 m:
       this phase samples the terrain at whole ~30 m pixels against an
       interpolated ray, so it needs more slack for sampling error. *)
    h < h' +. 1. || d -. d' < Float.min summit_exemption (0.06 *. d)
  in
  follow_line check src_x src_y dst_x dst_y

(** Bilinear interpolation of height at fractional coordinates.
    [get_height row col] returns height at integer grid coordinates (y+1 is
    "up", increasing latitude). Reads the (x0+1, y0+1) neighbours, so the caller
    must guarantee they are inside the height grid. *)
let bilinear_height (get_height : int -> int -> float) ~x ~y =
  let x0 = int_of_float (floor x) in
  let y0 = int_of_float (floor y) in
  let fx = x -. float x0 in
  let fy = y -. float y0 in
  (* h00 at (x0, y0), h10 at (x0+1, y0), h01 at (x0, y0+1), h11 at
     (x0+1, y0+1) *)
  let h00 = get_height y0 x0 in
  let h10 = get_height y0 (x0 + 1) in
  let h01 = get_height (y0 + 1) x0 in
  let h11 = get_height (y0 + 1) (x0 + 1) in
  let h0 = h00 +. (fx *. (h10 -. h00)) in
  let h1 = h01 +. (fx *. (h11 -. h01)) in
  h0 +. (fy *. (h1 -. h0))

let debug = false

(** Precise visibility test. Tests visibility from (src_x + off_x, src_y +
    off_y) to (dst_x, dst_y).

    Walks the ray in 0.02-pixel steps (~0.6 m) with bilinear interpolation for
    the first 6 pixels (~185 m), where the ray hugs the terrain and whole-pixel
    sampling aliases worst, then switches to the faster Bresenham-based
    algorithm for the rest. The bilinear phase therefore stays within ~6 pixels
    of the source, whose 1-pixel neighbourhood must lie inside the height grid
    (the viewers place the observer at the tile centre).

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
let test_precise (get_height : int -> int -> float) ?src_h ?curvature ~off_x
    ~off_y ~src_x ~src_y ~dst_x ~dst_y () =
  let src_x_f = float src_x +. off_x in
  let src_y_f = float src_y +. off_y in
  (* With [curvature] = (metres per pixel in x, in y), work in the
     observer-anchored frame: every height is lowered by the curvature drop at
     its distance from the source, and the straight sight line of the flat
     algorithm below is then exactly the refracted ray over the sphere. The
     wrapped [get_height] also feeds the Bresenham fallback, whose sub-segment
     of the ray stays straight in this frame. *)
  let get_height =
    match curvature with
    | None -> get_height
    | Some (mx, my) ->
        fun row col ->
          let dxm = (float col -. src_x_f) *. mx in
          let dym = (float row -. src_y_f) *. my in
          get_height row col -. curvature_drop ((dxm *. dxm) +. (dym *. dym))
  in
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
    (* Bilinear phase extent (pixels) and step (fraction of a pixel) *)
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
