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

type refinement = {
  sample : float -> float -> float option;
      (** height at a fractional base-grid position, [None] outside this grid *)
  step : float;
      (** finest useful walk step over this grid, in base-grid pixels: half its
          sample spacing *)
}

(* How steeply terrain is assumed to rise, as metres per metre, between one
   sample of the walked grid and the next. The march below turns the ray's
   clearance above the terrain into a distance it can safely skip: with
   clearance [c], nothing can reach the ray within [c / max_slope] metres. 4
   (76 degrees) is far above the slope any of our grids actually resolves at
   its own spacing, and it buys long steps through open air. *)
let max_slope = 4.

(* Bounds on the adaptive step, in base-grid pixels (~30 m). The upper bound is
   1 pixel because that is what the whole-pixel walk this replaces sampled: a
   longer step could skip a ridge narrower than a pixel whose flanks are locally
   steeper than [max_slope], so the march is never coarser than its predecessor.
   The lower bound is the fixed step below, past which bilinear samples of the
   same cell stop telling us anything new. *)
let max_step = 1.0

(* Floor on the step when no refinement covers the point: half the base grid's
   spacing. Sampling a bilinear surface finer than half a cell adds nothing, and
   each refinement supplies its own floor the same way -- without which a ray
   grazing terrain inside the 9.5 m ring was walked in 0.6 m steps, 16x finer
   than the data, which cost more than the whole-pixel walk it replaced. *)
let base_step = 0.5

(* The observer's immediate surroundings are walked at a fixed step regardless of
   clearance. [max_slope] is a fair bound on what a grid resolves over its own
   spacing, but not on what sits a few metres from the eye: a boulder or the lip
   of the summit one is standing on rises far faster than 4:1 over a couple of
   metres, and letting the step grow with clearance there skips it. Measured: on
   the Mont Blanc vista, dropping this phase let 125 further POIs through that
   the fixed walk correctly hid. *)
let near_distance = 6.0

(** Precise visibility test. Tests visibility from (src_x + off_x, src_y +
    off_y) to (dst_x, dst_y).

    Marches the ray with bilinear interpolation, taking each step from the
    clearance between the ray and the terrain below it: tight where the ray
    grazes the ground, long where it flies high. That replaces a fixed
    0.02-pixel walk over the first 6 pixels followed by whole-pixel sampling,
    which chose its accuracy by distance from the observer rather than by
    proximity to the terrain -- so a ray skimming a ridge 500 m out was tested
    every ~30 m and could step straight over it. [fine] is now consulted along
    the whole ray rather than only near the source, which is what lets the
    near-field grids contribute to occlusion at the distances they cover.

    The source's 1-pixel neighbourhood must lie inside the height grid (the
    viewers place the observer at the tile centre).

    @param get_height function to get height at (row, col)
    @param src_h
      optional override for source elevation (defaults to terrain + 2m)
    @param curvature
      metres per pixel (x, y): when given, heights are evaluated in the
      observer-anchored frame lowered by the Earth-curvature drop, in which
      sight lines are straight (see {!curvature_drop})
    @param fine
      [fine x y] is the terrain height at the fractional grid position (x, y) as
      read from a finer grid covering part of the ray, or [None] outside it.
      Heights are raw: the curvature drop is applied here.
    @param off_x fractional X offset from src_x (in pixels, ~30m each)
    @param off_y fractional Y offset from src_y (in pixels, ~30m each)
    @param src_x source column (integer)
    @param src_y source row (integer)
    @param dst_x destination column
    @param dst_y destination row
    @return true if destination is visible from source *)
let test_precise (get_height : int -> int -> float) ?src_h ?curvature
    ?(fine = []) ~off_x ~off_y ~src_x ~src_y ~dst_x ~dst_y () =
  let src_x_f = float src_x +. off_x in
  let src_y_f = float src_y +. off_y in
  (* With [curvature] = (metres per pixel in x, in y), work in the
     observer-anchored frame: every height is lowered by the curvature drop at
     its distance from the source, and the straight sight line of the flat
     algorithm below is then exactly the refracted ray over the sphere. *)
  let drop =
    match curvature with
    | None -> fun _ _ -> 0.
    | Some (mx, my) ->
        fun x y ->
          let dxm = (x -. src_x_f) *. mx in
          let dym = (y -. src_y_f) *. my in
          curvature_drop ((dxm *. dxm) +. (dym *. dym))
  in
  let get_height =
    match curvature with
    | None -> get_height
    | Some _ ->
        fun row col -> get_height row col -. drop (float col) (float row)
  in
  (* The near phase's step, from the finest grid actually available rather than a
     constant: sampling a bilinear surface finer than half a cell tells us
     nothing new, which is the same argument [base_step] rests on. With the 2.38 m
     ring that is 0.0386 base pixels against the 0.02 this replaces, so the phase
     costs about half as many steps; with no refinement at all it is [base_step]
     and the phase collapses into the walk that follows. *)
  let near_step =
    List.fold_left (fun a r -> Float.min a r.step) base_step fine
  in
  (* Height, and the finest step the grid that supplied it justifies. *)
  let terrain_at x y =
    let rec first = function
      | [] -> (bilinear_height get_height ~x ~y, base_step)
      | r :: rest -> (
          match r.sample x y with
          | Some h -> (h -. drop x y, r.step)
          | None -> first rest)
    in
    first fine
  in
  let dst_x_f = float dst_x in
  let dst_y_f = float dst_y in
  let dx = dst_x_f -. src_x_f in
  let dy = dst_y_f -. src_y_f in
  let d = sqrt ((dx *. dx) +. (dy *. dy)) in
  if d < 0.1 then true (* Source and destination are the same point *)
  else
    let src_h =
      Option.value ~default:(fst (terrain_at src_x_f src_y_f) +. 2.) src_h
    in
    let dst_h = get_height dst_y dst_x in
    (* Metres per pixel, to convert a clearance in metres into a step in pixels.
       Without [curvature] there is no scale to hand, so fall back to the base
       grid's ~30 m and let [max_step] bound the result. *)
    let m_per_px =
      match curvature with Some (mx, my) -> 0.5 *. (mx +. my) | None -> 30.
    in
    let ux = dx /. d and uy = dy /. d in
    let rise = (dst_h -. src_h) /. d in
    (* Terrain this close to the target does not occlude it, so that a summit
       sticks out of its own massif. Capped proportionally for nearby targets,
       where a fixed ~300 m would swallow most of the path. *)
    let stop = d -. Float.min summit_exemption (0.06 *. d) in
    (* Beyond the reach of any finer grid there is nothing left to resolve that
       the whole-pixel walk would miss, so hand over to it: that keeps the long
       tail of a 70 km ray as cheap as it was. *)
    let handover t x y =
      let sx = int_of_float (floor x) and sy = int_of_float (floor y) in
      if sx = dst_x && sy = dst_y then true
      else
        test get_height
          ~src_h:(src_h +. (rise *. t))
          ~src_x:sx ~src_y:sy ~dst_x ~dst_y ()
    in
    let rec march t =
      if t >= stop then true
      else
        let x = src_x_f +. (ux *. t) and y = src_y_f +. (uy *. t) in
        let terrain_h, floor_step = terrain_at x y in
        (* Past the reach of every refinement there is nothing left to resolve
           that the whole-pixel walk would miss, so hand over to it: that keeps
           the long tail of a 70 km ray as cheap as it was. *)
        if floor_step = base_step && t > near_distance then handover t x y
        else
          let h_line = src_h +. (rise *. t) in
          if debug then Format.eprintf "%f %f@." terrain_h h_line;
          if terrain_h +. 0.1 > h_line then false
          else
            let step =
              if t < near_distance then near_step
              else
                let safe = (h_line -. terrain_h) /. max_slope /. m_per_px in
                Float.max floor_step (Float.min max_step safe)
            in
            march (t +. step)
    in
    march 0.
