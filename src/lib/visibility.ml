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

let test
    (height :
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t) ~src_x
    ~src_y ~dst_x ~dst_y =
  let src_h = height.{src_y, src_x} +. 2. in
  let dst_h = height.{dst_y, dst_x} in
  let dx = dst_x - src_x in
  let dy = dst_y - src_y in
  let d = sqrt ((float dx ** 2.) +. (float dy ** 2.)) in
  let check x y =
    let dx' = x - src_x in
    let dy' = y - src_y in
    let d' = sqrt ((float dx' ** 2.) +. (float dy' ** 2.)) in
    let h = height.{y, x} in
    let h' =
      ((dst_h -. src_h) *. float ((dx * dx') + (dy * dy')) /. d /. d) +. src_h
    in
    let res = d' > 0.9 *. d || h < h' in
    (*    Format.eprintf "%g - %g %g - %b@." (d' /. d) h h' res;*)
    res
  in
  follow_line check src_x src_y dst_x dst_y
