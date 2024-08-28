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

let test height x0 y0 x1 y1 =
  let h0 = height.{y0, x0} +. 2. in
  let h1 = height.{y1, x1} in
  let dx = x1 - x0 in
  let dy = y1 - y0 in
  let d = sqrt ((float dx ** 2.) +. (float dy ** 2.)) in
  let check x y =
    let dx' = x - x0 in
    let dy' = y - y0 in
    let d' = sqrt ((float dx' ** 2.) +. (float dy' ** 2.)) in
    let h = height.{y, x} in
    let h' = ((h1 -. h0) *. float ((dx * dx') + (dy * dy')) /. d /. d) +. h0 in
    let res = d' > 0.9 *. d || h < h' in
    (*    Format.eprintf "%g - %g %g - %b@." (d' /. d) h h' res;*)
    res
  in
  follow_line check x0 y0 x1 y1
