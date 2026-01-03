(* proj_3035.ml - Lambert Azimuthal Equal Area (EPSG:3035) to WGS84 *)

let pi = 3.14159265358979323846
let deg_to_rad = pi /. 180.0
let rad_to_deg = 180.0 /. pi

(* GRS80 Ellipsoid constants *)
let a = 6378137.0
let f = 1.0 /. 298.257222101
let e2 = f *. (2.0 -. f) (* First eccentricity squared *)
let e = sqrt e2

(* EPSG:3035 Projection Parameters *)
let lat0 = 52.0 *. deg_to_rad (* Center Latitude *)
let lon0 = 10.0 *. deg_to_rad (* Center Longitude *)
let fe = 4_321_000.0 (* False Easting *)
let fn = 3_210_000.0 (* False Northing *)

(* Authalic latitude calculations *)
let q_from_lat sin_phi =
  (1.0 -. e2)
  *. ((sin_phi /. (1.0 -. (e2 *. sin_phi *. sin_phi)))
     -. 1.0 /. (2.0 *. e)
        *. log ((1.0 -. (e *. sin_phi)) /. (1.0 +. (e *. sin_phi))))

let q_p = q_from_lat 1.0 (* q at pole *)

(* Pre-calculated constants for projection *)
let sin_lat0 = sin lat0
let cos_lat0 = cos lat0
let q0 = q_from_lat sin_lat0
let r_q = a *. sqrt (q_p /. 2.0) (* Authalic radius *)

(* beta0 = asin(q0/qP) - authalic latitude of center *)
let sin_beta0 = q0 /. q_p
let cos_beta0 = sqrt (1.0 -. (sin_beta0 *. sin_beta0))

(* D = a * cos(lat0) / sqrt(1 - e2*sin^2(lat0)) / (Rq * cos(beta0)) *)
let d =
  a *. cos_lat0
  /. sqrt (1.0 -. (e2 *. sin_lat0 *. sin_lat0))
  /. (r_q *. cos_beta0)

(* 
   Using the breakdown from PROJ:
   https://proj.org/operations/projections/laea.html
   Ref: Snyder, J. P. (1987). Map projections--A working manual (pp. 182-190).
*)

(* Inverse: (x,y) -> (lon, lat) *)
let laea_to_wgs84 x y =
  let x' = x -. fe in
  let y' = y -. fn in

  (* EPSG:9820 requires D scaling: rho = sqrt((x'/D)^2 + (D*y')^2) *)
  let x_scaled = x' /. d in
  let y_scaled = d *. y' in
  let rho = sqrt ((x_scaled *. x_scaled) +. (y_scaled *. y_scaled)) in

  if rho < 1.0e-9 then (lon0 *. rad_to_deg, lat0 *. rad_to_deg)
  else
    (* EPSG:9820 ellipsoidal LAEA inverse formulas *)
    let ce = 2.0 *. asin (rho /. (2.0 *. r_q)) in
    let sin_ce = sin ce in
    let cos_ce = cos ce in

    (* beta0 (sin_beta0, cos_beta0) pre-calculated at module level *)

    (* Compute sin(beta') *)
    let sin_beta' =
      (cos_ce *. sin_beta0) +. (y_scaled *. sin_ce *. cos_beta0 /. rho)
    in

    (* Recalculate Latitude from Authalic Latitude (beta') *)
    let beta' = asin sin_beta' in
    let phi =
      beta'
      +. ((e2 /. 3.0)
         +. (31.0 *. e2 *. e2 /. 180.0)
         +. (517.0 *. e2 *. e2 *. e2 /. 5040.0))
         *. sin (2.0 *. beta')
      +. ((23.0 *. e2 *. e2 /. 360.0) +. (251.0 *. e2 *. e2 *. e2 /. 3780.0))
         *. sin (4.0 *. beta')
      +. (761.0 *. e2 *. e2 *. e2 /. 45360.0 *. sin (6.0 *. beta'))
    in

    (* EPSG:9820: lon = lonO + atan2(x_scaled * sin_C, D*rho*cos_beta0*cos_C - D^2*y'*sin_beta0*sin_C) 
       Simplify using our scaled values *)
    let lam =
      lon0
      +. atan2 (x_scaled *. sin_ce)
           ((rho *. cos_ce *. cos_beta0) -. (y_scaled *. sin_beta0 *. sin_ce))
    in

    (lam *. rad_to_deg, phi *. rad_to_deg)

(* Direct: (lon, lat) -> (x,y) *)
(* Note: Not strictly needed for the extraction pipeline (we have coords in 3035),
   but good for verification. *)
let wgs84_to_laea lon lat =
  let lam = lon *. deg_to_rad in
  let phi = lat *. deg_to_rad in

  let q = q_from_lat (sin phi) in
  let sin_beta = q /. q_p in
  let cos_beta = sqrt (1.0 -. (sin_beta *. sin_beta)) in

  (* sin_beta0, cos_beta0 pre-calculated at module level *)
  let b =
    r_q
    *. sqrt
         (2.0
         /. (1.0 +. (sin_beta0 *. sin_beta)
            +. (cos_beta0 *. cos_beta *. cos (lam -. lon0))))
  in

  let x = fe +. (b *. d *. cos_beta *. sin (lam -. lon0)) in
  let y =
    fn
    +. b /. d
       *. ((cos_beta0 *. sin_beta)
          -. (sin_beta0 *. cos_beta *. cos (lam -. lon0)))
  in
  (x, y)

(* Tests *)
let test () =
  Printf.printf "Testing EPSG:3035 Projections...\n";
  let tx, ty = (fe, fn) in
  let l, p = laea_to_wgs84 tx ty in
  Printf.printf
    "Origin (%.1f, %.1f) -> Lon: %.6f Lat: %.6f (Expected: 10.0, 52.0)\n" tx ty
    l p;

  (* Test round trip *)
  let rx, ry = wgs84_to_laea l p in
  Printf.printf "Round Trip -> X: %.2f Y: %.2f\n" rx ry;
  if abs_float (rx -. fe) < 0.01 && abs_float (ry -. fn) < 0.01 then
    Printf.printf "Integration Test: PASSED\n"
  else Printf.printf "Integration Test: FAILED\n"

let () = if Array.length Sys.argv > 1 && Sys.argv.(1) = "test" then test ()
