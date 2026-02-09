(* proj_2975.ml - RGR92 / UTM zone 40S (EPSG:2975) to WGS84 *)

let pi = 3.14159265358979323846
let deg_to_rad = pi /. 180.0
let rad_to_deg = 180.0 /. pi

(* GRS80 Ellipsoid constants *)
let a = 6378137.0
let f = 1.0 /. 298.257222101
let e2 = f *. (2.0 -. f) (* First eccentricity squared *)
let e4 = e2 *. e2
let e6 = e4 *. e2
let ep2 = e2 /. (1.0 -. e2) (* Second eccentricity squared *)

(* EPSG:2975 Projection Parameters *)
let k0 = 0.9996 (* Scale Factor *)
let lon0 = 57.0 *. deg_to_rad (* Central Meridian *)
let lat0 = 0.0 *. deg_to_rad (* Latitude of Origin *)
let fe = 500_000.0 (* False Easting *)
let fn = 10_000_000.0 (* False Northing *)

(* Meridional distance calculation *)
let get_m lat =
  let c1 = 1.0 -. (e2 /. 4.0) -. (3.0 *. e4 /. 64.0) -. (5.0 *. e6 /. 256.0) in
  let c2 = (3.0 *. e2 /. 8.0) +. (3.0 *. e4 /. 32.0) +. (45.0 *. e6 /. 1024.0) in
  let c3 = (15.0 *. e4 /. 256.0) +. (45.0 *. e6 /. 1024.0) in
  let c4 = 35.0 *. e6 /. 3072.0 in
  a
  *. ((c1 *. lat)
     -. (c2 *. sin (2.0 *. lat))
     +. (c3 *. sin (4.0 *. lat))
     -. (c4 *. sin (6.0 *. lat)))

let m0 = get_m lat0

(* Direct: (lon, lat) -> (x,y) *)
let of_wgs84 lon lat =
  let lam = lon *. deg_to_rad in
  let phi = lat *. deg_to_rad in

  let dlam = lam -. lon0 in
  let sin_phi = sin phi in
  let cos_phi = cos phi in
  let tan_phi = sin_phi /. cos_phi in
  let tan2_phi = tan_phi *. tan_phi in

  let n = a /. sqrt (1.0 -. (e2 *. sin_phi *. sin_phi)) in
  let t = tan2_phi in
  let c = ep2 *. cos_phi *. cos_phi in
  let a_val = dlam *. cos_phi in

  let m = get_m phi in

  let x =
    fe
    +. (k0
       *. n
       *. (a_val
          +. ((1.0 -. t +. c) *. (a_val ** 3.0) /. 6.0)
          +. ((5.0 -. (18.0 *. t) +. (t *. t) +. (72.0 *. c) -. (58.0 *. ep2))
             *. (a_val ** 5.0)
             /. 120.0)))
  in
  let y =
    fn
    +. (k0
       *. ((m -. m0)
          +. (n
             *. tan_phi
             *. ((a_val *. a_val /. 2.0)
                +. ((5.0 -. t +. (9.0 *. c) +. (4.0 *. c *. c))
                   *. (a_val ** 4.0)
                   /. 24.0)
                +. ((61.0 -. (58.0 *. t) +. (t *. t) +. (600.0 *. c)
                    -. (330.0 *. ep2))
                   *. (a_val ** 6.0)
                   /. 720.0)))))
  in
  (x, y)

(* Inverse: (x,y) -> (lon, lat) *)
let to_wgs84 x y =
  let x' = x -. fe in
  let y' = y -. fn in

  let m = m0 +. (y' /. k0) in
  let e1 = (1.0 -. sqrt (1.0 -. e2)) /. (1.0 +. sqrt (1.0 -. e2)) in

  let mu =
    m
    /. (a
       *. (1.0
          -. (e2 /. 4.0)
          -. (3.0 *. e4 /. 64.0)
          -. (5.0 *. e6 /. 256.0)))
  in
  let phi1 =
    mu
    +. (((3.0 *. e1 /. 2.0) -. (27.0 *. (e1 ** 3.0) /. 32.0)) *. sin (2.0 *. mu))
    +. (((21.0 *. (e1 ** 2.0) /. 16.0) -. (55.0 *. (e1 ** 4.0) /. 32.0))
       *. sin (4.0 *. mu))
    +. (151.0 *. (e1 ** 3.0) /. 96.0 *. sin (6.0 *. mu))
    +. (1097.0 *. (e1 ** 4.0) /. 512.0 *. sin (8.0 *. mu))
  in

  let sin_phi1 = sin phi1 in
  let cos_phi1 = cos phi1 in
  let tan_phi1 = sin_phi1 /. cos_phi1 in
  let tan2_phi1 = tan_phi1 *. tan_phi1 in

  let c1 = ep2 *. cos_phi1 *. cos_phi1 in
  let t1 = tan2_phi1 in
  let n1 = a /. sqrt (1.0 -. (e2 *. sin_phi1 *. sin_phi1)) in
  let r1 = a *. (1.0 -. e2) /. ((1.0 -. (e2 *. sin_phi1 *. sin_phi1)) ** 1.5) in
  let d = x' /. (n1 *. k0) in

  let lat =
    phi1
    -. (n1
       *. tan_phi1
       /. r1
       *. ((d *. d /. 2.0)
          -. ((5.0 +. (3.0 *. t1) +. (10.0 *. c1) -. (4.0 *. c1 *. c1)
              -. (9.0 *. ep2))
             *. (d ** 4.0)
             /. 24.0)
          +. ((61.0 +. (90.0 *. t1) +. (298.0 *. c1) +. (45.0 *. t1 *. t1)
              -. (252.0 *. ep2) -. (3.0 *. c1 *. c1))
             *. (d ** 6.0)
             /. 720.0)))
  in
  let lon =
    lon0
    +. ((d
        -. ((1.0 +. (2.0 *. t1) +. c1) *. (d ** 3.0) /. 6.0)
        +. ((5.0
            -. (2.0 *. c1)
            +. (28.0 *. t1)
            -. (3.0 *. c1 *. c1)
            +. (8.0 *. ep2)
            +. (24.0 *. t1 *. t1))
           *. (d ** 5.0)
           /. 120.0))
       /. cos_phi1)
  in
  (lon *. rad_to_deg, lat *. rad_to_deg)

(* Tests *)
let test () =
  Printf.printf "Testing EPSG:2975 Projections...\n";
  (* Réunion origin (approx) *)
  let lon, lat = (55.5, -21.0) in
  let x, y = of_wgs84 lon lat in
  Printf.printf "Lon: %.6f Lat: %.6f -> X: %.2f Y: %.2f\n" lon lat x y;

  let rl, rp = to_wgs84 x y in
  Printf.printf "Round Trip -> Lon: %.6f Lat: %.6f\n" rl rp;
  if abs_float (rl -. lon) < 0.00001 && abs_float (rp -. lat) < 0.00001 then
    Printf.printf "Integration Test: PASSED\n"
  else Printf.printf "Integration Test: FAILED\n"

let () = if Array.length Sys.argv > 1 && Sys.argv.(1) = "test" then test ()