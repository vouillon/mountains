(* 
   Simple astronomical calculations for Sun position.
   Based on common formulas (e.g. from NOAA or "Astronomical Algorithms" by Meeus).
*)

let pi = 4. *. atan 1.
let rad_of_deg d = d *. pi /. 180.

(* Julian Date from Unix timestamp *)
let julian_day t =
  let t = t /. 86400. in
  t +. 2440587.5

(* Position of the sun in ECI (Earth Centered Inertial) coordinates *)
let sun_position_eci t =
  let jd = julian_day t in
  let n = jd -. 2451545.0 in
  let l = 280.460 +. (0.9856474 *. n) in
  let g = 357.528 +. (0.9856003 *. n) in
  let l = mod_float l 360. in
  let g = mod_float g 360. in
  let l = if l < 0. then l +. 360. else l in
  let g = if g < 0. then g +. 360. else g in

  let lambda =
    l +. (1.915 *. sin (rad_of_deg g)) +. (0.020 *. sin (rad_of_deg (2. *. g)))
  in
  let epsilon = 23.439 -. (0.0000004 *. n) in

  let lambda_rad = rad_of_deg lambda in
  let epsilon_rad = rad_of_deg epsilon in

  let alpha = atan2 (cos epsilon_rad *. sin lambda_rad) (cos lambda_rad) in
  let delta = asin (sin epsilon_rad *. sin lambda_rad) in
  (alpha, delta)

(* Convert to local horizontal coordinates (Azimuth, Elevation) *)
let position ~lat ~lon ~time =
  let alpha, delta = sun_position_eci time in

  let jd = julian_day time in
  let n = jd -. 2451545.0 in
  (* Greenwich Mean Sidereal Time *)
  let gmst = 18.697374558 +. (24.06570982441908 *. n) in
  let gmst = mod_float gmst 24. in
  let gmst = if gmst < 0. then gmst +. 24. else gmst in
  let gmst_rad = rad_of_deg (gmst *. 15.) in

  let lon_rad = rad_of_deg lon in
  let lat_rad = rad_of_deg lat in

  (* Local Sidereal Time *)
  let lmst = gmst_rad +. lon_rad in

  (* Hour Angle *)
  let h = lmst -. alpha in

  let sin_el =
    (sin lat_rad *. sin delta) +. (cos lat_rad *. cos delta *. cos h)
  in
  (* let el = asin sin_el in *)

  (* Normalized *)

  (* Azimuth *)

  (* East Positive? No, let's stick to standard math first *)

  (* Standard formula: tan(Az) = sin(H) / (cos(H)sin(lat) - tan(dec)cos(lat)) *)
  (* But we want a 3D vector. *)

  (* Let's convert to ENU (East, North, Up) vector directly if possible, or computing Az/El first *)
  (* x = - cos(delta) * sin(h) 
     y = sin(delta) * cos(lat) - cos(delta) * sin(lat) * cos(h)
     z = sin(el) *)

  (* Points West if h > 0? No wait. *)
  (* Let's verify coordinates:
     h = LST - alpha. 
     If Sun is at Meridian (noon), h=0.
     x = 0.
     y = sin(delta)*cos(lat) - cos(delta)*sin(lat) = sin(delta - lat). 
     If delta = lat, Sun is at zenith (x=0, y=0, z=1). Correct-ish.
     
     Wait, standard formula for cartesian in local tangent plane (X=North, Y=East, Z=Up)?
     Or (X=East, Y=North, Z=Up)?
     Let's assume X=East, Y=North, Z=Up for consistency with general math.
  *)

  let x_east = -.cos delta *. sin h in
  let y_north =
    (sin delta *. cos lat_rad) -. (cos delta *. sin lat_rad *. cos h)
  in
  let z_up = sin_el in

  let len =
    sqrt ((x_east *. x_east) +. (y_north *. y_north) +. (z_up *. z_up))
  in
  (x_east /. len, y_north /. len, z_up /. len)

(*
let%test_module "Sun Position Tests" =
  (module struct
    (* Helper to check proximity *)
    (* Helper to check proximity *)

    let%test "Approximate Noon at Equator on Equinox" =
      (* March 21, 2024, 12:00 UTC. JD approx 2460391.0 *)
      (* Using a fixed timestamp for consistency. 
          2024-03-20 12:00:00 UTC is approx equinox. Timestamp: 1710936000 *)
      let t = 1710936000. in
      let _, _, z = position ~lat:0. ~lon:0. ~time:t in
      (* Sun should be overhead. z near 1. *)
      (* Actually, equinox is roughly when sun is at equator. *)
      z > 0.95

    let%test "Midnight should be down" =
      let t = 1710936000. +. 43200. in
      (* +12h *)
      let _, _, z = position ~lat:0. ~lon:0. ~time:t in
      z < 0.
  end)
*)
