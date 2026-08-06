(* See projection.mli. Both families here are conformal projections of GRS80,
   which is what lets a caller reduce either of them to a single 2x2 matrix over
   the extent of a refinement ring. *)

let a = 6378137.0
let f = 1. /. 298.257222101
let e = sqrt ((2. *. f) -. (f *. f))
let e2 = e *. e
let deg = Float.pi /. 180.

(* Lambert conformal conic with two standard parallels, in the form Snyder gives
   it. [n] and [big_f] depend only on the parallels, so they are computed once
   when the CRS is built rather than per point. *)
type lcc = {
  lcc_lon0 : float; (* degrees *)
  lcc_x0 : float;
  lcc_y0 : float;
  lcc_n : float;
  lcc_big_f : float;
  lcc_rho0 : float;
}

(* Transverse Mercator, Snyder's series, which is well inside a millimetre over
   the width of a UTM zone. *)
type tm = {
  tm_lon0 : float; (* degrees *)
  tm_k0 : float;
  tm_x0 : float;
  tm_y0 : float;
}

type kind = Lcc of lcc | Tm of tm

type t = {
  name : string;
  epsg : string;
  kind : kind;
  lat_min : float;
  lat_max : float;
  lon_min : float;
  lon_max : float;
}

let name p = p.name
let epsg p = p.epsg

(* Snyder's m and t, the two auxiliaries the conic is written in. *)
let conic_m phi = cos phi /. sqrt (1. -. (e2 *. sin phi *. sin phi))

let conic_t phi =
  tan ((Float.pi /. 4.) -. (phi /. 2.))
  /. (((1. -. (e *. sin phi)) /. (1. +. (e *. sin phi))) ** (e /. 2.))

let lcc ~lon0 ~lat0 ~lat1 ~lat2 ~x0 ~y0 =
  let m1 = conic_m (lat1 *. deg) and m2 = conic_m (lat2 *. deg) in
  let t1 = conic_t (lat1 *. deg) and t2 = conic_t (lat2 *. deg) in
  let n = log (m1 /. m2) /. log (t1 /. t2) in
  let big_f = m1 /. (n *. (t1 ** n)) in
  {
    lcc_lon0 = lon0;
    lcc_x0 = x0;
    lcc_y0 = y0;
    lcc_n = n;
    lcc_big_f = big_f;
    lcc_rho0 = a *. big_f *. (conic_t (lat0 *. deg) ** n);
  }

let lcc_forward c ~lat ~lon =
  let rho = a *. c.lcc_big_f *. (conic_t (lat *. deg) ** c.lcc_n) in
  let theta = c.lcc_n *. (lon -. c.lcc_lon0) *. deg in
  (c.lcc_x0 +. (rho *. sin theta), c.lcc_y0 +. c.lcc_rho0 -. (rho *. cos theta))

let tm_forward c ~lat ~lon =
  let ep2 = e2 /. (1. -. e2) in
  let p = lat *. deg in
  let sp = sin p and cp = cos p in
  let big_n = a /. sqrt (1. -. (e2 *. sp *. sp)) in
  let t = tan p *. tan p in
  let big_c = ep2 *. cp *. cp in
  let big_a = (lon -. c.tm_lon0) *. deg *. cp in
  let big_m =
    a
    *. ((1. -. (e2 /. 4.)
        -. (3. *. e2 *. e2 /. 64.)
        -. (5. *. e2 *. e2 *. e2 /. 256.))
        *. p
       -. ((3. *. e2 /. 8.)
          +. (3. *. e2 *. e2 /. 32.)
          +. (45. *. e2 *. e2 *. e2 /. 1024.))
          *. sin (2. *. p)
       +. ((15. *. e2 *. e2 /. 256.) +. (45. *. e2 *. e2 *. e2 /. 1024.))
          *. sin (4. *. p)
       -. (35. *. e2 *. e2 *. e2 /. 3072. *. sin (6. *. p)))
  in
  let a2 = big_a *. big_a in
  let x =
    c.tm_x0
    +. c.tm_k0 *. big_n
       *. (big_a
          +. ((1. -. t +. big_c) *. big_a *. a2 /. 6.)
          +. (5. -. (18. *. t) +. (t *. t) +. (72. *. big_c) -. (58. *. ep2))
             *. big_a *. a2 *. a2 /. 120.)
  in
  let y =
    c.tm_y0
    +. c.tm_k0
       *. (big_m
          +. big_n *. tan p
             *. ((a2 /. 2.)
                +. (5. -. t +. (9. *. big_c) +. (4. *. big_c *. big_c))
                   *. a2 *. a2 /. 24.
                +. (61. -. (58. *. t) +. (t *. t) +. (600. *. big_c)
                  -. (330. *. ep2))
                   *. a2 *. a2 *. a2 /. 720.))
  in
  (x, y)

let forward p ~lat ~lon =
  match p.kind with
  | Lcc c -> lcc_forward c ~lat ~lon
  | Tm c -> tm_forward c ~lat ~lon

(* Central differences over a hundredth of an arcsecond, about a metre on the
   ground. Wide enough that catastrophic cancellation is nowhere near -- the
   coordinates are of order 1e6 m and the differences of order 1 m, so a dozen
   significant digits survive -- and far too narrow for the truncation error of
   a central difference to matter on a function whose curvature scale is the
   Earth's radius. *)
let jacobian p ~lat ~lon =
  let h = 1e-5 in
  let xe, ye = forward p ~lat ~lon:(lon +. h) in
  let xw, yw = forward p ~lat ~lon:(lon -. h) in
  let xn, yn = forward p ~lat:(lat +. h) ~lon in
  let xs, ys = forward p ~lat:(lat -. h) ~lon in
  let d = 1. /. (2. *. h) in
  ((xe -. xw) *. d, (xn -. xs) *. d, (ye -. yw) *. d, (yn -. ys) *. d)

(* Metropolitan France. The bounds are the ones the WMS capabilities advertise
   for IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE.LAMB93, which offers
   EPSG:2154 and nothing else. *)
let lambert93 =
  {
    name = "LAMB93";
    epsg = "EPSG:2154";
    kind =
      Lcc (lcc ~lon0:3. ~lat0:46.5 ~lat1:44. ~lat2:49. ~x0:700000. ~y0:6600000.);
    lat_min = 41.328656396190425;
    lat_max = 51.093704078352104;
    lon_min = -5.1558878596855333;
    lon_max = 9.5704320859312553;
  }

(* La Reunion: RGR92 / UTM zone 40 South, likewise the only CRS its layer
   offers. Same ellipsoid, so nothing above is territory-specific. *)
let rgr92_utm40s =
  {
    name = "RGR92UTM40S";
    epsg = "EPSG:2975";
    kind =
      Tm { tm_lon0 = 57.; tm_k0 = 0.9996; tm_x0 = 500000.; tm_y0 = 10000000. };
    lat_min = -21.390939072673053;
    lat_max = -20.864901560198948;
    lon_min = 55.209470950963471;
    lon_max = 55.844202311454637;
  }

let all = [ lambert93; rgr92_utm40s ]

let of_location ~lat ~lon =
  List.find_opt
    (fun p ->
      lat >= p.lat_min && lat <= p.lat_max && lon >= p.lon_min
      && lon <= p.lon_max)
    all
