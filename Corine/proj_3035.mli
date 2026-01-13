(** EPSG:3035 (Lambert Azimuthal Equal Area) projection utilities *)

val laea_to_wgs84 : float -> float -> float * float
(** Convert from EPSG:3035 (x, y) coordinates to WGS84 (lon, lat) *)

val wgs84_to_laea : float -> float -> float * float
(** Convert from WGS84 (lon, lat) to EPSG:3035 (x, y) coordinates *)

val test : unit -> unit
(** Run integration tests for projection accuracy *)
