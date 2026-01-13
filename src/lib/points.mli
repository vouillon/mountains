type coord = { lat : float; lon : float }
(** Latitude and longitude coordinates *)

type t = { name : string; coord : coord; elevation : int option }
(** Point of interest with name, coordinates and optional elevation *)

val load : string -> t list
(** Load points of interest from a GeoJSON string *)

val find : coord -> coord -> string -> t list
(** Find points of interest within a bounding box in a GeoJSON string. The
    bounding box is defined by two corners (min_lat, min_lon) and (max_lat,
    max_lon). *)
