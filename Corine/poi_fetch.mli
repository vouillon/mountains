(** Utility module for fetching POIs from Overpass API *)

type poi_type = Peak | Saddle

type poi = {
  name : string;
  lat : float;
  lon : float;
  elevation : int option;
  poi_type : poi_type;
}

val fetch_pois :
  min_lat:float -> min_lon:float -> max_lat:float -> max_lon:float -> poi list

val response_error : string -> string option
(** [response_error body] returns a description of why an Overpass response is
    unusable (not JSON, malformed, or carrying a server "remark"), or [None] if
    it looks complete. Exposed for tests. *)

val parse_overpass_elements : string -> poi list
(** Parse an Overpass JSON response into POIs. Exposed for tests. *)
