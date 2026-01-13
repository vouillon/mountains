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
