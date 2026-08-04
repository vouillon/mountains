type trackpoint = { lat : float; lon : float; ele : float option }

type waypoint = {
  name : string;
  lat : float;
  lon : float;
  ele : float option;
  desc : string option;
}

type track = { name : string; points : trackpoint list }
type gpx_data = { tracks : track list; waypoints : waypoint list }

val parse : string -> gpx_data
