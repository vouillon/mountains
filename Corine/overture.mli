(** Water areas and POIs from Overture Maps, in place of the Overpass API *)

val load_water : string -> Osm_fetch.water_feature list
(** [load_water file] reads the water areas of a GeoJSON-sequence file, a
    feature a line, as [overturemaps download -f geojsonseq -t water] writes it
    (fetch_overture.sh): the sea, which OSM has as a coastline only and Overture
    as polygons, and the inland water. *)

val load_pois : string -> Poi_fetch.poi list
(** [load_pois file] reads the named peaks and saddles of such a file of the
    [land] type. *)

val water_of_lines : string list -> Osm_fetch.water_feature list

val pois_of_lines : string list -> Poi_fetch.poi list
(** The same, of the lines themselves. Exposed for tests. *)
