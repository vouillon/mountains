(** Full-screen map for choosing a location.

    Shows IGN's Plan IGN basemap with the LiDAR HD bare earth relief shaded on
    top, served as ordinary Web Mercator tiles. A crosshair fixed at the centre
    of the viewport is the selection: panning and zooming move the map under it,
    and the position is only committed on an explicit confirmation, so a stray
    tap can never reload the terrain somewhere unintended. *)

type region = {
  name : string;  (** shown on the region switch *)
  min_lat : float;
  max_lat : float;
  min_lon : float;
  max_lon : float;
  view_lat : float;
      (** where the region is first shown, for want of anywhere better: a
          rectangle bounding an irregular coast has much of its area at sea, and
          its middle is not necessarily on land. Only the starting point --
          afterwards the region reopens wherever it was last left *)
  view_lon : float;
}
(** A rectangle the map is confined to: the centre never leaves it and zooming
    out stops once it covers the viewport. Regions are far enough apart that
    panning between them is pointless, so the map offers a switch instead.

    Each must lie inside both the elevation coverage and the basemap's. The
    latter is roughly France-shaped -- a buffer into Italy is served,
    Switzerland beyond the border is not, the overseas territories are islands
    of their own -- so one rectangle cannot describe it and several regions are
    needed. *)

val create :
  regions:region list ->
  in_range:(lat:float -> lon:float -> bool) ->
  traces:(unit -> (float * float) array list) ->
  landmarks:(float * float) list ->
  on_select:(lat:float -> lon:float -> unit) ->
  lat:float ->
  lon:float ->
  unit
(** [create ~regions ~in_range ~on_select] builds the map, appends it to the
    document body hidden, and returns a function that opens it centred on the
    given position, in the region holding it or else the nearest one.

    Each region remembers where it was last left, scale included, so reopening
    the map or switching back to a region resumes there rather than starting
    over. Only the centre is overridden on opening, by the position asked for.

    [traces] gives the tracks to draw over the map, each as its points in
    latitude and longitude, and is read afresh on every opening. Drawing the
    loaded GPX traces is what lets a location be chosen against them -- a
    viewpoint over a route rather than a bare coordinate.

    [landmarks] are marked with a dot: the featured locations, so the places
    already known to be worth looking from can be seen while choosing, and aimed
    at. Positions only -- they are not labelled, which at the zoom where several
    are on screen at once would be more clutter than help.

    [in_range] gates the confirmation as the authority on what the renderer can
    actually load. [regions] are expected to be inside it already, so a
    rejection means the two disagree rather than something the user can act on.

    [on_select] receives the crosshair position; it is responsible for closing
    whatever opened the map. Raises [Invalid_argument] if [regions] is empty. *)
