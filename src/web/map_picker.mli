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
      (** where to open when entering the region with no position of its own: a
          rectangle bounding an irregular coast has much of its area at sea, and
          its middle is not necessarily on land *)
  view_lon : float;
}
(** A rectangle the map is confined to: the centre never leaves it and zooming
    out stops once it covers the viewport. Regions are far enough apart that
    panning between them is pointless, so the map offers a switch instead.

    Each must lie inside both the elevation coverage and the basemap's. The
    latter is roughly France-shaped -- a buffer into Italy is served,
    Switzerland beyond the border is not, Corsica is detached -- so one
    rectangle cannot describe it and several regions are needed. *)

val create :
  regions:region list ->
  in_range:(lat:float -> lon:float -> bool) ->
  on_select:(lat:float -> lon:float -> unit) ->
  lat:float ->
  lon:float ->
  unit
(** [create ~regions ~in_range ~on_select] builds the map, appends it to the
    document body hidden, and returns a function that opens it centred on the
    given position, in the region holding it or else the nearest one.

    [in_range] gates the confirmation as the authority on what the renderer can
    actually load. [regions] are expected to be inside it already, so a
    rejection means the two disagree rather than something the user can act on.

    [on_select] receives the crosshair position; it is responsible for closing
    whatever opened the map. Raises [Invalid_argument] if [regions] is empty. *)
