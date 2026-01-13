(** WKB (Well-Known Binary) and GeoPackage geometry decoding *)

type point = { x : float; y : float }

type geometry =
  | Point of point
  | Polygon of point list list
  | MultiPolygon of point list list list
  | Unknown of int * string  (** Supported geometric types *)

type bbox = { min_x : float; min_y : float; max_x : float; max_y : float }
(** Bounding box *)

val decode_wkb : string -> geometry option
(** Decode a WKB or GeoPackage binary string into a geometry structure *)

val get_bbox : geometry -> bbox
(** Compute the bounding box of a geometry *)

val to_string : geometry -> string
(** String representation of a geometry (WKT-like) *)
