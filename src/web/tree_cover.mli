(** Tree cover density and dominant leaf type over Europe (EEA Copernicus
    high-resolution layers, 2018), in blocks around a location: level 0 at ~20 m
    over ~20 km, level 1 at ~80 m over ~80 km. *)

val size : int
(** Samples per side of a block. *)

val tile : int
(** Samples per side of a tile of the responses. *)

type t = {
  west : float;
  south : float;
  east : float;
  north : float;  (** The block's bounds, degrees. *)
  density : Brr.Tarray.uint8;
      (** The density response, as received: percent, above 100 for no data. *)
  leaf_type : Brr.Tarray.uint8;
      (** The leaf type response: 1 broadleaved, 2 coniferous, 0 neither. *)
  density_tiles : int array;
  leaf_type_tiles : int array;
      (** Byte offsets in each response of its [tile]-square tiles, rows of
          tiles from the north, each row of a tile from the north; 0 for an
          empty tile. *)
}

val block_id : level:int -> lat:float -> lon:float -> string
(** Identity of the block of [level] {!fetch} would ask for at [lat]/[lon]. *)

val fetch :
  ?timeout_s:float ->
  level:int ->
  lat:float ->
  lon:float ->
  unit ->
  t option Lwt.t
(** Fetch the block of [level] around [lat]/[lon]. Never fails: [None] when it
    could not be had within [timeout_s] (60 by default). *)
