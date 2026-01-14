module Make (_ : Tiff.READER) : sig
  val f :
    size:int ->
    lat:float ->
    lon:float ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t Lwt.t
  (** Load a DEM tile of the given size at the specified latitude/longitude.
      Size is typically the power of two size of the clipmap level. *)

  val prefetch : size:int -> lat:float -> lon:float -> unit Lwt.t
  (** Proactively load a DEM tile into cache. *)

  val in_range :
    size:int ->
    min_lat:int ->
    max_lat:int ->
    min_lon:int ->
    max_lon:int ->
    lat:float ->
    lon:float ->
    bool
  (** Check if a tile at (lat, lon) is within the specified bounding box. *)
end
