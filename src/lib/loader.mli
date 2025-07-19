module Make (_ : Tiff.READER) : sig
  val f :
    size:int ->
    lat:float ->
    lon:float ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t Lwt.t

  val prefetch : size:int -> lat:float -> lon:float -> unit Lwt.t

  val in_range :
    size:int ->
    min_lat:int ->
    max_lat:int ->
    min_lon:int ->
    max_lon:int ->
    lat:float ->
    lon:float ->
    bool
end
