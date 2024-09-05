module Make (_ : Tiff.READER) : sig
  val f :
    size:int ->
    lat:float ->
    lon:float ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t Lwt.t
end
