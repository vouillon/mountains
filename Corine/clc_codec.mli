(** The geometry of a layer of a CLC6 tile (land cover or water): its features
    coded as their boundary rings, shared with their neighbours, and a walk of
    their triangles. See clc_codec.ml for the format. *)

exception Unsupported of string
(** A feature the coding does not apply to: a vertex inside it or in no
    triangle, a triangle repeating a vertex, an edge in more than two triangles,
    or triangles not wound alike. *)

type feature = {
  code : int;  (** CORINE class code, below 65536 *)
  xs : int array;  (** quantized coordinates, below [2^bits] *)
  ys : int array;
  tris : int array;  (** three vertex indices per triangle *)
}

type layer = {
  count : int;  (** features *)
  verts : int;  (** vertices once decoded: the positions of the rings *)
  indices : int;  (** three per triangle *)
  arcs : int;  (** arcs stored for a later feature to name *)
  max_walk : int;  (** the longest walk round a component *)
  streams : string list;
      (** class codes, topology, arcs, coordinate bytes (x then y, most
          significant byte first), connectivity *)
}

val encode : bits:int -> feature array -> layer
(** [bits] is 16 or 24. Raises [Unsupported] naming the feature. *)

val decode : bits:int -> count:int -> string list -> feature array
(** The features back, with the same class codes and the same triangles as
    triples of points wound the same way; their vertices are the positions of
    their rings, in another order (a pinch is a vertex of each of its rings). *)

val differences : feature array -> feature array -> string option
(** [None] when the two hold the same features: the same class codes and the
    same triangles as triples of points wound the same way, whatever the vertex
    numbering and the order of the triangles. Otherwise the first difference. *)
