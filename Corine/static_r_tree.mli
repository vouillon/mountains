(** High-performance Static Dense R-tree with Hilbert indexing. This module
    provides a pointerless, implicit R-tree stored in flat arrays. *)

type t
(** The abstract type of a static R-tree. *)

val build :
  verts:float array ->
  vert_idx:int array ->
  items:int array ->
  min_x:float ->
  min_y:float ->
  max_x:float ->
  max_y:float ->
  t
(** [build ~verts ~vert_idx ~items ~min_x ~min_y ~max_x ~max_y] constructs a
    static R-tree. [verts] contains point coordinates, [vert_idx] maps item node
    indices to vertex indices in [verts], and [items] is the list of active item
    node indices to index. *)

val lookup : t -> float -> float -> float -> float -> (int -> unit) -> unit
(** [lookup t q_xmin q_ymin q_xmax q_ymax callback] searches the tree. *)
