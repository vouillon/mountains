(** The browser cache the tiles are kept in *)

val name : string
(** The name of the Cache Storage cache that holds the tiles, shared by the
    service worker, which fills it and serves from it, and the page, which looks
    into it. *)
