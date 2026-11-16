(* The tiles are immutable and expensive to fetch, so they live in a cache of
   their own, which survives deploys (see service_worker.ml). The service
   worker and the page both name it, here.

   It is renamed when hosted tiles change under the names they had, as cached
   ones are never asked for again: v2 for the land cover tiles with the water
   and the summits from Overture, which the client of that time needs (with
   the tiles before them it fills the sea with land); v3 for the land cover
   tiles in CLC6, which the client of that time alone reads. The service worker
   drops the cache of the former name with those of the former builds, and with
   it the elevation it held, which is fetched again as it is asked for. *)
let name = "v3"
