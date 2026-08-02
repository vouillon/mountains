open Fut.Result_syntax

(* The application shell goes to a cache named after a digest of its contents:
   a deploy changing any of them uses a fresh cache, so cache-first assets (the
   worker, the Wasm modules) can no longer be served from a previous build.
   The tiles are immutable and expensive to fetch, so they live in a separate,
   persistent cache which survives deploys. Its name is also hard-coded in the
   main thread ([Dem_loader.prefetch], [Clc_loader.prefetch_tile],
   [Reader.prefetch]) and must be kept in sync. *)
let app_cache_name = Jstr.v ("mountains-" ^ Sw_version.version)
let data_cache_name = Jstr.v "v1"

let open_cache name =
  Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ()) name

(* Near-field high-resolution elevation ([Hd_dem]) comes from IGN's WMTS.
   Those responses are as immutable and as expensive as the hosted tiles --
   the tile grid is fixed, so nearby locations share URLs -- and belong in the
   persistent data cache. Unlike the hosted tiles, a WMTS request carries its
   whole identity in its query string, so these are the entries that must be
   stored and matched with the query included. *)
let is_hd_dem url = String.starts_with ~prefix:"https://data.geopf.fr/" url

let is_tile url =
  String.ends_with ~suffix:".dem" url
  || String.ends_with ~suffix:".clc" url
  || String.ends_with ~suffix:".tif" url
  || is_hd_dem url

(* [Cache.Storage.match'] searches every cache, so application caches from
   previous builds must be dropped, not just left unused. *)
let delete_old_caches () =
  let caches = Brr_io.Fetch.caches () in
  let* names = Brr_io.Fetch.Cache.Storage.keys caches in
  let open Fut.Syntax in
  let+ _ =
    Fut.of_list
      (List.filter_map
         (fun name ->
           if Jstr.equal name app_cache_name || Jstr.equal name data_cache_name
           then None
           else Some (Brr_io.Fetch.Cache.Storage.delete caches name))
         names)
  in
  Ok ()

(* Earlier versions of this worker stored the application shell in the data
   cache; those entries must go, or [Cache.Storage.match'] would keep serving
   them for ever. *)
let prune_data_cache () =
  let* cache = open_cache data_cache_name in
  let* requests = Brr_io.Fetch.Cache.keys cache in
  let open Fut.Syntax in
  let+ _ =
    Fut.of_list
      (List.filter_map
         (fun request ->
           if is_tile (Jstr.to_string (Brr_io.Fetch.Request.url request)) then
             None
           else Some (Brr_io.Fetch.Cache.delete cache request))
         requests)
  in
  Ok ()

let store_in_cache request response =
  let url = Brr_io.Fetch.Request.url request in
  let* cache =
    open_cache
      (if is_tile (Jstr.to_string url) then data_cache_name else app_cache_name)
  in
  (* Strip query string to avoid duplicate cache entries *)
  let cache_url =
    if is_hd_dem (Jstr.to_string url) then url
    else
      match Brr.Uri.of_jstr url with
      | Ok uri -> (
          match Brr.Uri.with_uri ~query:Jstr.empty uri with
          | Ok uri -> Brr.Uri.to_jstr uri
          | Error _ -> url)
      | Error _ -> url
  in
  let cache_request = Brr_io.Fetch.Request.v cache_url in
  Brr_io.Fetch.Cache.put cache cache_request
    (Brr_io.Fetch.Response.of_response response)

(* Caching is best effort: a failed put (a full cache raises
   [QuotaExceededError]) must not turn a successful download into an error
   response. *)
let put_in_cache request response =
  let open Fut.Syntax in
  let+ _ = store_in_cache request response in
  Ok ()

let match_opts =
  let o = Jv.obj [| ("ignoreSearch", Jv.true') |] in
  Obj.magic o

(* [put_in_cache] stores error responses too. With [serve_not_found], a cached
   404 is served rather than skipped: a WMTS elevation tile answers 404 when it
   lies outside French territory, which is permanent, and asking the network
   again on every visit to a location at the edge of coverage is at best
   wasted requests and at worst -- on a weak connection -- tiles that hang
   until [Hd_dem]'s timeout instead of resolving instantly. *)
let use_cache_first ?(serve_not_found = false) ?query_opts request =
  let* response =
    Brr_io.Fetch.Cache.Storage.match' ?query_opts (Brr_io.Fetch.caches ())
      request
  in
  match response with
  | Some response
    when Brr_io.Fetch.Response.ok response
         || (serve_not_found && Brr_io.Fetch.Response.status response = 404) ->
      Fut.return (Ok response)
  | _ ->
      let* response = Brr_io.Fetch.request request in
      let* () = put_in_cache request response in
      Fut.return (Ok response)

(* [preloadResponse] only exists where navigation preload is implemented
   (Chromium); elsewhere [Fut.of_promise] would raise on the undefined
   property, aborting the whole fetch handler. *)
let preload_response event =
  if
    Jv.is_none
      (Jv.get
         (Brr.Ev.to_jv (Brr_io.Fetch.Ev.as_extendable event))
         "preloadResponse")
  then Fut.return (Ok None)
  else Brr_io.Fetch.Ev.preload_response event

let use_cache_on_error event request =
  let open Fut.Syntax in
  let* response = preload_response event in
  match response with
  | Ok (Some response) when Brr_io.Fetch.Response.ok response ->
      let open Fut.Result_syntax in
      let* () = put_in_cache request response in
      Fut.return (Ok response)
  | Ok _ | Error _ -> (
      let* response = Brr_io.Fetch.request request in
      match response with
      | Ok response when Brr_io.Fetch.Response.ok response ->
          let open Fut.Result_syntax in
          let* () = put_in_cache request response in
          Fut.return (Ok response)
      | Ok _ | Error _ -> (
          let open Fut.Result_syntax in
          let* response =
            Brr_io.Fetch.Cache.Storage.match' ~query_opts:match_opts
              (Brr_io.Fetch.caches ()) request
          in
          match response with
          | Some response -> Fut.return (Ok response)
          | None -> Fut.return (Ok (Brr_io.Fetch.Response.error ()))))

let () =
  ignore
    (Brr.Ev.listen Brr_io.Fetch.Ev.fetch
       (fun ev ->
         let ev = Brr.Ev.as_type ev in
         let request = Brr_io.Fetch.Ev.request ev in
         let url = Brr_io.Fetch.Request.url request in
         Brr_io.Fetch.Ev.respond_with ev
           (let open Fut.Syntax in
            let* response =
              let url = Jstr.to_string url in
              if is_hd_dem url then
                use_cache_first ~serve_not_found:true request
              else if
                is_tile url
                || String.ends_with ~suffix:"worker.bc.js" url
                || String.ends_with ~suffix:"decompress_tile.wasm" url
              then use_cache_first ~query_opts:match_opts request
              else use_cache_on_error ev request
            in
            match response with
            | Ok _ -> Fut.return response
            | Error _ -> Fut.return (Ok (Brr_io.Fetch.Response.error ()))))
       Brr.G.target)

(* Navigation preload is unimplemented in Firefox, where
   [registration.navigationPreload] is undefined: enabling it must not throw
   before [Clients.claim] runs. *)
let enable_navigation_preload () =
  let open Brr_webworkers.Service_worker in
  let preload = Registration.navigation_preload G.registration in
  if Jv.is_none (Navigation_preload_manager.to_jv preload) then
    Fut.return (Ok ())
  else Navigation_preload_manager.enable preload

let () =
  ignore
    (Brr.Ev.listen Brr.Ev.activate
       (fun ev ->
         Brr.Ev.Extendable.wait_until (Brr.Ev.as_type ev)
           (let open Fut.Result_syntax in
            let+ () = enable_navigation_preload ()
            and+ () = Brr_webworkers.Service_worker.(Clients.claim G.clients)
            and+ () = delete_old_caches ()
            and+ () = prune_data_cache () in
            ()))
       Brr.G.target)

let () =
  ignore
    (Brr.Ev.listen Brr.Ev.install
       (fun ev ->
         Brr.Ev.Extendable.wait_until (Brr.Ev.as_type ev)
           (let open Fut.Result_syntax in
            let* cache = open_cache app_cache_name in
            Brr_io.Fetch.Cache.add_all cache
              (List.map
                 (fun url -> Brr_io.Fetch.Request.v (Jstr.v url))
                 ([
                    ".";
                    "viewer.bc.wasm.js";
                    "viewer.bc.js";
                    "worker.bc.js";
                    "decompress_tile.wasm";
                    "decode_clc.wasm";
                    "assets/details_bc7.ktx2";
                  ]
                 (* Loaded before the service worker claims the page, so the
                    fetch handler never gets to cache it on a first visit. *)
                 @ Sw_version.wasm_assets))))
       Brr.G.target)

(*
- Use cache first for the DEM (.dem, .tif) and land cover (.clc) tiles, and for
  the worker and Wasm modules
- Use cache on error for other files

https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API/Using_Service_Workers
https://googlechrome.github.io/samples/service-worker/custom-offline-page/

self.addEventListener("fetch", (event) => {
  event.respondWith(caches.match(event.request));
});

****************

const addResourcesToCache = async (resources) => {
  const cache = await caches.open("v1");
  await cache.addAll(resources);
};

self.addEventListener("install", (event) => {
  event.waitUntil(
    addResourcesToCache([
      "/",
      "/index.html",
      "/style.css",
      "/app.js",
      "/image-list.js",
      "/star-wars-logo.jpg",
      "/gallery/bountyHunters.jpg",
      "/gallery/myLittleVader.jpg",
      "/gallery/snowTroopers.jpg",
    ]),
  );
});

**********************

Add to cache from the main page:
document.querySelector( '.fetch-content' ).addEventListener( 'click', () => {
  window.caches.open( myCache )
    .then( cache => cache.addAll( content ) )
    .then( () => alert( 'content is now available offline' ) )
    .catch( () => alert( 'oh noes! something went wrong' ) );
});
*)
