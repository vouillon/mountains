open Fut.Result_syntax

(* The application shell goes to a cache named after a digest of its contents:
   a deploy changing any of them uses a fresh cache, so nothing can be served
   from a previous build. Every file of the shell is served from that cache
   first, the network only answering what it does not hold: on the bad
   connections this app is used on, revalidating a few megabytes of code is the
   slowest part of a start, and it buys nothing a deploy does not already give
   -- a new build arrives as a new worker, which the page offers as an update
   (see [watchForUpdate] in index.html).

   The tiles are immutable and expensive to fetch, so they live in a separate,
   persistent cache which survives deploys. Its name is also hard-coded in the
   main thread ([Dem_loader.prefetch], [Clc_loader.prefetch_tile],
   [Reader.prefetch]) and must be kept in sync. *)
let app_cache_name = Jstr.v ("mountains-" ^ Sw_version.version)
let data_cache_name = Jstr.v "v1"

(* Files handed over by the Web Share Target below. Neither shell nor tiles,
   hence a third cache; entries live only until the page has read them. *)
let share_cache_name = Jstr.v "shared-gpx"

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

let contains_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec from i = i + m <= n && (String.sub s i m = sub || from (i + 1)) in
  m = 0 || from 0

(* The map picker's basemap and its relief shading come from the same host as
   the elevation tiles, but must not be treated like them. Panning walks an
   unbounded set of URLs, one per tile at every level visited, and the data cache
   has no eviction: cached like the rest they would grow it without end and
   crowd out the elevation and land cover tiles that have to survive offline.

   Passing them straight through does not mean they are fetched twice. The
   service answers these with [cache-control: private, max-age=1814400], three
   weeks, so the browser's own HTTP cache holds them: measured at 6.5 MB over the
   network on a first view of a screen of tiles and zero bytes for the same view
   after a reload. What is given up is only the Cache Storage copy, and with it
   the map alone offline -- picking a location is an online act anyway, whereas
   the terrain the picked location loads still has to work without a network. *)
let is_map_basemap url =
  is_hd_dem url
  && (contains_sub url "LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2"
     || contains_sub url
          "LAYER=IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE.SHADOW"
     || contains_sub url "LAYER=ELEVATION.CONTOUR.LINE")

(* The shell is what lives under the worker's own scope, tiles aside. Anything
   else -- the elevation service above, and whatever else may be added -- is
   left to the handlers that know it, rather than falling into the shell's
   cache-first path, where a query-insensitive lookup would answer one request
   with another's response. *)
let scope =
  Jstr.to_string
    Brr_webworkers.Service_worker.(Registration.scope G.registration)

let is_shell url = String.starts_with ~prefix:scope url

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
           if
             Jstr.equal name app_cache_name
             || Jstr.equal name data_cache_name
             || Jstr.equal name share_cache_name
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

let match_opts = Brr_io.Fetch.Cache.query_opts ~ignore_search:true ()

(* Shell lookups are pinned to this build's cache instead of searching every
   one of them. A new worker fills its cache while installing, under a page this
   one is still serving: a storage-wide match could answer that page with a file
   from a build it never loaded, mixing the two. Pinned, each worker serves its
   own build whole, until the page reloads into the new one. *)
let shell_match_opts =
  Brr_io.Fetch.Cache.query_opts ~ignore_search:true ~cache_name:app_cache_name
    ()

(* [put_in_cache] stores error responses too. With [serve_not_found], a cached
   404 is served rather than skipped: a WMTS elevation tile answers 404 when it
   lies outside French territory, which is permanent, and asking the network
   again on every visit to a location at the edge of coverage is at best
   wasted requests and at worst -- on a weak connection -- tiles that hang
   until [Hd_dem]'s timeout instead of resolving instantly. *)
let use_cache_first ?(serve_not_found = false) ?query_opts request =
  (* A failed *lookup* is a miss, not an error. Binding it with the error-
     propagating [let*] meant any hiccup in the Cache API short-circuited to
     [Error], which the fetch handler turns into [Response.error ()] -- a hard
     network failure for the page, without ever having tried the network.

     Reachable rather than theoretical: with the disk near full, [Cache.put]
     starts answering "Unexpected internal error", and a lookup can fail the same
     way. Losing the cache should cost latency, never the response. *)
  let response =
    let open Fut.Syntax in
    let+ found =
      Brr_io.Fetch.Cache.Storage.match' ?query_opts (Brr_io.Fetch.caches ())
        request
    in
    match found with Ok r -> r | Error _ -> None
  in
  let open Fut.Syntax in
  let* response = response in
  match response with
  | Some response
    when Brr_io.Fetch.Response.ok response
         || (serve_not_found && Brr_io.Fetch.Response.status response = 404) ->
      Fut.return (Ok response)
  | _ ->
      let open Fut.Result_syntax in
      let* response = Brr_io.Fetch.request request in
      let* () = put_in_cache request response in
      Fut.return (Ok response)

(* Web Share Target ([share_target] in manifest.json): a share arrives as a
   [multipart/form-data] POST navigation to [share-target], and no document can
   read the body of a navigation. Only the worker can, so it answers the POST
   itself -- moving the files into [share_cache_name] and redirecting to the
   application, which drains that cache on startup (see [drainSharedFiles] in
   index.html). Without this handler the POST goes to the network, where
   nothing serves it, which is what made sharing fail. *)
(* Not [Brr_io.Fetch.Request.method']: up to brr 0.0.8 it reads a property
   named "method'", stray apostrophe included, and so is always undefined. *)
let request_method request =
  Jv.Jstr.get (Brr_io.Fetch.Request.to_jv request) "method"

let is_share_target request =
  Jstr.equal (request_method request) (Jstr.v "POST")
  &&
  match Brr.Uri.of_jstr (Brr_io.Fetch.Request.url request) with
  | Ok uri ->
      String.ends_with ~suffix:"/share-target"
        (Jstr.to_string (Brr.Uri.path uri))
  | Error _ -> false

(* A cache entry is a request/response pair, so the URL is the only place left
   to keep the file name. Entries are numbered from those already in the cache:
   a share the page has not read yet -- its navigation was cancelled, say --
   must not be overwritten by the next one. *)
let share_entry_request ~base ~index file =
  let name =
    match Brr.Uri.encode_component (Brr.File.name file) with
    | Ok name -> name
    | Error _ -> Jstr.empty
  in
  let path = Jstr.(v "shared-gpx/" + of_int index + v "?name=" + name) in
  match Brr.Uri.of_jstr ~base path with
  | Ok uri -> Some (Brr_io.Fetch.Request.v (Brr.Uri.to_jstr uri))
  | Error _ -> None

(* Every file entry is taken, whatever its field name: the manifest asks for
   [gpx_file], but that is a request to the browser, not a guarantee. *)
let stash_shared_files request =
  let base = Brr_io.Fetch.Request.url request in
  let* form =
    Brr_io.Fetch.Body.form_data (Brr_io.Fetch.Request.as_body request)
  in
  let files =
    List.filter_map
      (fun (_, value) ->
        match value with `File file -> Some file | `String _ -> None)
      (Brr_io.Form.Data.to_assoc form)
  in
  match files with
  | [] -> Fut.return (Ok 0)
  | files ->
      let* cache = open_cache share_cache_name in
      let* pending = Brr_io.Fetch.Cache.keys cache in
      let first = List.length pending in
      let open Fut.Syntax in
      let+ _ =
        Fut.of_list
          (List.mapi
             (fun i file ->
               match share_entry_request ~base ~index:(first + i) file with
               | None -> Fut.return (Ok ())
               | Some entry ->
                   let blob = Brr.Blob.of_jv (Brr.File.to_jv file) in
                   Brr_io.Fetch.Cache.put cache entry
                     (Brr_io.Fetch.Response.v
                        ~body:(Brr_io.Fetch.Body.of_blob blob)
                        ()))
             files)
      in
      Ok (List.length files)

(* The user must end up in the application whatever happens, never on an error
   page: a share that yielded no file just opens it. 303 so that the browser
   follows up with a GET, which the cache-on-error path then serves. *)
let handle_share_target request =
  let base = Brr_io.Fetch.Request.url request in
  let redirect target =
    match Brr.Uri.of_jstr ~base (Jstr.v target) with
    | Ok uri ->
        Ok (Brr_io.Fetch.Response.redirect ~status:303 (Brr.Uri.to_jstr uri))
    | Error _ -> Ok (Brr_io.Fetch.Response.error ())
  in
  let open Fut.Syntax in
  let+ stashed = stash_shared_files request in
  match stashed with
  | Ok count when count > 0 -> redirect "./?share_received=1"
  | Ok _ -> redirect "./"
  | Error e ->
      Brr.Console.(warn [ Jstr.v "Share target failed:"; Jv.Error.message e ]);
      redirect "./"

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
              if is_share_target request then handle_share_target request
              else
                let url = Jstr.to_string url in
                if is_map_basemap url then Brr_io.Fetch.request request
                else if is_hd_dem url then
                  use_cache_first ~serve_not_found:true request
                else if is_tile url then
                  use_cache_first ~query_opts:match_opts request
                else if is_shell url then
                  use_cache_first ~query_opts:shell_match_opts request
                else Brr_io.Fetch.request request
            in
            match response with
            | Ok _ -> Fut.return response
            | Error _ -> Fut.return (Ok (Brr_io.Fetch.Response.error ()))))
       Brr.G.target)

(* A new worker otherwise waits for every window of the app to close, which for
   something people leave open can be days: the page offers the update instead
   and asks for this when it is accepted (see [watchForUpdate] in index.html).
   Not done on install, unprompted: activating swaps the worker under a running
   page, and [delete_old_caches] then takes away the shell it is still using, so
   it must happen only when the page is about to reload into the new one. *)
let () =
  ignore
    (Brr.Ev.listen Brr_io.Message.Ev.message
       (fun ev ->
         let data = Brr_io.Message.Ev.data (Brr.Ev.as_type ev) in
         if Jstr.equal (Jv.to_jstr data) (Jstr.v "skipWaiting") then
           ignore Brr_webworkers.Service_worker.G.(skip_waiting ()))
       Brr.G.target)

(* Navigation preload sends the document to the network in parallel with the
   fetch handler, which was the fast path while documents were served
   network-first. Served from the cache instead, that response is dropped
   unread, so it is one request's worth of a scarce connection spent on nothing.
   Turning it off has to be done, not merely left undone: the setting belongs to
   the registration and outlives the worker that asked for it, so installs
   carrying it from an earlier build would keep preloading for ever.

   It is unimplemented in Firefox, where [registration.navigationPreload] is
   undefined: this must not throw before [Clients.claim] runs. *)
let disable_navigation_preload () =
  let open Brr_webworkers.Service_worker in
  let preload = Registration.navigation_preload G.registration in
  if Jv.is_none (Navigation_preload_manager.to_jv preload) then
    Fut.return (Ok ())
  else Navigation_preload_manager.disable preload

let () =
  ignore
    (Brr.Ev.listen Brr.Ev.activate
       (fun ev ->
         Brr.Ev.Extendable.wait_until (Brr.Ev.as_type ev)
           (let open Fut.Result_syntax in
            let+ () = disable_navigation_preload ()
            and+ () = Brr_webworkers.Service_worker.(Clients.claim G.clients)
            and+ () = delete_old_caches ()
            and+ () = prune_data_cache () in
            ()))
       Brr.G.target)

(* Precaching goes to the network, not through the browser's own HTTP cache,
   which is how a new worker can otherwise fill its cache with the build it is
   replacing. A document served with any freshness at all -- GitHub Pages sends
   [max-age=600] for HTML -- is reused from there rather than fetched, and
   [Cache.add_all] does nothing to prevent it: measured here, a worker installed
   seconds after a deploy precached the *previous* index.html into the cache
   named after the new build. Network-first that lasted until the next
   revalidation; cache-first it is what the build would serve for as long as it
   lives, an update the user accepted and did not get. [Cache.reload] is the
   fetch that ignores the HTTP cache in both directions. *)
let precache_request url =
  let init =
    Brr_io.Fetch.Request.init ~cache:Brr_io.Fetch.Request.Cache.reload ()
  in
  Brr_io.Fetch.Request.v ~init (Jstr.v url)

let () =
  ignore
    (Brr.Ev.listen Brr.Ev.install
       (fun ev ->
         Brr.Ev.Extendable.wait_until (Brr.Ev.as_type ev)
           (let open Fut.Result_syntax in
            let* cache = open_cache app_cache_name in
            Brr_io.Fetch.Cache.add_all cache
              (List.map precache_request
                 ([
                    ".";
                    "viewer.bc.wasm.js";
                    "viewer.bc.js";
                    "worker.bc.js";
                    "decompress_tile.wasm";
                    "decode_clc.wasm";
                    "blend.wasm";
                    "assets/details_bc7.ktx2";
                  ]
                 (* Loaded before the service worker claims the page, so the
                    fetch handler never gets to cache it on a first visit. *)
                 @ Sw_version.wasm_assets))))
       Brr.G.target)

(*
- Use cache first for the DEM (.dem, .tif) and land cover (.clc) tiles, and for
  the application shell
- Pass anything else through to the network

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
