open Fut.Result_syntax

let get_cache () =
  Brr_io.Fetch.Cache.Storage.open' (Brr_io.Fetch.caches ()) (Jstr.v "v1")

let put_in_cache request response =
  let* cache = get_cache () in
  Brr_io.Fetch.Cache.put cache request
    (Brr_io.Fetch.Response.of_response response)

let use_cache_first request =
  let* response =
    Brr_io.Fetch.Cache.Storage.match' (Brr_io.Fetch.caches ()) request
  in
  match response with
  | Some response -> Fut.return (Ok response)
  | None ->
      let* response = Brr_io.Fetch.request request in
      let* () = put_in_cache request response in
      Fut.return (Ok response)

let use_cache_on_error event request =
  let open Fut.Syntax in
  let* response = Brr_io.Fetch.Ev.preload_response event in
  match response with
  | Ok (Some response) ->
      let open Fut.Result_syntax in
      let* () = put_in_cache request response in
      Fut.return (Ok response)
  | Ok None | Error _ -> (
      let* response = Brr_io.Fetch.request request in
      match response with
      | Ok response ->
          let open Fut.Result_syntax in
          let* () = put_in_cache request response in
          Fut.return (Ok response)
      | Error _ -> (
          let open Fut.Result_syntax in
          let* response =
            Brr_io.Fetch.Cache.Storage.match' (Brr_io.Fetch.caches ()) request
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
              if String.ends_with ~suffix:".tif" (Jstr.to_string url) then
                use_cache_first request
              else use_cache_on_error ev request
            in
            match response with
            | Ok _ -> Fut.return response
            | Error _ -> Fut.return (Ok (Brr_io.Fetch.Response.error ()))))
       Brr.G.target)

let () =
  ignore
    (Brr.Ev.listen Brr.Ev.activate
       (fun ev ->
         Brr.Ev.Extendable.wait_until (Brr.Ev.as_type ev)
           (let open Fut.Result_syntax in
            let+ () =
              Brr_webworkers.Service_worker.(
                Navigation_preload_manager.enable
                  (Registration.navigation_preload G.registration))
            and+ () = Brr_webworkers.Service_worker.(Clients.claim G.clients) in
            ()))
       Brr.G.target)

let () =
  ignore
    (Brr.Ev.listen Brr.Ev.install
       (fun ev ->
         Brr.Ev.Extendable.wait_until (Brr.Ev.as_type ev)
           (let open Fut.Result_syntax in
            let* cache = get_cache () in
            Brr_io.Fetch.Cache.add_all cache
              (List.map
                 (fun url -> Brr_io.Fetch.Request.v (Jstr.v url))
                 [ "."; "viewer.bc.wasm.js" (*; "viewer.bc.wasm"*) ])))
       Brr.G.target)

(*
- Use cache first for .tif and .geojson files
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
