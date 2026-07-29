(* Generates the [Sw_version] module embedded in the service worker.

   Usage: gen_sw_version <wasm assets dir> <file>...

   [version] is a digest of every asset the service worker precaches, so that
   any deploy changing one of them yields a different cache name (and a
   byte-different service_worker.bc.js, which is what triggers an update).
   [wasm_assets] are the content-hashed Wasm modules loaded by
   viewer.bc.wasm.js; their names are only known at build time. *)

let () =
  let assets_dir = Sys.argv.(1) in
  let files =
    Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
  in
  let assets =
    Sys.readdir assets_dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".wasm")
    |> List.sort String.compare
  in
  let digest =
    List.map Digest.file
      (List.sort String.compare files
      @ List.map (Filename.concat assets_dir) assets)
    |> String.concat "" |> Digest.string |> Digest.to_hex
  in
  Printf.printf "let version = %S\n" digest;
  Printf.printf "let wasm_assets = [ %s ]\n"
    (String.concat "; "
       (List.map
          (fun f ->
            Printf.sprintf "%S" (Filename.basename assets_dir ^ "/" ^ f))
          assets))
