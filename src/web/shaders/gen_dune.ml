let () =
  let files = Sys.readdir "." in
  let shader_files =
    Array.to_list files
    |> List.filter (fun f ->
        Filename.check_suffix f ".vert" || Filename.check_suffix f ".frag")
    |> List.sort String.compare
  in
  List.iter
    (fun f ->
      Printf.printf
        "(rule\n\
        \ (target %s.fmt)\n\
        \ (deps %s)\n\
        \ (action\n\
        \  (with-stdout-to %%{target}\n\
        \   (run clang-format %%{deps}))))\n\n\
         (rule\n\
        \ (alias fmt)\n\
        \ (action\n\
        \  (diff %s %s.fmt)))\n\n"
        f f f f)
    shader_files
