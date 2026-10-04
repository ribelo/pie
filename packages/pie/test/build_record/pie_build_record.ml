open Build_record_lib

let usage ~(caps : < Ox_caps.Cap.exit ; Ox_caps.Cap.stderr ; .. >) () =
  Ox_caps.Cap_stdlib.prerr_endline ~caps
    "pie_build_record --root DIR --build-dir DIR --output PATH [--target PATH]";
  Ox_caps.Cap_stdlib.exit ~caps 2

let need ~(caps : < Ox_caps.Cap.exit ; Ox_caps.Cap.stderr ; .. >) i =
  if i + 1 >= Array.length Sys.argv then usage ~caps () else Sys.argv.(i + 1)

let rec arg ~(caps : < Ox_caps.Cap.exit ; Ox_caps.Cap.stderr ; .. >) i root
    build_dir output target =
  if i >= Array.length Sys.argv then (root, build_dir, output, target)
  else
    match Sys.argv.(i) with
    | "--root" ->
        arg ~caps (i + 2) (Some (need ~caps i)) build_dir output target
    | "--build-dir" ->
        arg ~caps (i + 2) root (Some (need ~caps i)) output target
    | "--output" ->
        arg ~caps (i + 2) root build_dir (Some (need ~caps i)) target
    | "--target" -> arg ~caps (i + 2) root build_dir output (need ~caps i)
    | _ -> usage ~caps ()

let main
    ~(caps :
       < Ox_caps.Cap.chdir
       ; Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.exit
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.tmp
       ; .. >) =
  let root, build_dir, output, target =
    arg ~caps 1 None None None "packages/pie/bin/pie.exe"
  in
  match (root, build_dir, output) with
  | Some root, Some build_dir, Some output ->
      ignore (Provenance.write_record ~caps ~root ~build_dir ~target ~output ())
  | _ -> usage ~caps ()

let () = main ~caps:(Ox_test.powerbox ())
