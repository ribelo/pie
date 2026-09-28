open Build_record_lib

let usage () =
  prerr_endline
    "pie_build_record --root DIR --build-dir DIR --output PATH [--target PATH]";
  exit 2

let need i =
  if i + 1 >= Array.length Sys.argv then usage () else Sys.argv.(i + 1)

let rec arg i root build_dir output target =
  if i >= Array.length Sys.argv then (root, build_dir, output, target)
  else
    match Sys.argv.(i) with
    | "--root" -> arg (i + 2) (Some (need i)) build_dir output target
    | "--build-dir" -> arg (i + 2) root (Some (need i)) output target
    | "--output" -> arg (i + 2) root build_dir (Some (need i)) target
    | "--target" -> arg (i + 2) root build_dir output (need i)
    | _ -> usage ()

let () =
  let root, build_dir, output, target =
    arg 1 None None None "packages/pie/bin/pie.exe"
  in
  match (root, build_dir, output) with
  | Some root, Some build_dir, Some output ->
      ignore (Provenance.write_record ~root ~build_dir ~target ~output ())
  | _ -> usage ()
