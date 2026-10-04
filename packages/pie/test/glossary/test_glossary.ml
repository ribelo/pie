(* Global glossary resolution: dict-gnkq dict-60is dict-zwjf dict-5lsq. *)
(* ^pcaps-n1rg *)

module Glossary = Pie.Glossary

let env_keys = [ "HOME"; "XDG_CONFIG_HOME"; "PIE_GLOSSARY"; "PATH"; "LC_ALL" ]

let save_env ~(caps : < Ox_caps.Cap.env ; .. >) () =
  List.map (fun key -> (key, Ox_caps.Cap_sys.getenv_opt ~caps key)) env_keys

let putenv ~(caps : < Ox_caps.Cap.env ; .. >) =
  Ox_caps.Cap_unix.putenv ~caps [@alert "-unsafe_multidomain"]

let restore_env ~(caps : < Ox_caps.Cap.env ; .. >) saved =
  List.iter
    (fun (key, value) ->
      match value with
      | Some value -> putenv ~caps key value
      | None -> putenv ~caps key "")
    saved

let with_env ~(caps : < Ox_caps.Cap.env ; .. >) bindings f =
  let saved = save_env ~caps () in
  Fun.protect
    ~finally:(fun () -> restore_env ~caps saved)
    (fun () ->
      List.iter (fun (key, value) -> putenv ~caps key value) bindings;
      f ())

let rec chmod_tree ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >)
    path =
  (try Ox_caps.Cap_unix.chmod ~caps path 0o700 with Unix.Unix_error _ -> ());
  match Unix.lstat path with
  | exception Unix.Unix_error _ -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      match Ox_caps.Cap_sys.readdir ~caps path with
      | exception Sys_error _ -> ()
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                chmod_tree ~caps (Filename.concat path name))
            names)
  | _ -> ()

let rec rm_rf ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >) path
    =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Ox_caps.Cap_sys.readdir ~caps path with
      | exception Sys_error _ -> ()
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf ~caps (Filename.concat path name))
            names);
      try Ox_caps.Cap_unix.rmdir ~caps path with Unix.Unix_error _ -> ())
  | _ -> (
      try Ox_caps.Cap_unix.unlink ~caps path with Unix.Unix_error _ -> ())

let temp_dir ~(caps : < Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) name =
  let path = Ox_caps.Cap_stdlib.Filename.temp_file ~caps name "" in
  Ox_caps.Cap_sys.remove ~caps path;
  Ox_caps.Cap_unix.mkdir ~caps path 0o700;
  Unix.realpath path

let with_sandbox
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let root = temp_dir ~caps "pie-glossary" in
  Fun.protect
    ~finally:(fun () ->
      chmod_tree ~caps root;
      rm_rf ~caps root)
    (fun () -> f root)

let rec mkdir_p ~(caps : < Ox_caps.Cap.fs_write ; .. >) path =
  if path = "" || path = "/" || path = "." then ()
  else (
    mkdir_p ~caps (Filename.dirname path);
    try Ox_caps.Cap_unix.mkdir ~caps path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_file ~(caps : < Ox_caps.Cap.fs_write ; .. >) path contents =
  mkdir_p ~caps (Filename.dirname path);
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let with_global
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) f =
  with_sandbox ~caps (fun root ->
      let path = Filename.concat root "config/pie/glossary.md" in
      with_env ~caps
        [
          ("HOME", root);
          ("XDG_CONFIG_HOME", Filename.concat root "config");
          ("PIE_GLOSSARY", "");
        ]
        (fun () -> f root path))

let check_read
    ~(caps :
       < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >)
    path content =
  let result = Glossary.resolve ~caps () in
  Alcotest.(check (list string)) "one path" [ path ] result.paths;
  Alcotest.(check int) "no warnings" 0 (List.length result.warnings);
  let newline =
    if content = "" || String.ends_with ~suffix:"\n" content then "" else "\n"
  in
  Alcotest.(check string)
    "full content"
    ("# " ^ path ^ "\n\n" ^ content ^ newline)
    result.text

let check_warning
    ~(caps :
       < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >)
    path =
  let result = Glossary.resolve ~caps () in
  Alcotest.(check string) "no prompt" "" result.text;
  Alcotest.(check (list string)) "no paths" [] result.paths;
  match result.warnings with
  | [ Glossary.Unreadable unreadable ] ->
      Alcotest.(check string) "warned path" path unreadable.path;
      Alcotest.(check bool) "reason" true (unreadable.message <> "")
  | _ -> Alcotest.fail "Expected one unreadable warning"

let test_xdg_and_home
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun root path ->
      write_file ~caps path "XDG — ex dee gee\n";
      check_read ~caps path "XDG — ex dee gee\n";
      let fallback = Filename.concat root ".config/pie/glossary.md" in
      write_file ~caps fallback "HOME — hom\n";
      with_env ~caps
        [ ("XDG_CONFIG_HOME", "") ]
        (fun () -> check_read ~caps fallback "HOME — hom\n"))

let test_override
    ~(caps :
       < Ox_caps.Cap.chdir
       ; Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun root path ->
      write_file ~caps path "DEFAULT\n";
      let override = Filename.concat root "override.md" in
      write_file ~caps override "OVERRIDE\n";
      with_env ~caps
        [ ("PIE_GLOSSARY", override) ]
        (fun () -> check_read ~caps override "OVERRIDE\n");
      let cwd = Sys.getcwd () in
      Fun.protect
        ~finally:(fun () -> Ox_caps.Cap_sys.chdir ~caps cwd)
        (fun () ->
          Ox_caps.Cap_sys.chdir ~caps root;
          with_env ~caps
            [ ("PIE_GLOSSARY", "override.md") ]
            (fun () -> check_read ~caps override "OVERRIDE\n")))

let test_no_project
    ~(caps :
       < Ox_caps.Cap.chdir
       ; Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun root path ->
      write_file ~caps path "GLOBAL\n";
      let project = Filename.concat root "project" in
      write_file ~caps (Filename.concat project ".pie/glossary.md") "PROJECT\n";
      let cwd = Sys.getcwd () in
      Fun.protect
        ~finally:(fun () -> Ox_caps.Cap_sys.chdir ~caps cwd)
        (fun () ->
          Ox_caps.Cap_sys.chdir ~caps project;
          check_read ~caps path "GLOBAL\n"))

let test_reload_and_symlink
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun root path ->
      let target = Filename.concat root "target.md" in
      write_file ~caps target "FIRST\n";
      mkdir_p ~caps (Filename.dirname path);
      Ox_caps.Cap_unix.symlink ~caps target path;
      check_read ~caps path "FIRST\n";
      write_file ~caps target "SECOND\n";
      check_read ~caps path "SECOND\n";
      let next = Filename.concat root "next.md" in
      write_file ~caps next "RETARGETED\n";
      Ox_caps.Cap_unix.unlink ~caps path;
      Ox_caps.Cap_unix.symlink ~caps next path;
      check_read ~caps path "RETARGETED\n")

let test_no_cap
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun _ path ->
      let content = String.make 1_200_000 'x' ^ "\nTAIL — tail\n" in
      write_file ~caps path content;
      check_read ~caps path content)

let test_missing
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun _ path ->
      check_warning ~caps path;
      let status = Glossary.status ~caps () in
      Alcotest.(check (option bool)) "missing" (Some false) status.exists;
      Alcotest.(check (option int)) "zero lines" (Some 0) status.line_count)

let test_invalid_files
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun _ path ->
      mkdir_p ~caps path;
      check_warning ~caps path;
      Ox_caps.Cap_unix.rmdir ~caps path;
      Unix.mkfifo path 0o600;
      check_warning ~caps path;
      Ox_caps.Cap_unix.unlink ~caps path;
      Ox_caps.Cap_unix.symlink ~caps "missing-target" path;
      check_warning ~caps path)

let test_permissions
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun _ path ->
      write_file ~caps path "SECRET\n";
      Ox_caps.Cap_unix.chmod ~caps path 0;
      if Unix.getuid () = 0 then () else check_warning ~caps path)

let test_status_lines
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_global ~caps (fun _ path ->
      List.iter
        (fun (content, lines) ->
          write_file ~caps path content;
          let status = Glossary.status ~caps () in
          Alcotest.(check string) "path" path status.path;
          Alcotest.(check (option bool)) "exists" (Some true) status.exists;
          Alcotest.(check (option int)) "lines" (Some lines) status.line_count;
          check_read ~caps path content)
        [ ("", 0); ("one", 1); ("one\n", 1); ("one\n\nthree", 3) ])

let main
    ~(caps :
       < Ox_caps.Cap.chdir
       ; Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "Glossary"
    [
      ( "global",
        [
          Alcotest.test_case "XDG and HOME" `Quick (test_xdg_and_home ~caps);
          Alcotest.test_case "override replaces default" `Quick
            (test_override ~caps);
          Alcotest.test_case "project files ignored" `Quick
            (test_no_project ~caps);
          Alcotest.test_case "reload and symlinks" `Quick
            (test_reload_and_symlink ~caps);
          Alcotest.test_case "no size cap" `Quick (test_no_cap ~caps);
          Alcotest.test_case "missing warns" `Quick (test_missing ~caps);
          Alcotest.test_case "non-files warn without blocking" `Quick
            (test_invalid_files ~caps);
          Alcotest.test_case "permissions warn" `Quick (test_permissions ~caps);
          Alcotest.test_case "physical line count" `Quick
            (test_status_lines ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
