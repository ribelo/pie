(* Global glossary resolution: dict-gnkq dict-60is dict-zwjf dict-5lsq. *)
let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

module Glossary = Pie.Glossary

let env_keys = [ "HOME"; "XDG_CONFIG_HOME"; "PIE_GLOSSARY"; "PATH"; "LC_ALL" ]
let save_env () = List.map (fun key -> (key, Sys.getenv_opt key)) env_keys
let putenv = Unix.putenv [@alert "-unsafe_multidomain"]

let restore_env saved =
  List.iter
    (fun (key, value) ->
      match value with Some value -> putenv key value | None -> putenv key "")
    saved

let with_env bindings f =
  let saved = save_env () in
  Fun.protect
    ~finally:(fun () -> restore_env saved)
    (fun () ->
      List.iter (fun (key, value) -> putenv key value) bindings;
      f ())

let rec chmod_tree path =
  (try Unix.chmod path 0o700 with Unix.Unix_error _ -> ());
  match Unix.lstat path with
  | exception Unix.Unix_error _ -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      match Sys.readdir path with
      | exception Sys_error _ -> ()
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                chmod_tree (Filename.concat path name))
            names)
  | _ -> ()

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Sys.readdir path with
      | exception Sys_error _ -> ()
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf (Filename.concat path name))
            names);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let temp_dir name =
  let path = Filename.temp_file name "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Unix.realpath path

let with_sandbox f =
  let root = temp_dir "pie-glossary" in
  Fun.protect
    ~finally:(fun () ->
      chmod_tree root;
      rm_rf root)
    (fun () -> f root)

let rec mkdir_p path =
  if path = "" || path = "/" || path = "." then ()
  else (
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let with_global f =
  with_sandbox (fun root ->
      let path = Filename.concat root "config/pie/glossary.md" in
      with_env
        [
          ("HOME", root);
          ("XDG_CONFIG_HOME", Filename.concat root "config");
          ("PIE_GLOSSARY", "");
        ]
        (fun () -> f root path))

let check_read path content =
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

let check_warning path =
  let result = Glossary.resolve ~caps () in
  Alcotest.(check string) "no prompt" "" result.text;
  Alcotest.(check (list string)) "no paths" [] result.paths;
  match result.warnings with
  | [ Glossary.Unreadable unreadable ] ->
      Alcotest.(check string) "warned path" path unreadable.path;
      Alcotest.(check bool) "reason" true (unreadable.message <> "")
  | _ -> Alcotest.fail "Expected one unreadable warning"

let test_xdg_and_home () =
  with_global (fun root path ->
      write_file path "XDG — ex dee gee\n";
      check_read path "XDG — ex dee gee\n";
      let fallback = Filename.concat root ".config/pie/glossary.md" in
      write_file fallback "HOME — hom\n";
      with_env
        [ ("XDG_CONFIG_HOME", "") ]
        (fun () -> check_read fallback "HOME — hom\n"))

let test_override () =
  with_global (fun root path ->
      write_file path "DEFAULT\n";
      let override = Filename.concat root "override.md" in
      write_file override "OVERRIDE\n";
      with_env
        [ ("PIE_GLOSSARY", override) ]
        (fun () -> check_read override "OVERRIDE\n");
      let cwd = Sys.getcwd () in
      Fun.protect
        ~finally:(fun () -> Sys.chdir cwd)
        (fun () ->
          Sys.chdir root;
          with_env
            [ ("PIE_GLOSSARY", "override.md") ]
            (fun () -> check_read override "OVERRIDE\n")))

let test_no_project () =
  with_global (fun root path ->
      write_file path "GLOBAL\n";
      let project = Filename.concat root "project" in
      write_file (Filename.concat project ".pie/glossary.md") "PROJECT\n";
      let cwd = Sys.getcwd () in
      Fun.protect
        ~finally:(fun () -> Sys.chdir cwd)
        (fun () ->
          Sys.chdir project;
          check_read path "GLOBAL\n"))

let test_reload_and_symlink () =
  with_global (fun root path ->
      let target = Filename.concat root "target.md" in
      write_file target "FIRST\n";
      mkdir_p (Filename.dirname path);
      Unix.symlink target path;
      check_read path "FIRST\n";
      write_file target "SECOND\n";
      check_read path "SECOND\n";
      let next = Filename.concat root "next.md" in
      write_file next "RETARGETED\n";
      Unix.unlink path;
      Unix.symlink next path;
      check_read path "RETARGETED\n")

let test_no_cap () =
  with_global (fun _ path ->
      let content = String.make 1_200_000 'x' ^ "\nTAIL — tail\n" in
      write_file path content;
      check_read path content)

let test_missing () =
  with_global (fun _ path ->
      check_warning path;
      let status = Glossary.status ~caps () in
      Alcotest.(check (option bool)) "missing" (Some false) status.exists;
      Alcotest.(check (option int)) "zero lines" (Some 0) status.line_count)

let test_invalid_files () =
  with_global (fun _ path ->
      mkdir_p path;
      check_warning path;
      Unix.rmdir path;
      Unix.mkfifo path 0o600;
      check_warning path;
      Unix.unlink path;
      Unix.symlink "missing-target" path;
      check_warning path)

let test_permissions () =
  with_global (fun _ path ->
      write_file path "SECRET\n";
      Unix.chmod path 0;
      if Unix.getuid () = 0 then () else check_warning path)

let test_status_lines () =
  with_global (fun _ path ->
      List.iter
        (fun (content, lines) ->
          write_file path content;
          let status = Glossary.status ~caps () in
          Alcotest.(check string) "path" path status.path;
          Alcotest.(check (option bool)) "exists" (Some true) status.exists;
          Alcotest.(check (option int)) "lines" (Some lines) status.line_count;
          check_read path content)
        [ ("", 0); ("one", 1); ("one\n", 1); ("one\n\nthree", 3) ])

let () =
  Alcotest.run "Glossary"
    [
      ( "global",
        [
          Alcotest.test_case "XDG and HOME" `Quick test_xdg_and_home;
          Alcotest.test_case "override replaces default" `Quick test_override;
          Alcotest.test_case "project files ignored" `Quick test_no_project;
          Alcotest.test_case "reload and symlinks" `Quick
            test_reload_and_symlink;
          Alcotest.test_case "no size cap" `Quick test_no_cap;
          Alcotest.test_case "missing warns" `Quick test_missing;
          Alcotest.test_case "non-files warn without blocking" `Quick
            test_invalid_files;
          Alcotest.test_case "permissions warn" `Quick test_permissions;
          Alcotest.test_case "physical line count" `Quick test_status_lines;
        ] );
    ]
