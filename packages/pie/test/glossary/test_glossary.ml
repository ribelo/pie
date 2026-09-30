(* Glossary resolution against real directories and child processes.
   dict-gnkq dict-cods dict-c5j3 dict-cwri dict-eiod dict-60is dict-zwjf
   dict-5lsq *)

let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

module Glossary = Pie.Glossary

let quote = Filename.quote
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

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let contains haystack needle =
  let n = String.length needle in
  let h = String.length haystack in
  let rec loop i =
    i <= h - n && (String.sub haystack i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

let global_path config =
  Filename.concat (Filename.concat config "pie") "glossary.md"

let project_path dir =
  Filename.concat (Filename.concat (Unix.realpath dir) ".pie") "glossary.md"

let proc_cwd pid =
  try Some (Unix.readlink (Printf.sprintf "/proc/%d/cwd" pid))
  with Unix.Unix_error _ -> None

let rec wait_cwd pid expected attempts =
  let want = Unix.realpath expected in
  match proc_cwd pid with
  | Some cwd when cwd = want || cwd = expected -> ()
  | _ when attempts <= 0 ->
      Alcotest.failf "pid %d cwd never became %s (got %s)" pid expected
        (Option.value (proc_cwd pid) ~default:"<gone>")
  | _ ->
      Unix.sleepf 0.02;
      wait_cwd pid expected (attempts - 1)

let kill pid = try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()
let reap pid = try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ()

let ppid_of pid =
  try
    let ic = open_in_bin (Printf.sprintf "/proc/%d/stat" pid) in
    let line =
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> input_line ic)
    in
    match String.rindex_opt line ')' with
    | None -> None
    | Some close_paren -> (
        let rest =
          String.trim
            (String.sub line (close_paren + 1)
               (String.length line - close_paren - 1))
        in
        match String.split_on_char ' ' rest with
        | _state :: ppid :: _ -> int_of_string_opt ppid
        | _ -> None)
  with End_of_file | Sys_error _ | Unix.Unix_error _ -> None

let children_of parent =
  match Sys.readdir "/proc" with
  | exception Sys_error _ -> []
  | names ->
      Array.fold_left
        (fun acc name ->
          match int_of_string_opt name with
          | None -> acc
          | Some pid -> (
              match ppid_of pid with
              | Some ppid when ppid = parent -> pid :: acc
              | _ -> acc))
        [] names

let descendants pid =
  let visited = Hashtbl.create 8 in
  let rec walk pid acc =
    if Hashtbl.mem visited pid then acc
    else (
      Hashtbl.add visited pid ();
      List.fold_left
        (fun acc child -> walk child (child :: acc))
        acc (children_of pid))
  in
  walk pid []

let kill_tree pid =
  List.iter kill (descendants pid);
  kill pid;
  reap pid

let spawn_script path =
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close null)
    (fun () -> Unix.create_process "/bin/sh" [| "sh"; path |] null null null)

let rec read_ints path attempts =
  if Sys.file_exists path then
    let ids =
      read_file path |> String.trim |> String.split_on_char ' '
      |> List.filter (fun item -> item <> "")
      |> List.map int_of_string
    in
    if ids <> [] || attempts <= 0 then ids
    else (
      Unix.sleepf 0.02;
      read_ints path (attempts - 1))
  else if attempts <= 0 then Alcotest.failf "timed out waiting for %s" path
  else (
    Unix.sleepf 0.02;
    read_ints path (attempts - 1))

let one_child_script ~parent_cwd ~child_cwd ~pid_file =
  String.concat "\n"
    [
      "#!/bin/sh";
      "set -eu";
      "cd " ^ quote parent_cwd;
      "/bin/sh -c "
      ^ quote ("cd " ^ quote child_cwd ^ " && exec sleep 30")
      ^ " &";
      "echo $! > " ^ quote pid_file;
      "wait";
      "";
    ]

let two_child_script ~parent_cwd ~old_cwd ~new_cwd ~pid_file =
  String.concat "\n"
    [
      "#!/bin/sh";
      "set -eu";
      "cd " ^ quote parent_cwd;
      "/bin/sh -c " ^ quote ("cd " ^ quote old_cwd ^ " && exec sleep 30") ^ " &";
      "old=$!";
      "sleep 0.2";
      "/bin/sh -c " ^ quote ("cd " ^ quote new_cwd ^ " && exec sleep 30") ^ " &";
      "new=$!";
      "printf '%s %s\\n' \"$old\" \"$new\" > " ^ quote pid_file;
      "wait";
      "";
    ]

let with_one ~sandbox ~parent_cwd ~child_cwd f =
  let pid_file = Filename.concat sandbox "child.pid" in
  let script_path = Filename.concat sandbox "parent.sh" in
  write_file script_path (one_child_script ~parent_cwd ~child_cwd ~pid_file);
  Unix.chmod script_path 0o700;
  let parent = spawn_script script_path in
  Fun.protect
    ~finally:(fun () -> kill_tree parent)
    (fun () ->
      match read_ints pid_file 150 with
      | [ child ] ->
          wait_cwd child child_cwd 150;
          f ~parent ~child
      | other ->
          Alcotest.failf "expected one child pid, got %d" (List.length other))

let with_two ~sandbox ~parent_cwd ~old_cwd ~new_cwd f =
  let pid_file = Filename.concat sandbox "children.pid" in
  let script_path = Filename.concat sandbox "parent.sh" in
  write_file script_path
    (two_child_script ~parent_cwd ~old_cwd ~new_cwd ~pid_file);
  Unix.chmod script_path 0o700;
  let parent = spawn_script script_path in
  Fun.protect
    ~finally:(fun () -> kill_tree parent)
    (fun () ->
      match read_ints pid_file 150 with
      | [ old_pid; new_pid ] ->
          wait_cwd old_pid old_cwd 150;
          wait_cwd new_pid new_cwd 150;
          f ~parent ~old_pid ~new_pid
      | other ->
          Alcotest.failf "expected two child pids, got %d" (List.length other))

let layout root =
  let home = Filename.concat root "home" in
  let config = Filename.concat root "config" in
  mkdir_p home;
  mkdir_p (Filename.concat config "pie");
  (home, config)

let bind ~home ~config ?(glossary = "") () =
  [
    ("HOME", home);
    ("XDG_CONFIG_HOME", config);
    ("PIE_GLOSSARY", glossary);
    ("LC_ALL", "C");
  ]

let unreadable = function
  | Glossary.Unreadable warning -> Some warning
  | Glossary.Focus_unavailable _ -> None

let test_nested_project () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let project = Filename.concat home "project" in
      let nested = Filename.concat project "src/deep" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p nested;
      mkdir_p parent_cwd;
      write_file (global_path config) "GLOBAL-WORDS\n";
      write_file (project_path project) "PROJECT-WORDS\n";
      write_file (project_path home) "HOME-WORDS\n";
      write_file (project_path parent_cwd) "PARENT-WORDS\n";
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
            (fun ~parent ~child:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check (option string))
                "cwd"
                (Some (Unix.realpath nested))
                resolution.cwd;
              Alcotest.(check (list string))
                "paths"
                [ global_path config; project_path project ]
                resolution.paths;
              Alcotest.(check bool)
                "project body" true
                (contains resolution.text "PROJECT-WORDS");
              Alcotest.(check bool)
                "not home" false
                (contains resolution.text "HOME-WORDS");
              Alcotest.(check bool)
                "not parent" false
                (contains resolution.text "PARENT-WORDS");
              Alcotest.(check int)
                "no warnings" 0
                (List.length resolution.warnings))))

let test_recent_leaf () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let old_dir = Filename.concat home "old-project" in
      let new_dir = Filename.concat home "new-project" in
      let parent_cwd = Filename.concat home "terminal" in
      List.iter mkdir_p [ old_dir; new_dir; parent_cwd ];
      write_file (project_path old_dir) "OLD-LEAF\n";
      write_file (project_path new_dir) "NEW-LEAF\n";
      write_file (project_path parent_cwd) "PARENT-CWD\n";
      with_env (bind ~home ~config ()) (fun () ->
          with_two ~sandbox:root ~parent_cwd ~old_cwd:old_dir ~new_cwd:new_dir
            (fun ~parent ~old_pid:_ ~new_pid:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check (option string))
                "cwd"
                (Some (Unix.realpath new_dir))
                resolution.cwd;
              Alcotest.(check bool)
                "newest leaf" true
                (contains resolution.text "NEW-LEAF");
              Alcotest.(check bool)
                "not older leaf" false
                (contains resolution.text "OLD-LEAF");
              Alcotest.(check bool)
                "not parent cwd" false
                (contains resolution.text "PARENT-CWD"))))

let test_no_project () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let child_cwd = Filename.concat home "browser" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p child_cwd;
      mkdir_p parent_cwd;
      write_file (global_path config) "GLOBAL-ONLY\n";
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd (fun ~parent ~child:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check (option string))
                "cwd"
                (Some (Unix.realpath child_cwd))
                resolution.cwd;
              Alcotest.(check (list string))
                "paths"
                [ global_path config ]
                resolution.paths;
              Alcotest.(check string)
                "text"
                ("# " ^ global_path config ^ "\n\nGLOBAL-ONLY\n")
                resolution.text;
              Alcotest.(check int)
                "no warnings" 0
                (List.length resolution.warnings))))

let test_pie_glossary_override () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let override = Filename.concat root "override.md" in
      write_file (global_path config) "CONFIG-DEFAULT\n";
      write_file override "OVERRIDE-WORDS\n";
      with_env (bind ~home ~config ~glossary:override ()) (fun () ->
          let resolution = Glossary.resolve ~caps ~pid:None in
          Alcotest.(check (list string)) "paths" [ override ] resolution.paths;
          Alcotest.(check bool)
            "override" true
            (contains resolution.text "OVERRIDE-WORDS");
          Alcotest.(check bool)
            "not config default" false
            (contains resolution.text "CONFIG-DEFAULT");
          Alcotest.(check (option string)) "no cwd" None resolution.cwd))

let test_symlink_full_read () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let project = Filename.concat home "project" in
      let nested = Filename.concat project "nested" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p nested;
      mkdir_p parent_cwd;
      let trailer = "END-OF-GLOSSARY\n" in
      let body = String.make 70000 'a' ^ trailer in
      let target = Filename.concat root "outside.md" in
      let global_target = Filename.concat root "global-target.md" in
      write_file target body;
      write_file global_target "GLOBAL-LINK\n";
      mkdir_p (Filename.dirname (project_path project));
      Unix.symlink target (project_path project);
      Unix.symlink global_target (global_path config);
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
            (fun ~parent ~child:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check (list string))
                "symlink paths, not targets"
                [ global_path config; project_path project ]
                resolution.paths;
              Alcotest.(check bool)
                "followed global" true
                (contains resolution.text "GLOBAL-LINK");
              Alcotest.(check bool)
                "read past one chunk" true
                (contains resolution.text trailer);
              Alcotest.(check bool)
                "full body" true
                (String.length resolution.text > 70000))))

let test_unreadable_directory () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let project = Filename.concat home "project" in
      let nested = Filename.concat project "nested" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p nested;
      mkdir_p parent_cwd;
      write_file (global_path config) "GLOBAL-STILL\n";
      write_file (project_path home) "SHOULD-NOT-FALL-THROUGH\n";
      mkdir_p (Filename.dirname (project_path project));
      Unix.mkdir (project_path project) 0o755;
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
            (fun ~parent ~child:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check bool)
                "global kept" true
                (contains resolution.text "GLOBAL-STILL");
              Alcotest.(check bool)
                "did not walk past unreadable" false
                (contains resolution.text "SHOULD-NOT-FALL-THROUGH");
              match List.filter_map unreadable resolution.warnings with
              | [ { path; message } ] ->
                  Alcotest.(check string) "path" (project_path project) path;
                  Alcotest.(check string) "message" "is a directory" message
              | other ->
                  Alcotest.failf "expected one unreadable warning, got %d"
                    (List.length other))))

let test_unreadable_permission () =
  if Unix.geteuid () = 0 then ()
  else
    with_sandbox (fun root ->
        let home, config = layout root in
        let project = Filename.concat home "project" in
        let nested = Filename.concat project "nested" in
        let parent_cwd = Filename.concat home "terminal" in
        mkdir_p nested;
        mkdir_p parent_cwd;
        write_file (global_path config) "SECRET-GLOBAL\n";
        Unix.chmod (global_path config) 0o000;
        write_file (project_path project) "PROJECT-KEPT\n";
        with_env (bind ~home ~config ()) (fun () ->
            with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
              (fun ~parent ~child:_ ->
                let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
                Alcotest.(check bool)
                  "project kept" true
                  (contains resolution.text "PROJECT-KEPT");
                Alcotest.(check bool)
                  "global omitted" false
                  (contains resolution.text "SECRET-GLOBAL");
                match List.filter_map unreadable resolution.warnings with
                | [ { path; message } ] ->
                    Alcotest.(check string) "path" (global_path config) path;
                    Alcotest.(check bool)
                      "reason" true
                      (String.length message > 0)
                | other ->
                    Alcotest.failf "expected one warning, got %d"
                      (List.length other))))

let test_fifo_not_opened () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let project = Filename.concat home "project" in
      let nested = Filename.concat project "nested" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p nested;
      mkdir_p parent_cwd;
      write_file (global_path config) "GLOBAL-FIFO\n";
      mkdir_p (Filename.dirname (project_path project));
      Unix.mkfifo (project_path project) 0o600;
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
            (fun ~parent ~child:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check bool)
                "returned" true
                (contains resolution.text "GLOBAL-FIFO");
              match List.filter_map unreadable resolution.warnings with
              | [ { path; message } ] ->
                  Alcotest.(check string) "path" (project_path project) path;
                  Alcotest.(check string) "message" "is a fifo" message
              | other ->
                  Alcotest.failf "expected fifo warning, got %d"
                    (List.length other))))

let test_missing_global_not_warning () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let project = Filename.concat home "project" in
      let nested = Filename.concat project "nested" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p nested;
      mkdir_p parent_cwd;
      write_file (project_path project) "PROJECT-ONLY\n";
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
            (fun ~parent ~child:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check (list string))
                "paths"
                [ project_path project ]
                resolution.paths;
              Alcotest.(check int)
                "no warnings" 0
                (List.length resolution.warnings);
              Alcotest.(check bool)
                "project" true
                (contains resolution.text "PROJECT-ONLY"))))

let test_missing_both () =
  with_sandbox (fun root ->
      let home, config = layout root in
      with_env (bind ~home ~config ()) (fun () ->
          let resolution = Glossary.resolve ~caps ~pid:None in
          Alcotest.(check string) "empty" "" resolution.text;
          Alcotest.(check (list string)) "no paths" [] resolution.paths;
          Alcotest.(check int) "no warnings" 0 (List.length resolution.warnings);
          Alcotest.(check (option string)) "no cwd" None resolution.cwd))

let test_edit_visible () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let project = Filename.concat home "project" in
      let nested = Filename.concat project "nested" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p nested;
      mkdir_p parent_cwd;
      write_file (global_path config) "GLOBAL-V1\n";
      write_file (project_path project) "PROJECT-V1\n";
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
            (fun ~parent ~child:_ ->
              let first = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check bool)
                "v1" true
                (contains first.text "GLOBAL-V1"
                && contains first.text "PROJECT-V1");
              write_file (global_path config) "GLOBAL-V2\n";
              write_file (project_path project) "PROJECT-V2\n";
              let second = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check bool)
                "v2 global" true
                (contains second.text "GLOBAL-V2");
              Alcotest.(check bool)
                "v2 project" true
                (contains second.text "PROJECT-V2");
              Alcotest.(check bool)
                "v1 gone" false
                (contains second.text "GLOBAL-V1"
                || contains second.text "PROJECT-V1"))))

let test_render_order () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let project = Filename.concat home "project" in
      let nested = Filename.concat project "nested" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p nested;
      mkdir_p parent_cwd;
      write_file (global_path config) "global words";
      write_file (project_path project) "project words\n";
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:nested
            (fun ~parent ~child:_ ->
              let resolution = Glossary.resolve ~caps ~pid:(Some parent) in
              let global = global_path config in
              let project = project_path project in
              let expected =
                "# " ^ global ^ "\n\nglobal words\n\n# " ^ project
                ^ "\n\nproject words\n"
              in
              Alcotest.(check string) "rendered" expected resolution.text;
              Alcotest.(check (list string))
                "order" [ global; project ] resolution.paths)))

let test_home_boundary () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let notes = Filename.concat home "notes" in
      let parent_cwd = Filename.concat home "terminal" in
      mkdir_p notes;
      mkdir_p parent_cwd;
      write_file (global_path config) "GLOBAL-BOUND\n";
      write_file (project_path home) "HOME-LEVEL\n";
      write_file (project_path root) "ABOVE-HOME\n";
      with_env (bind ~home ~config ()) (fun () ->
          with_one ~sandbox:root ~parent_cwd ~child_cwd:notes
            (fun ~parent ~child:_ ->
              let included = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check bool)
                "home included" true
                (contains included.text "HOME-LEVEL");
              Alcotest.(check bool)
                "not above home" false
                (contains included.text "ABOVE-HOME");
              Sys.remove (project_path home);
              let stopped = Glossary.resolve ~caps ~pid:(Some parent) in
              Alcotest.(check bool)
                "stopped at home" false
                (contains stopped.text "ABOVE-HOME");
              Alcotest.(check (list string))
                "global only"
                [ global_path config ]
                stopped.paths)))

let test_focus_unavailable () =
  with_sandbox (fun root ->
      let home, config = layout root in
      let bin = Filename.concat root "bin" in
      mkdir_p bin;
      write_file (global_path config) "GLOBAL-FOCUS\n";
      write_file (Filename.concat bin "niri") "#!/bin/sh\nexit 1\n";
      Unix.chmod (Filename.concat bin "niri") 0o700;
      with_env
        (bind ~home ~config () @ [ ("PATH", bin) ])
        (fun () ->
          let resolution =
            Pie_test_runtime.run (fun env ->
                Glossary.resolve_focused ~caps
                  ~process_mgr:(Eio.Stdenv.process_mgr env)
                  ())
          in
          Alcotest.(check bool)
            "global still rendered" true
            (contains resolution.text "GLOBAL-FOCUS");
          Alcotest.(check (option string)) "no cwd" None resolution.cwd;
          match resolution.warnings with
          | [ Glossary.Focus_unavailable _ ] -> ()
          | other ->
              Alcotest.failf "expected focus warning, got %d"
                (List.length other)))

let () =
  Alcotest.run "glossary"
    [
      ( "resolution",
        List.map
          (fun (name, test) -> Alcotest.test_case name `Quick test)
          [
            ( "project glossary from nested directory dict-cods dict-c5j3",
              test_nested_project );
            ("most recent leaf cwd wins dict-c5j3", test_recent_leaf);
            ("no project uses global only dict-cwri dict-gnkq", test_no_project);
            ( "PIE_GLOSSARY overrides config glossary dict-gnkq",
              test_pie_glossary_override );
            ( "symlinked glossary is read in full dict-zwjf",
              test_symlink_full_read );
            ( "unreadable glossary warns and continues dict-5lsq",
              test_unreadable_directory );
            ( "unreadable permission warns and continues dict-5lsq",
              test_unreadable_permission );
            ("fifo glossary is not opened dict-5lsq", test_fifo_not_opened);
            ( "missing global glossary is not a warning dict-5lsq",
              test_missing_global_not_warning );
            ("missing glossaries are not warnings dict-5lsq", test_missing_both);
            ( "glossary edit is visible on the next call dict-60is",
              test_edit_visible );
            ( "rendered glossary labels sources in order dict-eiod",
              test_render_order );
            ("walk includes HOME and stops there dict-cods", test_home_boundary);
            ( "niri failure does not block the global glossary dict-5lsq \
               dict-c5j3",
              test_focus_unavailable );
          ] );
    ]
