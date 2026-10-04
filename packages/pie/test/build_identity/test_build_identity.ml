let script () =
  List.find Sys.file_exists
    [ "scripts/build-identity.sh"; "../../../../scripts/build-identity.sh" ]

let utc time =
  let tm = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let identity ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) root =
  let channel =
    Ox_caps.Cap_unix.open_process_in ~caps
      ("SOURCE_DATE_EPOCH=0 "
      ^ Filename.quote_command "bash" [ script (); root ])
  in
  let lines = ref [] in
  (try
     while true do
       lines := input_line channel :: !lines
     done
   with End_of_file -> ());
  (match Unix.close_process_in channel with
  | Unix.WEXITED 0 -> ()
  | _ -> Alcotest.fail "generator failed");
  let field name =
    List.find_map
      (fun line ->
        if String.starts_with ~prefix:("let " ^ name ^ " = ") line then
          Some
            (Scanf.sscanf line
               (Scanf.format_from_string
                  ("let " ^ name ^ " = %S")
                  "let commit = %S")
               Fun.id)
        else None)
      !lines
    |> Option.get
  in
  (field "commit", field "time")

let rec remove ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >)
    path =
  if Sys.is_directory path then (
    Array.iter
      (fun name -> remove ~caps (Filename.concat path name))
      (Ox_caps.Cap_sys.readdir ~caps path);
    Ox_caps.Cap_unix.rmdir ~caps path)
  else Ox_caps.Cap_sys.remove ~caps path

let with_root
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let root =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-build-identity" ""
  in
  Ox_caps.Cap_sys.remove ~caps root;
  Ox_caps.Cap_unix.mkdir ~caps root 0o700;
  Fun.protect ~finally:(fun () -> remove ~caps root) (fun () -> f root)

let write_file ~(caps : < Ox_caps.Cap.fs_write ; .. >) path text =
  let channel = Ox_caps.Cap_stdlib.open_out ~caps path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let git ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) root args =
  let command = Filename.quote_command "git" ([ "-C"; root ] @ args) in
  if Ox_caps.Cap_sys.command ~caps command <> 0 then
    Alcotest.fail "git fixture failed"

let test_unknown_and_actual_time
    ~(caps :
       < Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let before = utc (Ox_caps.Cap_unix.gettimeofday ~caps ()) in
      let commit, time = identity ~caps root in
      let after = utc (Ox_caps.Cap_unix.gettimeofday ~caps ()) in
      Alcotest.(check string) "no .git" "unknown" commit;
      Alcotest.(check bool)
        "actual UTC time, ignores SOURCE_DATE_EPOCH" true
        (String.compare before time <= 0 && String.compare time after <= 0);
      Alcotest.(check bool)
        "UTC timestamp" true
        (String.ends_with ~suffix:"Z" time))

let test_git_and_dirty
    ~(caps :
       < Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      git ~caps root [ "init"; "--quiet" ];
      let source = Filename.concat root "source.txt" in
      write_file ~caps source "clean\n";
      git ~caps root [ "add"; "source.txt" ];
      git ~caps root
        [
          "-c";
          "user.name=Pie test";
          "-c";
          "user.email=pie@test.invalid";
          "commit";
          "--quiet";
          "-m";
          "fixture";
        ];
      let clean, _ = identity ~caps root in
      Alcotest.(check int) "short commit" 12 (String.length clean);
      write_file ~caps source "changed\n";
      let dirty, _ = identity ~caps root in
      Alcotest.(check string) "tracked change" (clean ^ "-dirty") dirty;
      write_file ~caps source "clean\n";
      write_file ~caps (Filename.concat root "untracked.txt") "new\n";
      let untracked, _ = identity ~caps root in
      Alcotest.(check string)
        "untracked source change" (clean ^ "-dirty") untracked)

let test_embedded () =
  Alcotest.(check bool)
    "embedded commit exists" true
    (Pie.Build_identity.commit <> "");
  Alcotest.(check bool)
    "embedded time is UTC" true
    (String.ends_with ~suffix:"Z" Pie.Build_identity.time)

let main
    ~(caps :
       < Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "build-identity"
    [
      ( "dict-w1pe",
        [
          Alcotest.test_case "no git and actual execution time" `Quick
            (test_unknown_and_actual_time ~caps);
          Alcotest.test_case "clean tracked-dirty and untracked-dirty" `Quick
            (test_git_and_dirty ~caps);
          Alcotest.test_case "embedded module" `Quick test_embedded;
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
