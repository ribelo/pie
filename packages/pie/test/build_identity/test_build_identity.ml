let script () =
  List.find Sys.file_exists
    [ "scripts/build-identity.sh"; "../../../../scripts/build-identity.sh" ]

let utc time =
  let tm = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let identity root =
  let channel =
    Unix.open_process_in
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

let rec remove path =
  if Sys.is_directory path then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let with_root f =
  let root = Filename.temp_file "pie-build-identity" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) (fun () -> f root)

let write_file path text =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let git root args =
  let command = Filename.quote_command "git" ([ "-C"; root ] @ args) in
  if Sys.command command <> 0 then Alcotest.fail "git fixture failed"

let test_unknown_and_actual_time () =
  with_root (fun root ->
      let before = utc (Unix.gettimeofday ()) in
      let commit, time = identity root in
      let after = utc (Unix.gettimeofday ()) in
      Alcotest.(check string) "no .git" "unknown" commit;
      Alcotest.(check bool)
        "actual UTC time, ignores SOURCE_DATE_EPOCH" true
        (String.compare before time <= 0 && String.compare time after <= 0);
      Alcotest.(check bool)
        "UTC timestamp" true
        (String.ends_with ~suffix:"Z" time))

let test_git_and_dirty () =
  with_root (fun root ->
      git root [ "init"; "--quiet" ];
      let source = Filename.concat root "source.txt" in
      write_file source "clean\n";
      git root [ "add"; "source.txt" ];
      git root
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
      let clean, _ = identity root in
      Alcotest.(check int) "short commit" 12 (String.length clean);
      write_file source "changed\n";
      let dirty, _ = identity root in
      Alcotest.(check string) "tracked change" (clean ^ "-dirty") dirty;
      write_file source "clean\n";
      write_file (Filename.concat root "untracked.txt") "new\n";
      let untracked, _ = identity root in
      Alcotest.(check string)
        "untracked source change" (clean ^ "-dirty") untracked)

let test_embedded () =
  Alcotest.(check bool)
    "embedded commit exists" true
    (Pie.Build_identity.commit <> "");
  Alcotest.(check bool)
    "embedded time is UTC" true
    (String.ends_with ~suffix:"Z" Pie.Build_identity.time)

let () =
  Alcotest.run "build-identity"
    [
      ( "dict-w1pe",
        [
          Alcotest.test_case "no git and actual execution time" `Quick
            test_unknown_and_actual_time;
          Alcotest.test_case "clean tracked-dirty and untracked-dirty" `Quick
            test_git_and_dirty;
          Alcotest.test_case "embedded module" `Quick test_embedded;
        ] );
    ]
