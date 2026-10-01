open Build_record_lib

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let rec mkdir_p path =
  try Unix.mkdir path 0o700 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p (Filename.dirname path);
      Unix.mkdir path 0o700

let plant_tree sandbox =
  let pkg = Filename.concat sandbox "packages/pie" in
  mkdir_p pkg;
  write_file (Filename.concat sandbox "dune-project") "(lang dune 3.21)\n";
  write_file (Filename.concat pkg "dummy.ml") "let () = ()\n";
  write_file (Filename.concat pkg "dune") "(library (name pie))\n"

let plant_ox_lib sandbox name cmxa_body a_body =
  let dir = Filename.concat (Filename.concat sandbox "ox") name in
  mkdir_p dir;
  let cmxa = Filename.concat dir (name ^ ".cmxa") in
  let archive = Filename.concat dir (name ^ ".a") in
  write_file cmxa cmxa_body;
  write_file archive a_body;
  (cmxa, archive)

let test_parse_selects_pie_exe () =
  let log =
    String.concat "\n"
      [
        "/usr/bin/ocamlopt.opt /tmp/stage/ox/ox.cmxa -o other.exe";
        "/usr/bin/ocamlopt.opt /tmp/stage/ox/ox.cmxa          \
         /tmp/stage/ox_http/ox_http.cmxa -o          \
         /tmp/build/default/packages/pie/bin/pie.exe";
      ]
  in
  let expected = "/tmp/build/default/packages/pie/bin/pie.exe" in
  let paths =
    Provenance.parse_pie_link_cmxa ~expected_output:expected ~root:"/tmp"
      ~build_dir:"/tmp/build" log
  in
  Alcotest.(check (list string))
    "only pie.exe cmxa"
    [ "/tmp/stage/ox/ox.cmxa"; "/tmp/stage/ox_http/ox_http.cmxa" ]
    paths

let test_reject_backslash_quoting () =
  match Provenance.tokenize_command {|ocamlopt foo\bar.cmxa -o pie.exe|} with
  | exception Failure msg ->
      Alcotest.(check bool) "reject" true (Provenance.contains "backslash" msg)
  | _ -> Alcotest.fail "expected quoting failure"

let test_fingerprint_includes_dune_project () =
  let sandbox = Filename.temp_file "pie-fp" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let a = Provenance.source_fingerprint sandbox in
  write_file (Filename.concat sandbox "dune-project") "(lang dune 3.22)\n";
  let b = Provenance.source_fingerprint sandbox in
  Alcotest.(check bool) "dune-project in fingerprint" true (a <> b);
  let script = Filename.concat sandbox "scripts/build-identity.sh" in
  mkdir_p (Filename.dirname script);
  write_file script "first generator\n";
  let c = Provenance.source_fingerprint sandbox in
  write_file script "changed generator\n";
  let d = Provenance.source_fingerprint sandbox in
  Alcotest.(check bool) "generator in fingerprint" true (c <> d)

let test_mutate_during_build_refuses () =
  let sandbox = Filename.temp_file "pie-mut" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let cmxa, _archive = plant_ox_lib sandbox "ox" "FAKE-CMXA\n" "FAKE-A\n" in
  Unix.putenv "OCAMLPATH" (Filename.concat sandbox "ox");
  let build_dir = Filename.concat sandbox "build" in
  mkdir_p (Filename.concat build_dir "default/packages/pie/bin");
  let output = Filename.concat sandbox "out.json" in
  match
    Provenance.write_record ~root:sandbox ~build_dir
      ~target:"packages/pie/bin/pie.exe" ~output
      ~run_build:(fun () ->
        write_file
          (Filename.concat sandbox "packages/pie/dummy.ml")
          "let () = print_endline \"mutated\"\n";
        write_file
          (Filename.concat build_dir "default/packages/pie/bin/pie.exe")
          "BIN\n";
        write_file
          (Filename.concat build_dir "log")
          (Printf.sprintf
             "/usr/bin/ocamlopt.opt %s -o %s/default/packages/pie/bin/pie.exe\n"
             cmxa build_dir))
      ()
  with
  | exception Failure msg ->
      Alcotest.(check bool)
        "mentions source change" true
        (Provenance.contains "source bytes changed" msg);
      Alcotest.(check bool) "no new record" false (Sys.file_exists output)
  | _ -> Alcotest.fail "expected refusal"

let test_record_from_planted_build () =
  let sandbox = Filename.temp_file "pie-ok" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let cmxa, archive = plant_ox_lib sandbox "ox" "FAKE-CMXA\n" "FAKE-A\n" in
  let stubs = Filename.concat sandbox "stublibs" in
  mkdir_p stubs;
  write_file (Filename.concat stubs "dllox_http_stubs.so") "FAKE-STUB\n";
  Unix.putenv "OCAMLPATH" (Filename.concat sandbox "ox");
  Unix.putenv "CAML_LD_LIBRARY_PATH" stubs;
  let build_dir = Filename.concat sandbox "build" in
  let bin_dir = Filename.concat build_dir "default/packages/pie/bin" in
  mkdir_p bin_dir;
  let binary = Filename.concat bin_dir "pie.exe" in
  let output = Filename.concat sandbox "pie.provenance.json" in
  write_file (Filename.concat sandbox "old.json") "OLD-RECORD\n";
  let log_line =
    Printf.sprintf "/usr/bin/ocamlopt.opt %s -o %s\n" cmxa binary
  in
  ignore
    (Provenance.write_record ~root:sandbox ~build_dir
       ~target:"packages/pie/bin/pie.exe" ~output
       ~run_build:(fun () ->
         write_file binary "FAKE-BIN\n";
         write_file
           (Filename.concat build_dir "default/packages/pie/build_identity.ml")
           {|let commit = "abc123-dirty"
let time = "2026-10-01T10:00:00Z"
|};
         write_file (Filename.concat build_dir "log") log_line)
       ());
  let ic = open_in_bin output in
  let json =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let ox_hash = Provenance.sha256_file cmxa in
  let companion_hash = Provenance.sha256_file archive in
  let bin_hash = Provenance.sha256_file binary in
  Alcotest.(check bool) "ox hash" true (Provenance.contains ox_hash json);
  Alcotest.(check bool)
    "companion .a hash" true
    (Provenance.contains companion_hash json);
  Alcotest.(check bool)
    "companion key" true
    (Provenance.contains "linked_companion_archives" json);
  Alcotest.(check bool) "binary hash" true (Provenance.contains bin_hash json);
  Alcotest.(check bool)
    "embedded commit in artifact record" true
    (Provenance.contains "abc123-dirty" json);
  Alcotest.(check bool)
    "embedded build time in artifact record" true
    (Provenance.contains "2026-10-01T10:00:00Z" json);
  Alcotest.(check bool)
    "runtime stublibs key" true
    (Provenance.contains "runtime_stublibs" json);
  Alcotest.(check bool)
    "old record file survives" true
    (Sys.file_exists (Filename.concat sandbox "old.json"))

let test_companion_a_change_refuses () =
  let sandbox = Filename.temp_file "pie-a" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let cmxa, archive = plant_ox_lib sandbox "ox" "FAKE-CMXA\n" "FAKE-A\n" in
  Unix.putenv "OCAMLPATH" (Filename.concat sandbox "ox");
  let build_dir = Filename.concat sandbox "build" in
  mkdir_p (Filename.concat build_dir "default/packages/pie/bin");
  let output = Filename.concat sandbox "out.json" in
  match
    Provenance.write_record ~root:sandbox ~build_dir
      ~target:"packages/pie/bin/pie.exe" ~output
      ~run_build:(fun () ->
        write_file archive "MUTATED-A\n";
        write_file
          (Filename.concat build_dir "default/packages/pie/bin/pie.exe")
          "BIN\n";
        write_file
          (Filename.concat build_dir "log")
          (Printf.sprintf
             "/usr/bin/ocamlopt.opt %s -o %s/default/packages/pie/bin/pie.exe\n"
             cmxa build_dir))
      ()
  with
  | exception Failure msg ->
      Alcotest.(check bool)
        "mentions archive change" true
        (Provenance.contains "changed during build" msg);
      Alcotest.(check bool) "no new record" false (Sys.file_exists output)
  | _ -> Alcotest.fail "expected refusal"

let test_tls_static_archive_change_refuses () =
  let sandbox = Filename.temp_file "pie-tls-a" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let cmxa, _ = plant_ox_lib sandbox "ox" "FAKE-CMXA\n" "FAKE-A\n" in
  let tls_cmxa, _ = plant_ox_lib sandbox "ox_http" "TLS-CMXA\n" "TLS-A\n" in
  let tls_dir = Filename.concat (Filename.concat sandbox "ox") "ox_http" in
  let stubs_a = Filename.concat tls_dir "libox_http_stubs.a" in
  write_file stubs_a "TLS-STUBS-A\n";
  let stubs = Filename.concat sandbox "stublibs" in
  mkdir_p stubs;
  write_file (Filename.concat stubs "dllox_http_stubs.so") "FAKE-STUB-SO\n";
  Unix.putenv "OCAMLPATH" (Filename.concat sandbox "ox");
  Unix.putenv "CAML_LD_LIBRARY_PATH" stubs;
  let build_dir = Filename.concat sandbox "build" in
  mkdir_p (Filename.concat build_dir "default/packages/pie/bin");
  let output = Filename.concat sandbox "out.json" in
  match
    Provenance.write_record ~root:sandbox ~build_dir
      ~target:"packages/pie/bin/pie.exe" ~output
      ~run_build:(fun () ->
        write_file stubs_a "MUTATED-STUBS-A\n";
        write_file
          (Filename.concat build_dir "default/packages/pie/bin/pie.exe")
          "BIN\n";
        write_file
          (Filename.concat build_dir "log")
          (Printf.sprintf
             "/usr/bin/ocamlopt.opt %s %s -o %s/default/packages/pie/bin/pie.exe\n"
             cmxa tls_cmxa build_dir))
      ()
  with
  | exception Failure msg ->
      Alcotest.(check bool)
        "mentions archive change" true
        (Provenance.contains "changed during build" msg);
      Alcotest.(check bool) "no new record" false (Sys.file_exists output)
  | _ -> Alcotest.fail "expected refusal"

let test_two_visible_tls_roots_records_linked_winner () =
  let sandbox = Filename.temp_file "pie-2root" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let r1 = Filename.concat sandbox "r1" in
  let r2 = Filename.concat sandbox "r2" in
  let ox_cmxa, _ = plant_ox_lib r1 "ox" "OX-CMXA\n" "OX-A\n" in
  (* plant_ox_lib nests under <sandbox>/ox/<name>; use r1/r2 as those roots. *)
  let tls1_cmxa, _ = plant_ox_lib r1 "ox_http" "TLS1-CMXA\n" "TLS1-A\n" in
  let tls1_dir = Filename.concat (Filename.concat r1 "ox") "ox_http" in
  let stubs1 = Filename.concat tls1_dir "libox_http_stubs.a" in
  write_file stubs1 "STUBS-ROOT1\n";
  let tls2_cmxa, _ = plant_ox_lib r2 "ox_http" "TLS2-CMXA\n" "TLS2-A\n" in
  let tls2_dir = Filename.concat (Filename.concat r2 "ox") "ox_http" in
  let stubs2 = Filename.concat tls2_dir "libox_http_stubs.a" in
  write_file stubs2 "STUBS-ROOT2\n";
  let _ = tls2_cmxa in
  Unix.putenv "OCAMLPATH"
    (Filename.concat r1 "ox" ^ ":" ^ Filename.concat r2 "ox");
  Unix.putenv "CAML_LD_LIBRARY_PATH" "";
  let build_dir = Filename.concat sandbox "build" in
  mkdir_p (Filename.concat build_dir "default/packages/pie/bin");
  let binary = Filename.concat build_dir "default/packages/pie/bin/pie.exe" in
  let output = Filename.concat sandbox "out.json" in
  ignore
    (Provenance.write_record ~root:sandbox ~build_dir
       ~target:"packages/pie/bin/pie.exe" ~output
       ~run_build:(fun () ->
         write_file binary "BIN\n";
         write_file
           (Filename.concat build_dir "log")
           (Printf.sprintf "/usr/bin/ocamlopt.opt %s %s -o %s\n" ox_cmxa
              tls1_cmxa binary))
       ());
  let ic = open_in_bin output in
  let json =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let h1 = Provenance.sha256_file stubs1 in
  let h2 = Provenance.sha256_file stubs2 in
  Alcotest.(check bool)
    "linked winner stub recorded" true
    (Provenance.contains h1 json);
  Alcotest.(check bool)
    "other root stub not selected" true
    (not (Provenance.contains h2 json))

let test_known_tls_missing_stub_refuses_even_if_other_root_has_it () =
  let sandbox = Filename.temp_file "pie-tls-miss" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let r1 = Filename.concat sandbox "r1" in
  let r2 = Filename.concat sandbox "r2" in
  let ox_cmxa, _ = plant_ox_lib r1 "ox" "OX-CMXA\n" "OX-A\n" in
  let tls1_cmxa, _ = plant_ox_lib r1 "ox_http" "TLS1-CMXA\n" "TLS1-A\n" in
  let _tls2_cmxa, _ = plant_ox_lib r2 "ox_http" "TLS2-CMXA\n" "TLS2-A\n" in
  let tls2_dir = Filename.concat (Filename.concat r2 "ox") "ox_http" in
  write_file
    (Filename.concat tls2_dir "libox_http_stubs.a")
    "OTHER-ROOT-STUBS\n";
  Unix.putenv "OCAMLPATH"
    (Filename.concat r1 "ox" ^ ":" ^ Filename.concat r2 "ox");
  let build_dir = Filename.concat sandbox "build" in
  mkdir_p (Filename.concat build_dir "default/packages/pie/bin");
  let output = Filename.concat sandbox "out.json" in
  match
    Provenance.write_record ~root:sandbox ~build_dir
      ~target:"packages/pie/bin/pie.exe" ~output
      ~run_build:(fun () ->
        write_file
          (Filename.concat build_dir "default/packages/pie/bin/pie.exe")
          "BIN\n";
        write_file
          (Filename.concat build_dir "log")
          (Printf.sprintf
             "/usr/bin/ocamlopt.opt %s %s -o %s/default/packages/pie/bin/pie.exe\n"
             ox_cmxa tls1_cmxa build_dir))
      ()
  with
  | exception Failure msg ->
      Alcotest.(check bool)
        "mentions missing TLS stub" true
        (Provenance.contains "missing TLS native C stub archive" msg);
      Alcotest.(check bool) "no new record" false (Sys.file_exists output)
  | _ -> Alcotest.fail "expected refusal"

let test_unrelated_stub_is_not_tls_identity () =
  let sandbox = Filename.temp_file "pie-unrel" "" in
  Sys.remove sandbox;
  mkdir_p sandbox;
  plant_tree sandbox;
  let cmxa, _ = plant_ox_lib sandbox "ox" "FAKE-CMXA\n" "FAKE-A\n" in
  let ox_dir = Filename.concat (Filename.concat sandbox "ox") "ox" in
  let unrelated = Filename.concat ox_dir "libox_turso_stubs.a" in
  write_file unrelated "NOT-TLS\n";
  Unix.putenv "OCAMLPATH" (Filename.concat sandbox "ox");
  Unix.putenv "CAML_LD_LIBRARY_PATH" "";
  let build_dir = Filename.concat sandbox "build" in
  mkdir_p (Filename.concat build_dir "default/packages/pie/bin");
  let binary = Filename.concat build_dir "default/packages/pie/bin/pie.exe" in
  let output = Filename.concat sandbox "out.json" in
  ignore
    (Provenance.write_record ~root:sandbox ~build_dir
       ~target:"packages/pie/bin/pie.exe" ~output
       ~run_build:(fun () ->
         write_file binary "BIN\n";
         write_file
           (Filename.concat build_dir "log")
           (Printf.sprintf "/usr/bin/ocamlopt.opt %s -o %s\n" cmxa binary))
       ());
  let ic = open_in_bin output in
  let json =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  Alcotest.(check bool)
    "turso stub is not TLS identity" true
    (not (Provenance.contains "libox_turso_stubs.a" json));
  Alcotest.(check bool)
    "tls identity is null" true
    (Provenance.contains "tls_native_stub_archive" json
    && Provenance.contains "null" json)

let () =
  Alcotest.run "build_record"
    [
      ( "provenance",
        [
          Alcotest.test_case "parse selects pie.exe link line" `Quick
            test_parse_selects_pie_exe;
          Alcotest.test_case "reject backslash quoting" `Quick
            test_reject_backslash_quoting;
          Alcotest.test_case "fingerprint includes dune-project" `Quick
            test_fingerprint_includes_dune_project;
          Alcotest.test_case "source mutation during build refuses record"
            `Quick test_mutate_during_build_refuses;
          Alcotest.test_case "record hashes link-line archive" `Quick
            test_record_from_planted_build;
          Alcotest.test_case "companion .a change refuses record" `Quick
            test_companion_a_change_refuses;
          Alcotest.test_case "TLS static archive change refuses record" `Quick
            test_tls_static_archive_change_refuses;
          Alcotest.test_case "two visible TLS roots records linked winner"
            `Quick test_two_visible_tls_roots_records_linked_winner;
          Alcotest.test_case "known TLS missing same-dir stub refuses" `Quick
            test_known_tls_missing_stub_refuses_even_if_other_root_has_it;
          Alcotest.test_case "unrelated stub is not TLS identity" `Quick
            test_unrelated_stub_is_not_tls_identity;
        ] );
    ]
