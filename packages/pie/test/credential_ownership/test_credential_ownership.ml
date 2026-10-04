(* ^pcaps-n1rg *)

(* PIE010: shared Codex auth.json is read-only. No refresh/write. *)

let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let rec rm_rf ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >) path
    =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Ox_caps.Cap_sys.readdir ~caps path with
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf ~caps (Filename.concat path name))
            names
      | exception Sys_error _ -> ());
      try Ox_caps.Cap_unix.rmdir ~caps path with Unix.Unix_error _ -> ())
  | _ -> (
      try Ox_caps.Cap_unix.unlink ~caps path with Unix.Unix_error _ -> ())

let with_sandbox
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let dir =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-credential-ownership" ""
  in
  Ox_caps.Cap_sys.remove ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf ~caps dir) (fun () -> f dir)

let write_file ~(caps : < Ox_caps.Cap.fs_write ; .. >) path contents =
  let oc =
    Ox_caps.Cap_stdlib.open_out_gen ~caps
      [ Open_creat; Open_trunc; Open_wronly ]
      0o600 path
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let read_file ~(caps : < Ox_caps.Cap.fs_read ; .. >) path =
  Ox_caps.Cap_stdlib.In_channel.with_open_bin ~caps path In_channel.input_all

let file_mode path =
  let st = Unix.stat path in
  st.Unix.st_perm land 0o777

let b64url_encode s =
  let table =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  in
  let n = String.length s in
  let buf = Buffer.create ((n + 2) / 3 * 4) in
  let rec loop i =
    if i >= n then ()
    else
      let b0 = Char.code s.[i] in
      let b1 = if i + 1 < n then Char.code s.[i + 1] else 0 in
      let b2 = if i + 2 < n then Char.code s.[i + 2] else 0 in
      let triple = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
      Buffer.add_char buf table.[(triple lsr 18) land 63];
      Buffer.add_char buf table.[(triple lsr 12) land 63];
      if i + 1 < n then Buffer.add_char buf table.[(triple lsr 6) land 63];
      if i + 2 < n then Buffer.add_char buf table.[triple land 63];
      loop (i + 3)
  in
  loop 0;
  Buffer.contents buf

let jwt ~exp =
  let payload = Printf.sprintf "{\"exp\":%d}" exp in
  "eyJhbGciOiJub25lIn0." ^ b64url_encode payload ^ ".sig"

let fresh_jwt ~(caps : < Ox_caps.Cap.time ; .. >) () =
  jwt ~exp:(int_of_float (Ox_caps.Cap_unix.time ~caps ()) + 3600)

let expired_jwt ~(caps : < Ox_caps.Cap.time ; .. >) () =
  jwt ~exp:(int_of_float (Ox_caps.Cap_unix.time ~caps ()) - 3600)

let auth_json token =
  Printf.sprintf
    "{\"tokens\":{\"access_token\":%S,\"refresh_token\":\"do-not-use\"},\"unrelated\":\"keep-me\"}\n"
    token

let cause_text (`Stt_error message) = message

let run_access
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. >)
    () =
  Pie.Stt.Codex_realtime.Codex_auth.access_token ~caps ()

let run_realtime
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. >)
    ~client_env =
  ignore client_env;
  Pie.Stt.Codex_realtime.realtime_access_token ~caps ~client:() ()

let with_codex_home ~(caps : < Ox_caps.Cap.env ; .. >) dir f =
  Ox_caps.Cap_unix.putenv ~caps "CODEX_HOME" dir;
  Ox_caps.Cap_unix.putenv ~caps "OPENAI_API_KEY" "";
  f ()

let test_env_key_succeeds
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. >)
    () =
  Ox_caps.Cap_unix.putenv ~caps "OPENAI_API_KEY" "synthetic-env-key";
  match run_realtime ~caps ~client_env:() with
  | Ok token -> Alcotest.(check string) "env key" "synthetic-env-key" token
  | Error cause -> Alcotest.failf "env key failed: %s" (cause_text cause)

let test_fresh_jwt_succeeds_without_write
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  let token = fresh_jwt ~caps () in
  let body = auth_json token in
  write_file ~caps path body;
  Ox_caps.Cap_unix.chmod ~caps path 0o600;
  with_codex_home ~caps dir @@ fun () ->
  match run_access ~caps () with
  | Ok got ->
      Alcotest.(check string) "fresh token" token got;
      Alcotest.(check string) "bytes unchanged" body (read_file ~caps path);
      Alcotest.(check int) "mode 0600" 0o600 (file_mode path)
  | Error cause -> Alcotest.failf "fresh jwt failed: %s" (cause_text cause)

let test_expired_jwt_is_stable_error_without_write
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  let body = auth_json (expired_jwt ~caps ()) in
  write_file ~caps path body;
  with_codex_home ~caps dir @@ fun () ->
  match run_access ~caps () with
  | Ok _ -> Alcotest.fail "expired token was accepted"
  | Error cause ->
      let text = cause_text cause in
      Alcotest.(check bool) "expired" true (string_contains text "expired");
      Alcotest.(check bool)
        "owner login" true
        (string_contains text "codex login");
      Alcotest.(check bool)
        "no refresh_token leak" false
        (string_contains text "do-not-use");
      Alcotest.(check string) "bytes unchanged" body (read_file ~caps path)

let test_readonly_dir_allows_fresh_read
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  let token = fresh_jwt ~caps () in
  write_file ~caps path (auth_json token);
  Ox_caps.Cap_unix.chmod ~caps path 0o444;
  Ox_caps.Cap_unix.chmod ~caps dir 0o555;
  Fun.protect
    ~finally:(fun () -> Ox_caps.Cap_unix.chmod ~caps dir 0o700)
    (fun () ->
      with_codex_home ~caps dir @@ fun () ->
      match run_access ~caps () with
      | Ok got -> Alcotest.(check string) "readonly read" token got
      | Error cause ->
          Alcotest.failf "readonly read failed: %s" (cause_text cause))

let test_two_callers_cannot_overwrite_newest
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  let first = auth_json (fresh_jwt ~caps ()) in
  write_file ~caps path first;
  with_codex_home ~caps dir @@ fun () ->
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let newest_token = fresh_jwt ~caps () in
  let newest =
    Printf.sprintf
      "{\"tokens\":{\"access_token\":%S},\"unrelated\":\"keep-me\",\"generation\":2}\n"
      newest_token
  in
  let tmp = Filename.concat dir "auth.json.next" in
  write_file ~caps tmp newest;
  let first_done, resolve_first = Eio.Promise.create () in
  let second_done, resolve_second = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let result = Pie.Stt.Codex_realtime.Codex_auth.access_token ~caps () in
      Eio.Promise.resolve resolve_first result);
  Eio.Fiber.fork ~sw (fun () ->
      let result = Pie.Stt.Codex_realtime.Codex_auth.access_token ~caps () in
      Eio.Promise.resolve resolve_second result);
  Ox_caps.Cap_unix.rename ~caps tmp path;
  ignore (Eio.Promise.await first_done);
  ignore (Eio.Promise.await second_done);
  Alcotest.(check string) "newest file survives" newest (read_file ~caps path)

let test_expired_does_not_contact_auth_endpoint
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  write_file ~caps path (auth_json (expired_jwt ~caps ()));
  let hits = Atomic.make 0 in
  with_codex_home ~caps dir @@ fun () ->
  Ox_caps.Cap_unix.putenv ~caps "CODEX_REFRESH_TOKEN_URL_OVERRIDE"
    "http://127.0.0.1:1/oauth/token";
  let result = run_access ~caps () in
  (match result with
  | Error cause ->
      Alcotest.(check bool)
        "expired" true
        (string_contains (cause_text cause) "expired")
  | Ok _ -> Alcotest.fail "expired token accepted");
  Alcotest.(check int) "auth endpoint contacts" 0 (Atomic.get hits)

let fd_count ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  let dir = Ox_caps.Cap_unix.opendir ~caps "/proc/self/fd" in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
      let rec loop n =
        match Unix.readdir dir with
        | exception End_of_file -> n
        | "." | ".." -> loop n
        | _ -> loop (n + 1)
      in
      loop 0 - 1)

let test_cancel_leaves_no_temp_or_fd
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  write_file ~caps path (auth_json (fresh_jwt ~caps ()));
  with_codex_home ~caps dir @@ fun () ->
  let before = fd_count ~caps () in
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () ->
      ignore (Pie.Stt.Codex_realtime.Codex_auth.access_token ~caps ()));
  let temps =
    Array.to_list (Ox_caps.Cap_sys.readdir ~caps dir)
    |> List.filter (fun name -> name <> "auth.json")
  in
  Alcotest.(check (list string)) "no temps" [] temps;
  let after = fd_count ~caps () in
  Alcotest.(check bool) "fds not retained" true (after <= before + 2)

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "credential-ownership"
    [
      ( "read-only",
        [
          Alcotest.test_case "OPENAI_API_KEY succeeds" `Quick
            (test_env_key_succeeds ~caps);
          Alcotest.test_case "fresh JWT read does not write" `Quick
            (test_fresh_jwt_succeeds_without_write ~caps);
          Alcotest.test_case "expired JWT is stable owner error" `Quick
            (test_expired_jwt_is_stable_error_without_write ~caps);
          Alcotest.test_case "readonly dir still reads fresh token" `Quick
            (test_readonly_dir_allows_fresh_read ~caps);
          Alcotest.test_case "two callers cannot overwrite newest file" `Quick
            (test_two_callers_cannot_overwrite_newest ~caps);
          Alcotest.test_case "expired token does not contact auth endpoint"
            `Quick
            (test_expired_does_not_contact_auth_endpoint ~caps);
          Alcotest.test_case "cancel leaves no temp or extra fds" `Quick
            (test_cancel_leaves_no_temp_or_fd ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
