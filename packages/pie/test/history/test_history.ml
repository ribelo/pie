(* dict-dozi dict-3dqh dict-s0jv dict-zzju dict-5xmu dict-z87z dict-b18k dict-6frx *)

let caps = Ox_test.powerbox ()

let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let fail_history = function
  | Error (`History message) -> Alcotest.fail message
  | Ok value -> value

let perm path = (Unix.lstat path).Unix.st_perm land 0o777

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec rm_r path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter
        (fun name ->
          if name <> "." && name <> ".." then rm_r (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_root f =
  let root = Filename.temp_file "pie-history" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> rm_r root) (fun () -> f root)

let with_umask mask f =
  let previous = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask previous)) f

let transcribed ?(id = "attempt-1") ?(time = 1_700_000_000.)
    ?(mode = Pie.History.Transcribe) ?(binding = "keysym=65+shift pass2")
    ?(raw_text = "hello nema") () =
  {
    Pie.History.id;
    time;
    mode;
    binding;
    window =
      Some { id = Some 42; app_id = Some "firefox"; title = Some "Inbox" };
    glossary_paths = [ "/gloss/global.md"; ".pie/glossary.md" ];
    pass1_model = "codex-realtime/gpt-live-transcribe";
    pass1_duration_ms = 600;
    raw_text;
  }

let finished ?(id = "attempt-1") ?(time = 1_700_000_001.5)
    ?(final_text = Some "Hello Nema") ?(outcome = Pie.History.Delivered)
    ?(diagnostic = Some "ok") ?(pass2_duration_ms = Some 900)
    ?(delivery = Some Pie.History.Pasted) () =
  {
    Pie.History.id;
    time;
    final_text;
    pass2_model = Some "openrouter/google/gemini";
    pass2_duration_ms;
    outcome;
    delivery;
    diagnostic;
    audio_path = None;
    response_path = None;
    first_response_path = None;
    retry = None;
  }

let test_round_trip () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      let raw = transcribed ~raw_text:"hello\nnema" () in
      let retry : Pie.History.retry =
        {
          first_diagnostic = "unavailable";
          final_diagnostic = "stt:ok";
          audio_format = Some "wav";
          reasoning_effort = Some "medium";
        }
      in
      let done_ =
        {
          (finished ~final_text:(Some "Hello\nNema") ()) with
          response_path = Some "/audio/attempt-1.response.json";
          first_response_path = Some "/audio/attempt-1.response-1.json";
          retry = Some retry;
        }
      in
      fail_history (Pie.History.append_transcribed ~caps ~path raw);
      fail_history (Pie.History.append_finished ~caps ~path done_);
      let contents = read_file path in
      let lines = String.split_on_char '\n' contents in
      Alcotest.(check int) "two complete lines" 3 (List.length lines);
      Alcotest.(check string)
        "file ends with newline" ""
        (List.hd (List.rev lines));
      Alcotest.(check bool)
        "newline in text is escaped" true
        (string_contains (List.hd lines) "hello\\nnema");
      let entries = fail_history (Pie.History.read ~caps ~path) in
      match entries with
      | [ entry ] ->
          Alcotest.(check string) "id" "attempt-1" entry.id;
          Alcotest.(check (option string))
            "response path" done_.response_path entry.response_path;
          Alcotest.(check (option string))
            "first response path" done_.first_response_path
            entry.first_response_path;
          Alcotest.(check bool)
            "retry roundtrip dict-x6e9" true
            (entry.retry = done_.retry);
          Alcotest.(check string)
            "time" "2023-11-14T22:13:20.000Z"
            (match
               Yojson.Safe.Util.member "time" (Pie.History.json_of_entry entry)
             with
            | `String text -> text
            | _ -> Alcotest.fail "time is not a string");
          Alcotest.(check bool)
            "mode" true
            (entry.mode = Some Pie.History.Transcribe);
          Alcotest.(check (option string))
            "binding" (Some "keysym=65+shift pass2") entry.binding;
          Alcotest.(check (option string))
            "app" (Some "firefox")
            (Option.bind entry.window (fun window -> window.app_id));
          Alcotest.(check (list string))
            "glossary" raw.glossary_paths entry.glossary_paths;
          Alcotest.(check (option string))
            "pass1" (Some raw.pass1_model) entry.pass1_model;
          Alcotest.(check (option int))
            "pass1 ms" (Some 600) entry.pass1_duration_ms;
          Alcotest.(check (option string))
            "raw" (Some "hello\nnema") entry.raw_text;
          Alcotest.(check (option string))
            "final" (Some "Hello\nNema") entry.final_text;
          Alcotest.(check (option string))
            "pass2" done_.pass2_model entry.pass2_model;
          Alcotest.(check (option int))
            "pass2 ms" (Some 900) entry.pass2_duration_ms;
          Alcotest.(check bool)
            "outcome" true
            (entry.outcome = Some Pie.History.Delivered);
          Alcotest.(check (option string))
            "diagnostic" (Some "ok") entry.diagnostic;
          Alcotest.(check bool)
            "finished at" true
            (Option.value ~default:0. entry.finished_at = done_.time)
      | _ -> Alcotest.fail "expected one merged entry")

let test_epoch_time () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      fail_history
        (Pie.History.append_transcribed ~caps ~path (transcribed ~time:0. ()));
      let contents = read_file path in
      Alcotest.(check bool)
        "epoch iso" true
        (string_contains contents "1970-01-01T00:00:00.000Z"))

let test_permissions () =
  with_umask 0 (fun () ->
      with_root (fun root ->
          let share = Filename.concat root "share" in
          Unix.mkdir share 0o755;
          let path = Filename.concat share "pie/nested/history.jsonl" in
          fail_history
            (Pie.History.append_transcribed ~caps ~path (transcribed ()));
          Alcotest.(check int) "existing parent stays 0755" 0o755 (perm share);
          Alcotest.(check int)
            "created pie dir 0700" 0o700
            (perm (Filename.concat share "pie"));
          Alcotest.(check int)
            "created nested dir 0700" 0o700
            (perm (Filename.concat share "pie/nested"));
          Alcotest.(check int) "history file 0600" 0o600 (perm path)))

let test_append_only () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      fail_history
        (Pie.History.append_transcribed ~caps ~path (transcribed ~id:"a" ()));
      let first = read_file path in
      fail_history
        (Pie.History.append_finished ~caps ~path (finished ~id:"a" ()));
      let both = read_file path in
      Alcotest.(check bool)
        "second append keeps the first line" true
        (String.starts_with ~prefix:first both);
      Alcotest.(check bool)
        "first write ended a line" true
        (String.ends_with ~suffix:"\n" first))

let test_torn_last_line () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      fail_history
        (Pie.History.append_transcribed ~caps ~path
           (transcribed ~id:"kept" ~time:10. ()));
      let fd =
        Unix.openfile path
          [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CLOEXEC ]
          0o600
      in
      let torn = "{\"v\":1,\"event\":\"transcr" in
      ignore (Unix.single_write_substring fd torn 0 (String.length torn));
      Unix.close fd;
      let before = fail_history (Pie.History.read ~caps ~path) in
      Alcotest.(check int) "torn tail skipped" 1 (List.length before);
      fail_history
        (Pie.History.append_transcribed ~caps ~path
           (transcribed ~id:"later" ~time:20. ~raw_text:"later text" ()));
      let entries = fail_history (Pie.History.read ~caps ~path) in
      let ids =
        List.map (fun (entry : Pie.History.entry) -> entry.id) entries
      in
      Alcotest.(check (list string))
        "torn line does not hide later text" [ "later"; "kept" ] ids;
      let contents = read_file path in
      Alcotest.(check bool)
        "repair newline separates the torn tail" true
        (string_contains contents (torn ^ "\n")))

let test_malformed_lines_skipped () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      let good =
        {|{"v":1,"event":"transcribed","id":"good","time":"2026-09-30T08:00:00.000Z","mode":"transcribe","binding":"keysym=65","glossaryPaths":[],"pass1Model":"m","pass1DurationMs":1,"rawText":"kept"}|}
      in
      write_file path ("{not json}\n" ^ good ^ "\n{also bad}\n");
      let entries = fail_history (Pie.History.read ~caps ~path) in
      match entries with
      | [ entry ] -> Alcotest.(check string) "kept id" "good" entry.id
      | _ -> Alcotest.fail "malformed lines hid the valid event")

let test_merge_and_order () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      fail_history
        (Pie.History.append_transcribed ~caps ~path
           (transcribed ~id:"older" ~time:10. ~raw_text:"raw older" ()));
      fail_history
        (Pie.History.append_transcribed ~caps ~path
           (transcribed ~id:"newer" ~time:20. ~mode:Pie.History.Translate
              ~raw_text:"raw newer" ()));
      fail_history
        (Pie.History.append_finished ~caps ~path
           (finished ~id:"older" ~time:30. ~final_text:(Some "Final older")
              ~outcome:Pie.History.Delivered_raw_after_correction_failure
              ~diagnostic:(Some "timed out") ()));
      let entries = fail_history (Pie.History.read ~caps ~path) in
      match entries with
      | [ newer; older ] ->
          Alcotest.(check string) "newest id" "newer" newer.id;
          Alcotest.(check (option string))
            "newer raw" (Some "raw newer") newer.raw_text;
          Alcotest.(check bool) "newer unfinished" true (newer.outcome = None);
          Alcotest.(check string) "older id" "older" older.id;
          Alcotest.(check (option string))
            "both texts raw" (Some "raw older") older.raw_text;
          Alcotest.(check (option string))
            "both texts final" (Some "Final older") older.final_text;
          Alcotest.(check bool)
            "outcome kept" true
            (older.outcome
           = Some Pie.History.Delivered_raw_after_correction_failure);
          let line = Pie.History.format_listing_line older in
          Alcotest.(check bool)
            "listing prefers final text" true
            (string_contains line "Final older");
          Alcotest.(check bool)
            "listing names the long outcome" true
            (string_contains line "delivered-raw-after-correction-failure")
      | _ -> Alcotest.fail "expected newer then older")

let test_write_failure_is_result () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      fail_history
        (Pie.History.append_transcribed ~caps ~path
           (transcribed ~raw_text:"keep me" ()));
      let before = read_file path in
      Unix.chmod path 0o644;
      (match
         Pie.History.append_finished ~caps ~path
           (finished ~final_text:(Some "nope") ())
       with
      | Error (`History _) -> ()
      | Ok () -> Alcotest.fail "appended to a loose history file");
      Alcotest.(check string) "loose file unchanged" before (read_file path);
      Alcotest.(check int) "mode not rewritten" 0o644 (perm path);
      let blocked = Filename.concat root "blocked" in
      Unix.mkdir blocked 0o500;
      Fun.protect
        ~finally:(fun () -> Unix.chmod blocked 0o700)
        (fun () ->
          match
            Pie.History.append_transcribed ~caps
              ~path:(Filename.concat blocked "history.jsonl")
              (transcribed ~id:"blocked" ())
          with
          | Error (`History _) -> ()
          | Ok () ->
              Alcotest.fail "wrote into a directory without write permission");
      Unix.symlink
        (Filename.concat root "secret")
        (Filename.concat root "link.jsonl");
      (match
         Pie.History.append_transcribed ~caps
           ~path:(Filename.concat root "link.jsonl")
           (transcribed ~id:"link" ())
       with
      | Error (`History message) ->
          Alcotest.(check bool)
            "symlink named" true
            (string_contains message "symbolic")
      | Ok () -> Alcotest.fail "followed a history symlink");
      Alcotest.(check bool)
        "symlink target not created" false
        (Sys.file_exists (Filename.concat root "secret"));
      (match
         Pie.History.append_transcribed ~caps ~path (transcribed ~id:"last" ())
       with
      | Error (`History _) -> ()
      | Ok () -> Alcotest.fail "reserved id last was stored");
      match
        Pie.History.append_finished ~caps ~path
          (finished ~id:"attempt-1" ~pass2_duration_ms:(Some (-1)) ())
      with
      | Error (`History _) -> ()
      | Ok () -> Alcotest.fail "negative duration was stored")

let test_missing_file_and_path () =
  with_root (fun root ->
      let path = Filename.concat root "missing.jsonl" in
      let entries = fail_history (Pie.History.read ~caps ~path) in
      Alcotest.(check int) "missing is empty" 0 (List.length entries);
      Alcotest.(check bool) "read does not create" false (Sys.file_exists path);
      Alcotest.(check string)
        "default path"
        (Filename.concat (Pie.Paths.data_dir ~caps) "history.jsonl")
        (Pie.History.path ~caps))

let test_binding_and_fresh_id () =
  Alcotest.(check string)
    "binding" "keysym=65+shift pass2"
    (Pie.History.describe_binding ~keysym:65 ~shift:true ~pass2:true);
  Alcotest.(check string)
    "binding without flags" "keysym=65"
    (Pie.History.describe_binding ~keysym:65 ~shift:false ~pass2:false);
  let left = Pie.History.fresh_id ~caps () in
  let right = Pie.History.fresh_id ~caps () in
  Alcotest.(check bool) "fresh ids differ" false (String.equal left right);
  Alcotest.(check bool) "fresh id is not last" false (String.equal left "last")

let test_listing_truncation () =
  let letter = "\u{0105}" in
  let long = String.concat "" (List.init 90 (fun _ -> letter)) in
  let preview =
    Pie.History.format_listing_line
      {
        id = "t";
        time = 0.;
        mode = Some Pie.History.Transcribe;
        binding = Some "keysym=65";
        window = None;
        glossary_paths = [];
        pass1_model = Some "m";
        pass1_duration_ms = Some 1;
        raw_text = Some "second line should not appear\nHIDDEN";
        final_text = Some (long ^ "\nHIDDEN");
        pass2_model = None;
        pass2_duration_ms = None;
        outcome = None;
        diagnostic = None;
        finished_at = None;
        audio_path = None;
        response_path = None;
        first_response_path = None;
        retry = None;
        delivery = None;
      }
  in
  Alcotest.(check bool) "ellipsis" true (string_contains preview "...");
  Alcotest.(check bool)
    "second line hidden" false
    (string_contains preview "HIDDEN");
  Alcotest.(check bool)
    "missing outcome dash" true
    (string_contains preview "  -  ")

let pie_exe =
  Filename.concat (Filename.dirname Sys.executable_name) "../../bin/pie.exe"

let child_deadline_s = 8.0

type cli_run = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let read_available buf fd =
  let bytes = Bytes.create 4096 in
  try
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> `Eof
    | n ->
        Buffer.add_subbytes buf bytes 0 n;
        `Read
  with
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> `Empty
  | Unix.Unix_error (Unix.EBADF, _, _) -> `Eof

let rec drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
    stdout_open stderr_open =
  if not (stdout_open || stderr_open) then (false, false)
  else
    let now = Unix.gettimeofday () in
    if now >= deadline then (stdout_open, stderr_open)
    else
      let timeout = min 0.05 (deadline -. now) in
      let waiting =
        (if stdout_open then [ stdout_r ] else [])
        @ if stderr_open then [ stderr_r ] else []
      in
      let ready, _, _ =
        try Unix.select waiting [] [] timeout
        with Unix.Unix_error (Unix.EBADF, _, _) -> ([], [], [])
      in
      let stdout_open, stdout_more =
        if List.mem stdout_r ready then
          match read_available stdout_buf stdout_r with
          | `Eof ->
              close_fd stdout_r;
              (false, false)
          | `Read -> (true, true)
          | `Empty -> (true, false)
        else (stdout_open, false)
      in
      let stderr_open, stderr_more =
        if List.mem stderr_r ready then
          match read_available stderr_buf stderr_r with
          | `Eof ->
              close_fd stderr_r;
              (false, false)
          | `Read -> (true, true)
          | `Empty -> (true, false)
        else (stderr_open, false)
      in
      if stdout_more || stderr_more then
        drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
          stdout_open stderr_open
      else (stdout_open, stderr_open)

let run_pie ?(home = "") ?(data = "") args =
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock stdout_r;
  Unix.set_nonblock stderr_r;
  let home = if home = "" then data else home in
  let env =
    [|
      "PATH=" ^ Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH");
      "HOME=" ^ home;
      "XDG_CONFIG_HOME=" ^ home;
      "XDG_DATA_HOME=" ^ data;
      "XDG_RUNTIME_DIR=" ^ home;
      "TZ=UTC";
      "LC_ALL=C";
      "OTEL_SDK_DISABLED=true";
    |]
  in
  let pid =
    Unix.create_process_env pie_exe
      (Array.of_list (pie_exe :: args))
      env Unix.stdin stdout_w stderr_w
  in
  close_fd stdout_w;
  close_fd stderr_w;
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  let deadline = Unix.gettimeofday () +. child_deadline_s in
  let rec wait_child stdout_open stderr_open =
    let stdout_open, stderr_open =
      drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
        stdout_open stderr_open
    in
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then (
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
          snd (Unix.waitpid [] pid))
        else wait_child stdout_open stderr_open
    | _, status ->
        ignore
          (drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
             stdout_open stderr_open);
        status
  in
  let status =
    Fun.protect
      ~finally:(fun () ->
        close_fd stdout_r;
        close_fd stderr_r)
      (fun () -> wait_child true true)
  in
  {
    status;
    stdout = Buffer.contents stdout_buf;
    stderr = Buffer.contents stderr_buf;
  }

let require_exited_zero run =
  match run.status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Alcotest.failf "expected exit 0, got %d\nstdout=%s\nstderr=%s" n
        run.stdout run.stderr
  | Unix.WSIGNALED n -> Alcotest.failf "expected exit 0, signaled %d" n
  | Unix.WSTOPPED n -> Alcotest.failf "expected exit 0, stopped %d" n

let require_exited_nonzero run =
  match run.status with
  | Unix.WEXITED 0 ->
      Alcotest.failf "expected nonzero exit\nstdout=%s\nstderr=%s" run.stdout
        run.stderr
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n ->
      Alcotest.failf "expected normal nonzero exit, signaled %d" n
  | Unix.WSTOPPED n ->
      Alcotest.failf "expected normal nonzero exit, stopped %d" n

let mkdir_p path =
  let rec loop path =
    if path = "" || path = Filename.dirname path || Sys.file_exists path then ()
    else (
      loop (Filename.dirname path);
      Unix.mkdir path 0o700)
  in
  loop path

let history_jsonl =
  {|{"v":1,"event":"transcribed","id":"older","time":"2026-09-30T08:00:00.000Z","mode":"transcribe","binding":"keysym=65+shift pass2","window":{"appId":"firefox","title":"Inbox"},"glossaryPaths":["/gloss/global.md",".pie/glossary.md"],"pass1Model":"codex-realtime/gpt-live-transcribe","pass1DurationMs":600,"rawText":"hello nema"}
{"v":1,"event":"finished","id":"older","time":"2026-09-30T08:00:01.500Z","finalText":"Hello Nema","pass2Model":"openrouter/google/gemini","pass2DurationMs":900,"outcome":"delivered"}
{"v":1,"event":"transcribed","id":"newer","time":"2026-09-30T09:15:07.000Z","mode":"translate","binding":"keysym=66","glossaryPaths":[],"pass1Model":"openrouter/microsoft/mai-transcribe-2","pass1DurationMs":1000,"rawText":"raw newest"}
{"v":1,"event":"finished","id":"newer","time":"2026-09-30T09:15:08.000Z","finalText":"First line of the newest\nsecond line","pass2Model":"openrouter/google/gemini","pass2DurationMs":400,"outcome":"delivered-raw-after-correction-failure","diagnostic":"correction timed out"}
|}

let with_history ?(contents = history_jsonl) f =
  with_root (fun data ->
      let dir = Filename.concat data "pie" in
      Unix.mkdir dir 0o700;
      if contents <> "" then
        write_file (Filename.concat dir "history.jsonl") contents;
      f data)

let listing_text =
  "2026-09-30 09:15:07  translate  delivered-raw-after-correction-failure  \
   First line of the newest\n\
   2026-09-30 08:00:00  transcribe  delivered  Hello Nema\n"

let test_cli_list () =
  with_history (fun data ->
      let run = run_pie ~data [ "history" ] in
      require_exited_zero run;
      Alcotest.(check string) "stderr empty" "" run.stderr;
      Alcotest.(check string) "newest first dict-zzju" listing_text run.stdout;
      let limited = run_pie ~data [ "history"; "--limit"; "1" ] in
      require_exited_zero limited;
      Alcotest.(check string)
        "limit"
        "2026-09-30 09:15:07  translate  \
         delivered-raw-after-correction-failure  First line of the newest\n"
        limited.stdout)

let test_cli_json () =
  with_history (fun data ->
      let run = run_pie ~data [ "history"; "--json" ] in
      require_exited_zero run;
      Alcotest.(check string) "stderr empty" "" run.stderr;
      match Yojson.Safe.from_string run.stdout with
      | `List [ newer; older ] ->
          let id json =
            match Yojson.Safe.Util.member "id" json with
            | `String text -> text
            | _ -> Alcotest.fail "id"
          in
          Alcotest.(check string) "newest id" "newer" (id newer);
          Alcotest.(check string) "older id" "older" (id older);
          Alcotest.(check string)
            "outcome" "delivered-raw-after-correction-failure"
            (Yojson.Safe.Util.to_string
               (Yojson.Safe.Util.member "outcome" newer));
          Alcotest.(check string)
            "raw" "raw newest"
            (Yojson.Safe.Util.to_string
               (Yojson.Safe.Util.member "rawText" newer));
          Alcotest.(check string)
            "final" "First line of the newest\nsecond line"
            (Yojson.Safe.Util.to_string
               (Yojson.Safe.Util.member "finalText" newer));
          Alcotest.(check string)
            "diagnostic" "correction timed out"
            (Yojson.Safe.Util.to_string
               (Yojson.Safe.Util.member "diagnostic" newer));
          Alcotest.(check string)
            "app" "firefox"
            (Yojson.Safe.Util.to_string
               (Yojson.Safe.Util.member "appId"
                  (Yojson.Safe.Util.member "window" older)))
      | _ -> Alcotest.failf "expected two JSON entries, got %s" run.stdout)

let test_cli_show () =
  with_history (fun data ->
      let last = run_pie ~data [ "history"; "show" ] in
      require_exited_zero last;
      Alcotest.(check string)
        "show last exact" "First line of the newest\nsecond line\n" last.stdout;
      Alcotest.(check string) "show stderr empty" "" last.stderr;
      let by_id = run_pie ~data [ "history"; "show"; "older" ] in
      require_exited_zero by_id;
      Alcotest.(check string) "show id" "Hello Nema\n" by_id.stdout;
      let raw = run_pie ~data [ "history"; "show"; "--raw"; "last" ] in
      require_exited_zero raw;
      Alcotest.(check string) "show raw" "raw newest\n" raw.stdout;
      let raw_after = run_pie ~data [ "history"; "show"; "newer"; "--raw" ] in
      require_exited_zero raw_after;
      Alcotest.(check string)
        "show raw after id" "raw newest\n" raw_after.stdout;
      let json = run_pie ~data [ "history"; "show"; "older"; "--json" ] in
      require_exited_zero json;
      match Yojson.Safe.from_string json.stdout with
      | `Assoc _ as entry ->
          Alcotest.(check string)
            "json id" "older"
            (Yojson.Safe.Util.to_string (Yojson.Safe.Util.member "id" entry));
          Alcotest.(check string)
            "json final" "Hello Nema"
            (Yojson.Safe.Util.to_string
               (Yojson.Safe.Util.member "finalText" entry));
          Alcotest.(check string)
            "json raw" "hello nema"
            (Yojson.Safe.Util.to_string
               (Yojson.Safe.Util.member "rawText" entry))
      | _ -> Alcotest.fail "show --json was not an object")

let test_cli_errors_and_empty () =
  with_history (fun data ->
      let missing = run_pie ~data [ "history"; "show"; "no-such" ] in
      ignore (require_exited_nonzero missing);
      Alcotest.(check string) "unknown stdout empty" "" missing.stdout;
      Alcotest.(check bool)
        "unknown id named" true
        (string_contains missing.stderr "unknown history entry no-such");
      let both = run_pie ~data [ "history"; "show"; "--raw"; "--json" ] in
      ignore (require_exited_nonzero both);
      Alcotest.(check bool)
        "raw and json rejected" true
        (string_contains both.stderr "--raw and --json");
      let bad_limit = run_pie ~data [ "history"; "--limit"; "0" ] in
      ignore (require_exited_nonzero bad_limit);
      Alcotest.(check bool)
        "limit usage" true
        (string_contains (bad_limit.stderr ^ bad_limit.stdout) "positive"));
  with_history ~contents:"" (fun data ->
      let listed = run_pie ~data [ "history" ] in
      require_exited_zero listed;
      Alcotest.(check string) "empty list" "" listed.stdout;
      let json = run_pie ~data [ "history"; "--json" ] in
      require_exited_zero json;
      Alcotest.(check string) "empty json" "[]\n" json.stdout;
      Alcotest.(check bool)
        "listing does not create the file" false
        (Sys.file_exists (Filename.concat data "pie/history.jsonl"));
      let shown = run_pie ~data [ "history"; "show" ] in
      ignore (require_exited_nonzero shown);
      Alcotest.(check string) "empty show stdout" "" shown.stdout;
      Alcotest.(check bool)
        "empty show message" true
        (string_contains shown.stderr "no history"))

let test_cli_uses_xdg_data_home () =
  with_root (fun home ->
      with_root (fun data ->
          let decoy = Filename.concat home ".local/share/pie" in
          mkdir_p decoy;
          write_file (Filename.concat decoy "history.jsonl") "DECOY\n";
          let real_dir = Filename.concat data "pie" in
          Unix.mkdir real_dir 0o700;
          write_file (Filename.concat real_dir "history.jsonl") history_jsonl;
          let run = run_pie ~home ~data [ "history"; "show"; "older" ] in
          require_exited_zero run;
          Alcotest.(check string) "xdg data not home" "Hello Nema\n" run.stdout;
          Alcotest.(check bool)
            "decoy ignored" false
            (string_contains run.stdout "DECOY")))

let test_cli_help () =
  with_root (fun data ->
      let run = run_pie ~data [ "history"; "--help" ] in
      require_exited_zero run;
      let help = run.stdout ^ run.stderr in
      Alcotest.(check bool)
        "help mentions show" true
        (string_contains help "show");
      Alcotest.(check bool)
        "help mentions limit" true
        (string_contains help "limit"))

let test_audio_path_is_optional () =
  (* dict-z1ax: old lines still read; show JSON carries the archive path. *)
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      let channel =
        open_out_gen [ Open_creat; Open_wronly; Open_trunc ] 0o600 path
      in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () ->
          output_string channel
            "{\"v\":1,\"event\":\"finished\",\"id\":\"old\",\"time\":\"2023-11-14T22:13:20.000Z\",\"outcome\":\"delivered\"}\n");
      let entries = fail_history (Pie.History.read ~caps ~path) in
      (match entries with
      | [ entry ] ->
          Alcotest.(check (option string))
            "old line has no audio" None entry.audio_path
      | _ -> Alcotest.fail "expected the old finished line");
      let noted =
        { (finished ~id:"noted" ()) with audio_path = Some "/a/noted.opus" }
      in
      fail_history (Pie.History.append_finished ~caps ~path noted);
      fail_history
        (Pie.History.append_audio ~caps ~path
           { id = "old"; time = 1_700_000_002.; path = "/a/old.wav" });
      let entries = fail_history (Pie.History.read ~caps ~path) in
      let audio id =
        match
          List.find_opt
            (fun (entry : Pie.History.entry) -> entry.id = id)
            entries
        with
        | Some entry -> entry.audio_path
        | None -> Alcotest.fail ("missing " ^ id)
      in
      Alcotest.(check (option string))
        "audio event" (Some "/a/old.wav") (audio "old");
      Alcotest.(check (option string))
        "finished field" (Some "/a/noted.opus") (audio "noted");
      let json =
        Pie.History.json_of_entry
          (List.find (fun (e : Pie.History.entry) -> e.id = "old") entries)
      in
      Alcotest.(check bool)
        "show json" true
        (Yojson.Safe.Util.member "audioPath" json = `String "/a/old.wav"))

let test_delivery_round_trip () =
  List.iter
    (fun delivery ->
      with_root (fun root ->
          let path = Filename.concat root "history.jsonl" in
          fail_history
            (Pie.History.append_transcribed ~caps ~path (transcribed ()));
          fail_history
            (Pie.History.append_finished ~caps ~path
               (finished ~delivery:(Some delivery) ()));
          match fail_history (Pie.History.read ~caps ~path) with
          | [ entry ] ->
              Alcotest.(check bool)
                "delivery survives round trip" true
                (entry.delivery = Some delivery);
              Alcotest.(check bool)
                "captured id survives round trip" true
                (Option.bind entry.window (fun window -> window.id) = Some 42);
              let json = Pie.History.json_of_entry entry in
              Alcotest.(check string)
                "delivery in CLI JSON"
                (Pie.History.string_of_delivery delivery)
                Yojson.Safe.Util.(json |> member "delivery" |> to_string);
              Alcotest.(check int)
                "window id in CLI JSON" 42
                Yojson.Safe.Util.(
                  json |> member "window" |> member "id" |> to_int)
          | _ -> Alcotest.fail "expected one history entry"))
    [ Pie.History.Pasted; Pie.History.Clipboard ]

let test_old_delivery_fields_absent () =
  with_root (fun root ->
      let path = Filename.concat root "history.jsonl" in
      fail_history (Pie.History.append_transcribed ~caps ~path (transcribed ()));
      fail_history (Pie.History.append_finished ~caps ~path (finished ()));
      let old_event = function
        | `Assoc fields ->
            `Assoc
              (List.filter_map
                 (fun (key, value) ->
                   if key = "delivery" then None
                   else if key = "window" then
                     match value with
                     | `Assoc fields ->
                         Some (key, `Assoc (List.remove_assoc "id" fields))
                     | value -> Some (key, value)
                   else Some (key, value))
                 fields)
        | value -> value
      in
      read_file path |> String.split_on_char '\n'
      |> List.filter (fun line -> line <> "")
      |> List.map (fun line ->
          Yojson.Safe.from_string line |> old_event |> Yojson.Safe.to_string)
      |> fun lines ->
      write_file path (String.concat "\n" lines ^ "\n");
      match fail_history (Pie.History.read ~caps ~path) with
      | [ entry ] ->
          Alcotest.(check bool)
            "old history has unknown delivery" true (entry.delivery = None);
          Alcotest.(check bool)
            "old history keeps title without id" true
            (entry.window
            = Some
                {
                  Pie.History.id = None;
                  app_id = Some "firefox";
                  title = Some "Inbox";
                })
      | _ -> Alcotest.fail "expected one legacy history entry")

let () =
  Alcotest.run "history"
    [
      ( "store",
        [
          Alcotest.test_case "dict-ib9n delivery and window id round trip"
            `Quick test_delivery_round_trip;
          Alcotest.test_case "dict-ib9n older history has no delivery or id"
            `Quick test_old_delivery_fields_absent;
          Alcotest.test_case
            "append and read round trip dict-dozi dict-3dqh dict-s0jv" `Quick
            test_round_trip;
          Alcotest.test_case "epoch time encodes as UTC ISO dict-s0jv" `Quick
            test_epoch_time;
          Alcotest.test_case "file 0600 and directory 0700 dict-b18k" `Quick
            test_permissions;
          Alcotest.test_case "append only dict-b18k" `Quick test_append_only;
          Alcotest.test_case "torn last line skipped and repaired dict-b18k"
            `Quick test_torn_last_line;
          Alcotest.test_case "malformed lines skipped dict-b18k" `Quick
            test_malformed_lines_skipped;
          Alcotest.test_case
            "merge and newest first dict-3dqh dict-s0jv dict-zzju" `Quick
            test_merge_and_order;
          Alcotest.test_case "write failure is a result dict-6frx dict-b18k"
            `Quick test_write_failure_is_result;
          Alcotest.test_case "missing file and default path dict-b18k" `Quick
            test_missing_file_and_path;
          Alcotest.test_case "binding description and fresh id dict-s0jv" `Quick
            test_binding_and_fresh_id;
          Alcotest.test_case "listing truncation dict-zzju" `Quick
            test_listing_truncation;
          Alcotest.test_case
            "audio path is optional and shown in json dict-z1ax" `Quick
            test_audio_path_is_optional;
        ] );
      ( "cli",
        [
          Alcotest.test_case "pie history lists newest dict-zzju" `Quick
            test_cli_list;
          Alcotest.test_case "pie history --json dict-z87z" `Quick test_cli_json;
          Alcotest.test_case
            "pie history show last raw and json dict-5xmu dict-z87z" `Quick
            test_cli_show;
          Alcotest.test_case "unknown id and empty history dict-5xmu dict-b18k"
            `Quick test_cli_errors_and_empty;
          Alcotest.test_case "history path is XDG_DATA_HOME dict-b18k" `Quick
            test_cli_uses_xdg_data_home;
          Alcotest.test_case "history help dict-zzju" `Quick test_cli_help;
        ] );
    ]
