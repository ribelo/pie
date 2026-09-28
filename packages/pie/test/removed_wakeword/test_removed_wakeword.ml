(* PIE024: removed wakeword CLI/config fail before capture; PTT remains. *)

let pie_exe =
  Filename.concat (Filename.dirname Sys.executable_name) "../../bin/pie.exe"

let removed_commands = [ "wakeword"; "wakeword-tune"; "wakeword-train" ]

let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Sys.readdir path with
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf (Filename.concat path name))
            names
      | exception Sys_error _ -> ());
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let with_sandbox f =
  let dir = Filename.temp_file "pie-removed-wakeword" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let write_file path contents =
  let oc = open_out_gen [ Open_creat; Open_trunc; Open_wronly ] 0o700 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let isolate_env sandbox =
  Unix.putenv "HOME" sandbox;
  Unix.putenv "XDG_CONFIG_HOME" (Filename.concat sandbox "config");
  Unix.putenv "XDG_DATA_HOME" (Filename.concat sandbox "data");
  Unix.putenv "XDG_RUNTIME_DIR" (Filename.concat sandbox "runtime");
  Unix.putenv "TMPDIR" sandbox;
  Unix.putenv "TMP" sandbox

let run_pie ~sandbox ~sentinel args =
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let bin = Filename.concat sandbox "bin" in
  let env =
    Array.of_list
      [
        "PATH=" ^ bin ^ ":"
        ^ Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH");
        "HOME=" ^ sandbox;
        "XDG_CONFIG_HOME=" ^ Filename.concat sandbox "config";
        "XDG_DATA_HOME=" ^ Filename.concat sandbox "data";
        "XDG_RUNTIME_DIR=" ^ Filename.concat sandbox "runtime";
        "TMPDIR=" ^ sandbox;
        "TMP=" ^ sandbox;
        "PULSE_SERVER=unix:" ^ Filename.concat sandbox "no-pulse";
        "OPENROUTER_API_KEY=";
        "OTEL_SDK_DISABLED=true";
        "PIE_REMOVED_WAKEWORD_SENTINEL=" ^ sentinel;
      ]
  in
  let pid =
    Unix.create_process_env pie_exe
      (Array.of_list (pie_exe :: args))
      env Unix.stdin stdout_w stderr_w
  in
  Unix.close stdout_w;
  Unix.close stderr_w;
  let rec slurp fd buf =
    let bytes = Bytes.create 256 in
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf bytes 0 n;
        slurp fd buf
  in
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  slurp stdout_r stdout_buf;
  slurp stderr_r stderr_buf;
  Unix.close stdout_r;
  Unix.close stderr_r;
  let _, status = Unix.waitpid [] pid in
  (status, Buffer.contents stdout_buf, Buffer.contents stderr_buf)

let sentinel_script =
  String.concat "\n"
    [
      "#!/bin/sh";
      "echo accessed >> \"$PIE_REMOVED_WAKEWORD_SENTINEL\"";
      "exit 1";
      "";
    ]

let with_helpers sandbox f =
  List.iter
    (fun name -> Unix.mkdir (Filename.concat sandbox name) 0o700)
    [ "bin"; "config"; "data"; "runtime" ];
  let sentinel = Filename.concat sandbox "sentinel" in
  List.iter
    (fun name ->
      let path = Filename.concat (Filename.concat sandbox "bin") name in
      write_file path sentinel_script;
      Unix.chmod path 0o700)
    [ "pactl"; "pulseaudio"; "curl"; "wget"; "busctl"; "parec" ];
  f ~sentinel

let exited_nonzero = function
  | Unix.WEXITED 0 -> Alcotest.fail "expected nonzero exit, got 0"
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> Alcotest.failf "signaled %d" n
  | Unix.WSTOPPED n -> Alcotest.failf "stopped %d" n

let advertised_as_command help name =
  let rec lines s =
    match String.index_opt s '\n' with
    | None -> if s = "" then [] else [ s ]
    | Some i ->
        String.sub s 0 i
        :: lines (String.sub s (i + 1) (String.length s - i - 1))
  in
  List.exists
    (fun line ->
      let t = String.trim line in
      let nlen = String.length name in
      String.length t >= nlen
      && String.sub t 0 nlen = name
      && (String.length t = nlen || t.[nlen] = ' ')
      && (String.length t = 0 || t.[0] <> '-'))
    (lines help)

let test_help_omits_wakeword_commands () =
  with_sandbox @@ fun sandbox ->
  with_helpers sandbox @@ fun ~sentinel ->
  let status, stdout, stderr = run_pie ~sandbox ~sentinel [ "--help" ] in
  (match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "help exit %d stderr=%s" n stderr
  | _ -> Alcotest.fail "help did not exit");
  let help = stdout ^ stderr in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " not advertised as a command")
        false
        (advertised_as_command help name))
    removed_commands;
  Alcotest.(check bool)
    "help does not claim model training" false
    (string_contains help "Train a custom wakeword");
  Alcotest.(check bool)
    "sentinel unused for --help" false (Sys.file_exists sentinel)

let test_removed_names_fail_before_helpers () =
  with_sandbox @@ fun sandbox ->
  with_helpers sandbox @@ fun ~sentinel ->
  List.iter
    (fun name ->
      let status, _stdout, _stderr = run_pie ~sandbox ~sentinel [ name ] in
      ignore (exited_nonzero status);
      Alcotest.(check bool)
        (name ^ " did not access Pulse/provider helpers")
        false (Sys.file_exists sentinel))
    removed_commands

let load_error path =
  Eio_main.run @@ fun env ->
  match
    Pie.Config.Stt.load ~fs:(Eio.Stdenv.fs env) ~source:(`Explicit path) ()
  with
  | Error (`Config msg) -> msg
  | _ -> Alcotest.fail "expected config error"

let test_wakeword_settings_rejected () =
  with_sandbox @@ fun dir ->
  isolate_env dir;
  Unix.mkdir (Filename.concat dir "config") 0o700;
  Unix.mkdir (Filename.concat dir "data") 0o700;
  Unix.mkdir (Filename.concat dir "runtime") 0o700;
  List.iter
    (fun key ->
      let path = Filename.concat dir "stt.toml" in
      write_file path (key ^ " = true\n");
      let msg = load_error path in
      Alcotest.(check bool)
        (key ^ " mentions unsupported wakeword")
        true
        (string_contains msg "unsupported wakeword"))
    [
      "wakeword_enabled";
      "wakeword_dictation_silence_seconds";
      "wakeword_dictation_max_seconds";
      "wakeword_dictation_speech_rms_threshold";
    ]

let test_ptt_config_without_wakeword_loads () =
  with_sandbox @@ fun dir ->
  isolate_env dir;
  Unix.mkdir (Filename.concat dir "config") 0o700;
  Unix.mkdir (Filename.concat dir "data") 0o700;
  Unix.mkdir (Filename.concat dir "runtime") 0o700;
  let path = Filename.concat dir "stt.toml" in
  write_file path
    "ptt_min_capture_rms = 0.0\n\
     ptt_min_capture_peak = 0.0\n\
     ptt_transcribe_keysym = 65478\n\
     ptt_translate_keysym = 65479\n";
  let config =
    Eio_main.run @@ fun env ->
    match
      Pie.Config.Stt.load ~fs:(Eio.Stdenv.fs env) ~source:(`Explicit path) ()
    with
    | Ok config -> config
    | Error (`Config msg) -> Alcotest.fail msg
  in
  Alcotest.(check (float 0.0)) "rms 0" 0.0 config.ptt_min_capture_rms;
  Alcotest.(check (float 0.0)) "peak 0" 0.0 config.ptt_min_capture_peak;
  Alcotest.(check int) "transcribe keysym" 65478 config.ptt_transcribe_keysym;
  Alcotest.(check int) "translate keysym" 65479 config.ptt_translate_keysym

let () =
  Alcotest.run "removed-wakeword"
    [
      ( "cli",
        [
          Alcotest.test_case "help omits wakeword commands" `Quick
            test_help_omits_wakeword_commands;
          Alcotest.test_case "removed names fail before helpers" `Quick
            test_removed_names_fail_before_helpers;
        ] );
      ( "config",
        [
          Alcotest.test_case "wakeword settings are unsupported" `Quick
            test_wakeword_settings_rejected;
          Alcotest.test_case "PTT config loads without wakeword fields" `Quick
            test_ptt_config_without_wakeword_loads;
        ] );
    ]
