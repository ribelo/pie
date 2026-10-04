(* ^pcaps-n1rg *)

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
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-removed-wakeword" ""
  in
  Ox_caps.Cap_sys.remove ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf ~caps dir) (fun () -> f dir)

let write_file ~(caps : < Ox_caps.Cap.fs_write ; .. >) path contents =
  let oc =
    Ox_caps.Cap_stdlib.open_out_gen ~caps
      [ Open_creat; Open_trunc; Open_wronly ]
      0o700 path
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let isolate_env ~(caps : < Ox_caps.Cap.env ; .. >) sandbox =
  Ox_caps.Cap_unix.putenv ~caps "HOME" sandbox;
  Ox_caps.Cap_unix.putenv ~caps "XDG_CONFIG_HOME"
    (Filename.concat sandbox "config");
  Ox_caps.Cap_unix.putenv ~caps "XDG_DATA_HOME" (Filename.concat sandbox "data");
  Ox_caps.Cap_unix.putenv ~caps "XDG_RUNTIME_DIR"
    (Filename.concat sandbox "runtime");
  Ox_caps.Cap_unix.putenv ~caps "TMPDIR" sandbox;
  Ox_caps.Cap_unix.putenv ~caps "TMP" sandbox

let run_pie
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.stdin
       ; .. >) ~sandbox ~sentinel args =
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let bin = Filename.concat sandbox "bin" in
  let env =
    Array.of_list
      [
        "PATH=" ^ bin ^ ":"
        ^ Option.value ~default:"/usr/bin"
            (Ox_caps.Cap_sys.getenv_opt ~caps "PATH");
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
    Ox_caps.Cap_unix.create_process_env ~caps pie_exe
      (Array.of_list (pie_exe :: args))
      env
      (Unix.descr_of_in_channel (Ox_caps.Cap_stdlib.stdin ~caps))
      stdout_w stderr_w
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

let with_helpers ~(caps : < Ox_caps.Cap.fs_write ; .. >) sandbox f =
  List.iter
    (fun name ->
      Ox_caps.Cap_unix.mkdir ~caps (Filename.concat sandbox name) 0o700)
    [ "bin"; "config"; "data"; "runtime" ];
  let sentinel = Filename.concat sandbox "sentinel" in
  List.iter
    (fun name ->
      let path = Filename.concat (Filename.concat sandbox "bin") name in
      write_file ~caps path sentinel_script;
      Ox_caps.Cap_unix.chmod ~caps path 0o700)
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

let test_help_omits_wakeword_commands
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun sandbox ->
  with_helpers ~caps sandbox @@ fun ~sentinel ->
  let status, stdout, stderr = run_pie ~caps ~sandbox ~sentinel [ "--help" ] in
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

let test_removed_names_fail_before_helpers
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun sandbox ->
  with_helpers ~caps sandbox @@ fun ~sentinel ->
  List.iter
    (fun name ->
      let status, _stdout, _stderr =
        run_pie ~caps ~sandbox ~sentinel [ name ]
      in
      ignore (exited_nonzero status);
      Alcotest.(check bool)
        (name ^ " did not access Pulse/provider helpers")
        false (Sys.file_exists sentinel))
    removed_commands

let load_error ~(caps : < Ox_caps.Cap.env ; .. >) path =
  Eio_main.run @@ fun env ->
  match
    Pie.Config.Stt.load ~caps ~fs:(Eio.Stdenv.fs env) ~source:(`Explicit path)
      ()
  with
  | Error (`Config msg) -> msg
  | _ -> Alcotest.fail "expected config error"

let test_wakeword_settings_rejected
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  isolate_env ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps (Filename.concat dir "config") 0o700;
  Ox_caps.Cap_unix.mkdir ~caps (Filename.concat dir "data") 0o700;
  Ox_caps.Cap_unix.mkdir ~caps (Filename.concat dir "runtime") 0o700;
  List.iter
    (fun key ->
      let path = Filename.concat dir "stt.toml" in
      write_file ~caps path (key ^ " = true\n");
      let msg = load_error ~caps path in
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

let test_ptt_config_without_wakeword_loads
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  isolate_env ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps (Filename.concat dir "config") 0o700;
  Ox_caps.Cap_unix.mkdir ~caps (Filename.concat dir "data") 0o700;
  Ox_caps.Cap_unix.mkdir ~caps (Filename.concat dir "runtime") 0o700;
  let path = Filename.concat dir "stt.toml" in
  write_file ~caps path
    "ptt_min_capture_rms = 0.0\n\
     ptt_min_capture_peak = 0.0\n\
     ptt_transcribe_keysym = 65478\n\
     ptt_translate_keysym = 65479\n";
  let config =
    Eio_main.run @@ fun env ->
    match
      Pie.Config.Stt.load ~caps ~fs:(Eio.Stdenv.fs env) ~source:(`Explicit path)
        ()
    with
    | Ok config -> config
    | Error (`Config msg) -> Alcotest.fail msg
  in
  Alcotest.(check (float 0.0)) "rms 0" 0.0 config.ptt_min_capture_rms;
  Alcotest.(check (float 0.0)) "peak 0" 0.0 config.ptt_min_capture_peak;
  Alcotest.(check int) "transcribe keysym" 65478 config.ptt_transcribe_keysym;
  Alcotest.(check int) "translate keysym" 65479 config.ptt_translate_keysym

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "removed-wakeword"
    [
      ( "cli",
        [
          Alcotest.test_case "help omits wakeword commands" `Quick
            (test_help_omits_wakeword_commands ~caps);
          Alcotest.test_case "removed names fail before helpers" `Quick
            (test_removed_names_fail_before_helpers ~caps);
        ] );
      ( "config",
        [
          Alcotest.test_case "wakeword settings are unsupported" `Quick
            (test_wakeword_settings_rejected ~caps);
          Alcotest.test_case "PTT config loads without wakeword fields" `Quick
            (test_ptt_config_without_wakeword_loads ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
