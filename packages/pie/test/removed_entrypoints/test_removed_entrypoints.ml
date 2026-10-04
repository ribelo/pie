(* PIE022: removed prototype CLI names fail before Pulse/provider helpers. *)

let pie_exe =
  Filename.concat (Filename.dirname Sys.executable_name) "../../bin/pie.exe"

let removed_commands =
  [ "ptt-portal"; "ptt"; "ptt-transcribe"; "ptt-translate"; "stt-interactive" ]

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
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-removed-entrypoints" ""
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

let run_pie
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >)
    ~sandbox ~sentinel args =
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
        "DBUS_SESSION_BUS_ADDRESS=unix:path="
        ^ Filename.concat sandbox "no-session-bus";
        "PULSE_SERVER=unix:" ^ Filename.concat sandbox "no-pulse";
        "OPENROUTER_API_KEY=";
        "OTEL_SDK_DISABLED=true";
        "PIE_REMOVED_ENTRYPOINTS_SENTINEL=" ^ sentinel;
      ]
  in
  let pid =
    Ox_caps.Cap_unix.create_process_env ~caps pie_exe
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
  "#!/bin/sh\n" ^ "echo accessed >> \"$PIE_REMOVED_ENTRYPOINTS_SENTINEL\"\n"
  ^ "exit 1\n"

let with_helpers ~(caps : < Ox_caps.Cap.fs_write ; .. >) sandbox f =
  let bin = Filename.concat sandbox "bin" in
  let config = Filename.concat sandbox "config" in
  let data = Filename.concat sandbox "data" in
  let runtime = Filename.concat sandbox "runtime" in
  Ox_caps.Cap_unix.mkdir ~caps bin 0o700;
  Ox_caps.Cap_unix.mkdir ~caps config 0o700;
  Ox_caps.Cap_unix.mkdir ~caps data 0o700;
  Ox_caps.Cap_unix.mkdir ~caps runtime 0o700;
  let sentinel = Filename.concat sandbox "sentinel" in
  List.iter
    (fun name ->
      write_file ~caps (Filename.concat bin name) sentinel_script;
      Ox_caps.Cap_unix.chmod ~caps (Filename.concat bin name) 0o700)
    [ "pactl"; "pulseaudio"; "curl"; "wget"; "busctl"; "parec" ];
  f ~sentinel

let exited_nonzero = function
  | Unix.WEXITED 0 -> Alcotest.fail "expected nonzero exit, got 0"
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> Alcotest.failf "signaled %d" n
  | Unix.WSTOPPED n -> Alcotest.failf "stopped %d" n

let test_help_omits_removed_commands
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
        (name ^ " absent from --help")
        false
        (string_contains help name))
    [ "ptt-portal"; "stt-interactive" ];
  let advertised_as_command name =
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
  in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " not advertised as a command")
        false
        (advertised_as_command name))
    removed_commands;
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
      let status, stdout, _stderr = run_pie ~caps ~sandbox ~sentinel [ name ] in
      ignore (exited_nonzero status);
      Alcotest.(check bool)
        (name ^ " did not access Pulse/provider helpers")
        false (Sys.file_exists sentinel);
      Alcotest.(check bool)
        (name ^ " produced no capture output")
        true
        (stdout = "" || not (string_contains stdout "recording")))
    removed_commands

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "removed-entrypoints"
    [
      ( "cli",
        [
          Alcotest.test_case "help omits removed prototype names" `Quick
            (test_help_omits_removed_commands ~caps);
          Alcotest.test_case "removed names fail before Pulse/provider helpers"
            `Quick
            (test_removed_names_fail_before_helpers ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
