(* Isolated PIE018 configuration-boundary controls.
   Always pass ~config_path into Config.Stt.load. Private HOME/XDG/TMPDIR
   keep accidental default-path loads off the user's real config. *)

let max_config_file_bytes = 1024 * 1024
let fifo_deadline_s = 1.0

let contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec scan i =
    i <= haystack_len - needle_len
    && (String.sub haystack i needle_len = needle || scan (i + 1))
  in
  needle_len = 0 || scan 0

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

let fd_count () =
  let dir = Unix.opendir "/proc/self/fd" in
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

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let isolate_process_env () =
  let root = Filename.temp_file "pie-config-boundary-home" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let mkdir_env key rel =
    let path = Filename.concat root rel in
    Unix.mkdir path 0o700;
    Unix.putenv key path;
    path
  in
  Unix.putenv "HOME" root;
  ignore (mkdir_env "XDG_CONFIG_HOME" "config");
  ignore (mkdir_env "XDG_DATA_HOME" "data");
  let runtime = mkdir_env "XDG_RUNTIME_DIR" "runtime" in
  Unix.putenv "TMPDIR" root;
  Unix.putenv "TMP" root;
  let pie_runtime = Filename.concat runtime "pie" in
  Unix.mkdir pie_runtime 0o700;
  Unix.putenv "CAMELPIE_RUNTIME_DIR" pie_runtime

let with_sandbox f =
  let dir = Filename.temp_file "pie-config-boundary" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let write_toml dir contents =
  let path = Filename.concat dir "stt.toml" in
  write_file path contents;
  path

let ensure_prompts dir =
  let prompts = Filename.concat dir "prompts" in
  (try Unix.mkdir prompts 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  prompts

let write_valid_prompts dir =
  let prompts = ensure_prompts dir in
  write_file
    (Filename.concat prompts "transcription.md")
    "Transcribe the spoken audio in {{language}}.";
  write_file
    (Filename.concat prompts "translation.md")
    "Translate from {{source_language}} to {{target_language}}.";
  prompts

let load_ok config_path = Pie.Config.Stt.load ~config_path ()

let load_error config_path =
  match Pie.Config.Stt.load ~config_path () with
  | exception Pie.Config.Stt.Config_error msg -> msg
  | _ -> Alcotest.fail "expected Config.Stt.Config_error"

let load_with_deadline ~seconds ~config_path =
  let result : [ `Ok of Pie.Config.Stt.t | `Exn of exn ] option Atomic.t =
    Atomic.make None
  in
  let thread =
    Thread.create
      (fun () ->
        Atomic.set result
          (Some
             (try `Ok (Pie.Config.Stt.load ~config_path ())
              with exn -> `Exn exn)))
      ()
  in
  let deadline = Unix.gettimeofday () +. seconds in
  let rec wait () =
    match Atomic.get result with
    | Some (`Ok config) ->
        Thread.join thread;
        config
    | Some (`Exn exn) ->
        Thread.join thread;
        raise exn
    | None ->
        if Unix.gettimeofday () >= deadline then
          Alcotest.fail
            (Printf.sprintf "Config.Stt.load blocked longer than %.3gs" seconds)
        else (
          Unix.sleepf 0.02;
          wait ())
  in
  wait ()

let default_transcription = "Transcribe the following audio in {{language}}."

let default_translation =
  "Translate from {{source_language}} to {{target_language}}."

let test_absent_prompts_use_builtin_defaults () =
  with_sandbox (fun dir ->
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let config = load_ok config_path in
      Alcotest.(check string)
        "transcription default" default_transcription
        config.transcription_prompt;
      Alcotest.(check string)
        "translation default" default_translation config.translation_prompt)

let test_regular_prompt_content_is_used () =
  with_sandbox (fun dir ->
      let _ = write_valid_prompts dir in
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let config = load_ok config_path in
      Alcotest.(check string)
        "custom transcription" "Transcribe the spoken audio in {{language}}."
        config.transcription_prompt)

let test_symlink_to_regular_prompt_is_used () =
  with_sandbox (fun dir ->
      let prompts = ensure_prompts dir in
      let target = Filename.concat dir "transcription-target.md" in
      write_file target "Transcribe via symlink in {{language}}.";
      Unix.symlink target (Filename.concat prompts "transcription.md");
      write_file (Filename.concat prompts "translation.md") default_translation;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let config = load_ok config_path in
      Alcotest.(check string)
        "symlink transcription" "Transcribe via symlink in {{language}}."
        config.transcription_prompt)

let test_directory_prompt_is_config_error_not_default () =
  with_sandbox (fun dir ->
      let prompts = ensure_prompts dir in
      Unix.mkdir (Filename.concat prompts "transcription.md") 0o700;
      write_file (Filename.concat prompts "translation.md") default_translation;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions path or type" true
        (contains ~needle:"transcription.md" msg
        || contains ~needle:"regular" msg
        || contains ~needle:"directory" msg);
      Alcotest.(check bool)
        "does not silently default" false
        (contains ~needle:"Transcribe the following audio" msg))

let test_directory_prompt_fd_count_flat_before_gc () =
  with_sandbox (fun dir ->
      let prompts = ensure_prompts dir in
      Unix.mkdir (Filename.concat prompts "transcription.md") 0o700;
      write_file (Filename.concat prompts "translation.md") default_translation;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let before = fd_count () in
      for i = 1 to 100 do
        match Pie.Config.Stt.load ~config_path () with
        | exception Pie.Config.Stt.Config_error _ -> ()
        | _ ->
            Alcotest.fail
              (Printf.sprintf
                 "directory prompt load %d returned success instead of \
                  Config_error"
                 i)
      done;
      let after = fd_count () in
      Alcotest.(check int)
        "fd count flat before GC after 100 directory prompt failures" before
        after;
      Gc.full_major ())

let test_fifo_prompt_without_writer_is_bounded () =
  with_sandbox (fun dir ->
      let prompts = ensure_prompts dir in
      Unix.mkfifo (Filename.concat prompts "transcription.md") 0o600;
      write_file (Filename.concat prompts "translation.md") default_translation;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      match load_with_deadline ~seconds:fifo_deadline_s ~config_path with
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.(check bool)
            "fifo prompt is a failure" true
            (String.length msg > 0)
      | _ -> Alcotest.fail "FIFO prompt without writer must be Config_error")

let test_fifo_toml_without_writer_is_bounded () =
  with_sandbox (fun dir ->
      let config_path = Filename.concat dir "stt.toml" in
      Unix.mkfifo config_path 0o600;
      match load_with_deadline ~seconds:fifo_deadline_s ~config_path with
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.(check bool)
            "fifo toml is not reported as missing" false
            (contains ~needle:"not found" msg)
      | _ -> Alcotest.fail "FIFO TOML without writer must be Config_error")

let test_oversize_prompt_rejected () =
  with_sandbox (fun dir ->
      let prompts = ensure_prompts dir in
      let oversized = String.make (max_config_file_bytes + 1) 'x' in
      write_file (Filename.concat prompts "transcription.md") oversized;
      write_file (Filename.concat prompts "translation.md") default_translation;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions size limit" true
        (contains ~needle:"1MiB" msg
        || contains ~needle:"MiB" msg
        || contains ~needle:"limit" msg
        || contains ~needle:"size" msg))

let test_oversize_toml_rejected () =
  with_sandbox (fun dir ->
      let config_path = Filename.concat dir "stt.toml" in
      write_file config_path (String.make (max_config_file_bytes + 1) 'a');
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions size limit" true
        (contains ~needle:"1MiB" msg
        || contains ~needle:"MiB" msg
        || contains ~needle:"limit" msg
        || contains ~needle:"size" msg))

let toml_of_size size =
  let header = "provider = \"codex-realtime\"\n#" in
  let header_len = String.length header in
  if size < header_len then
    Alcotest.fail "requested TOML size smaller than header";
  header ^ String.make (size - header_len) 'x'

let prompt_of_size size =
  let body = "Transcribe in {{language}}.\n" in
  let body_len = String.length body in
  if size < body_len then
    Alcotest.fail "requested prompt size smaller than body";
  body ^ String.make (size - body_len) 'x'

let test_exactly_1mib_toml_accepted () =
  with_sandbox (fun dir ->
      let config_path = Filename.concat dir "stt.toml" in
      let contents = toml_of_size max_config_file_bytes in
      Alcotest.(check int)
        "toml byte length is exactly 1MiB" max_config_file_bytes
        (String.length contents);
      write_file config_path contents;
      let config = load_ok config_path in
      Alcotest.(check string)
        "1MiB TOML loads" "codex-realtime"
        (Pie.Config.Stt.string_of_provider config.provider))

let test_exactly_1mib_prompt_accepted () =
  with_sandbox (fun dir ->
      let prompts = ensure_prompts dir in
      let contents = prompt_of_size max_config_file_bytes in
      Alcotest.(check int)
        "prompt byte length is exactly 1MiB" max_config_file_bytes
        (String.length contents);
      write_file (Filename.concat prompts "transcription.md") contents;
      write_file (Filename.concat prompts "translation.md") default_translation;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let config = load_ok config_path in
      Alcotest.(check int)
        "1MiB prompt is kept" max_config_file_bytes
        (String.length config.transcription_prompt))

let test_unreadable_prompt_is_config_error_not_default () =
  with_sandbox (fun dir ->
      let prompts = write_valid_prompts dir in
      let transcription = Filename.concat prompts "transcription.md" in
      Unix.chmod transcription 0o000;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      Fun.protect
        ~finally:(fun () ->
          try Unix.chmod transcription 0o600 with Unix.Unix_error _ -> ())
        (fun () ->
          let msg = load_error config_path in
          Alcotest.(check bool)
            "unreadable existing prompt is a failure" true
            (String.length msg > 0)))

let test_empty_existing_prompt_is_not_absence () =
  with_sandbox (fun dir ->
      let prompts = ensure_prompts dir in
      write_file (Filename.concat prompts "transcription.md") "";
      write_file (Filename.concat prompts "translation.md") default_translation;
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "empty prompt is invalid" true
        (contains ~needle:"empty" msg || contains ~needle:"prompt" msg))

let test_missing_toml_is_not_found () =
  with_sandbox (fun dir ->
      let config_path = Filename.concat dir "stt.toml" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "reports not found" true
        (contains ~needle:"not found" msg))

let test_toml_directory_is_not_missing () =
  with_sandbox (fun dir ->
      let config_path = Filename.concat dir "stt.toml" in
      Unix.mkdir config_path 0o700;
      let msg = load_error config_path in
      Alcotest.(check bool)
        "directory config is not 'not found'" false
        (contains ~needle:"not found" msg))

let test_rms_zero_disables_gate () =
  with_sandbox (fun dir ->
      let config_path =
        write_toml dir "ptt_min_capture_rms = 0.0\nptt_min_capture_peak = 0.0\n"
      in
      let config = load_ok config_path in
      Alcotest.(check (float 0.0)) "rms 0" 0.0 config.ptt_min_capture_rms;
      Alcotest.(check (float 0.0)) "peak 0" 0.0 config.ptt_min_capture_peak)

let test_rms_one_is_accepted () =
  with_sandbox (fun dir ->
      let config_path =
        write_toml dir "ptt_min_capture_rms = 1.0\nptt_min_capture_peak = 1.0\n"
      in
      let config = load_ok config_path in
      Alcotest.(check (float 0.0)) "rms 1" 1.0 config.ptt_min_capture_rms;
      Alcotest.(check (float 0.0)) "peak 1" 1.0 config.ptt_min_capture_peak)

let test_rms_negative_rejected () =
  with_sandbox (fun dir ->
      let config_path = write_toml dir "ptt_min_capture_rms = -1.0\n" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions rms" true
        (contains ~needle:"ptt_min_capture_rms" msg))

let test_peak_above_one_rejected () =
  with_sandbox (fun dir ->
      let config_path = write_toml dir "ptt_min_capture_peak = 2.0\n" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions peak" true
        (contains ~needle:"ptt_min_capture_peak" msg))

let test_rms_overflow_rejected () =
  with_sandbox (fun dir ->
      let config_path = write_toml dir "ptt_min_capture_rms = 1e999\n" in
      match Pie.Config.Stt.load ~config_path () with
      | exception Pie.Config.Stt.Config_error _ -> ()
      | config ->
          if
            Float.is_finite config.ptt_min_capture_rms
            && config.ptt_min_capture_rms >= 0.
            && config.ptt_min_capture_rms <= 1.
          then Alcotest.fail "overflow RMS was accepted as a normalized value"
          else Alcotest.fail "overflow RMS must raise Config_error")

let test_rms_wrong_type_rejected () =
  with_sandbox (fun dir ->
      let config_path = write_toml dir "ptt_min_capture_rms = \"high\"\n" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions type or field" true
        (contains ~needle:"ptt_min_capture_rms" msg
        || contains ~needle:"number" msg))

let test_peak_wrong_type_rejected () =
  with_sandbox (fun dir ->
      let config_path = write_toml dir "ptt_min_capture_peak = true\n" in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions type or field" true
        (contains ~needle:"ptt_min_capture_peak" msg
        || contains ~needle:"number" msg))

let test_duplicate_bindings_rejected () =
  with_sandbox (fun dir ->
      let config_path =
        write_toml dir
          "ptt_transcribe_keysym = 10\n\
           ptt_translate_keysym = 10\n\
           ptt_transcribe_shift = false\n\
           ptt_translate_shift = false\n"
      in
      let msg = load_error config_path in
      Alcotest.(check bool)
        "mentions bindings" true
        (contains ~needle:"differ" msg || contains ~needle:"binding" msg))

let test_valid_regular_toml_load_fd_count_flat () =
  with_sandbox (fun dir ->
      let _ = write_valid_prompts dir in
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let before = fd_count () in
      for _ = 1 to 100 do
        ignore (load_ok config_path)
      done;
      let after = fd_count () in
      Alcotest.(check int) "successful loads leave fd count flat" before after)

let unix_io = Pie.Config.Stt.unix_read_io

let with_io ?(after_acquire = unix_io.after_acquire)
    ?(after_fstat = unix_io.after_fstat) ?(read = unix_io.read)
    ?(close = unix_io.close) () : Pie.Config.Stt.read_io =
  { unix_io with after_acquire; after_fstat; read; close }

let load_io ~config_path read_io = Pie.Config.Stt.load ~config_path ~read_io ()

exception Test_cancel

let test_midread_eio_fd_count_flat () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let reads = Atomic.make 0 in
      let read fd buf off len =
        let n = Atomic.fetch_and_add reads 1 in
        if n = 0 then unix_io.read fd buf off (min 1 len)
        else raise (Unix.Unix_error (Unix.EIO, "read", config_path))
      in
      let read_io = with_io ~read () in
      let before = fd_count () in
      for _ = 1 to 100 do
        Atomic.set reads 0;
        match load_io ~config_path read_io with
        | exception Pie.Config.Stt.Config_error msg ->
            Alcotest.(check bool)
              "mid-read EIO is Config_error" true
              (contains ~needle:"Input/output error" msg
              || contains ~needle:"I/O" msg
              || contains ~needle:"failed to read" msg)
        | _ -> Alcotest.fail "expected Config_error on mid-read EIO"
      done;
      let after = fd_count () in
      Alcotest.(check int) "mid-read EIO leaves fd count flat" before after)

let test_path_swap_to_fifo_while_fd_held () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let after_acquire _fd =
        Unix.unlink config_path;
        Unix.mkfifo config_path 0o600
      in
      let read_io = with_io ~after_acquire () in
      let config = load_io ~config_path read_io in
      Alcotest.(check string)
        "still loaded original regular fd" "codex-realtime"
        (Pie.Config.Stt.string_of_provider config.provider))

let test_path_swap_to_dir_while_fd_held () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let after_acquire _fd =
        match Unix.lstat config_path with
        | { Unix.st_kind = Unix.S_REG; _ } ->
            Unix.unlink config_path;
            Unix.mkdir config_path 0o700
        | _ -> ()
      in
      let read_io = with_io ~after_acquire () in
      let config = load_io ~config_path read_io in
      Alcotest.(check string)
        "still loaded original regular fd" "codex-realtime"
        (Pie.Config.Stt.string_of_provider config.provider))

let test_growing_file_hits_cap () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let after_fstat _fd _stats =
        let oc =
          open_out_gen
            [ Open_wronly; Open_append; Open_binary ]
            0o600 config_path
        in
        Fun.protect
          ~finally:(fun () -> close_out_noerr oc)
          (fun () ->
            output_string oc (String.make (max_config_file_bytes + 1) 'x'))
      in
      let read_io = with_io ~after_fstat () in
      match load_io ~config_path read_io with
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.(check bool)
            "growing file mentions 1MiB" true
            (contains ~needle:"1MiB" msg)
      | _ -> Alcotest.fail "expected Config_error for growing file")

let test_close_error_after_success () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let closes = Atomic.make 0 in
      let close fd =
        let n = Atomic.fetch_and_add closes 1 in
        Unix.close fd;
        if n = 0 then raise (Unix.Unix_error (Unix.EIO, "close", config_path))
        else Alcotest.fail "close retried after Linux error"
      in
      let read_io = with_io ~close () in
      match load_io ~config_path read_io with
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.(check int) "close once" 1 (Atomic.get closes);
          Alcotest.(check bool)
            "mentions close" true
            (contains ~needle:"close" msg)
      | _ -> Alcotest.fail "expected Config_error on close failure")

let test_read_and_close_errors_both_preserved () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let closes = Atomic.make 0 in
      let reads = Atomic.make 0 in
      let read fd buf off len =
        let n = Atomic.fetch_and_add reads 1 in
        if n = 0 then unix_io.read fd buf off (min 1 len)
        else raise (Unix.Unix_error (Unix.EIO, "read", config_path))
      in
      let close fd =
        let n = Atomic.fetch_and_add closes 1 in
        Unix.close fd;
        if n = 0 then raise (Unix.Unix_error (Unix.EIO, "close", config_path))
        else Alcotest.fail "close retried after Linux error"
      in
      let read_io = with_io ~read ~close () in
      match load_io ~config_path read_io with
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.(check int) "close once" 1 (Atomic.get closes);
          Alcotest.(check bool)
            "mentions read" true
            (contains ~needle:"failed to read" msg);
          Alcotest.(check bool)
            "mentions close" true
            (contains ~needle:"close" msg)
      | _ -> Alcotest.fail "expected combined Config_error")

let dummy_audio () =
  let pending = Eio.Stream.create 1 in
  let rec source () =
    Eta_stream.Stream.from_effect
      (Eta.Effect.sync (fun () -> Eio.Stream.take pending))
    |> Eta_stream.Stream.flat_map (fun chunk ->
        Eta_stream.Stream.concat (Eta_stream.Stream.succeed chunk) (source ()))
  in
  source ()

let test_service_reload_cancel_settles_after_barrier () =
  Eio_main.run @@ fun env ->
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let entered = Atomic.make false in
      let release = Atomic.make false in
      let load_returned = Atomic.make false in
      let fd_closed = Atomic.make false in
      let loads = Atomic.make 0 in
      let stamp_state = Atomic.make [ Some (1, 1, 1, 1.0) ] in
      let barrier_io =
        with_io
          ~after_acquire:(fun _fd ->
            Atomic.set entered true;
            while not (Atomic.get release) do
              Thread.yield ()
            done)
          ~close:(fun fd ->
            Unix.close fd;
            Atomic.set fd_closed true)
          ()
      in
      let load _attempts =
        let n = Atomic.fetch_and_add loads 1 in
        let read_io = if n = 0 then unix_io else barrier_io in
        match Pie.Config.Stt.load ~config_path ~read_io () with
        | config ->
            if n > 0 then Atomic.set load_returned true;
            (config, Atomic.get stamp_state)
        | exception exn ->
            if n > 0 then Atomic.set load_returned true;
            raise exn
      in
      let stamp () = Atomic.get stamp_state in
      let config_control = { Pie.Assistant.Service.stamp; load } in
      Eio.Switch.run @@ fun outer ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let socket_path = Filename.concat dir "control.sock" in
      let retry_dir = Filename.concat dir "recordings" in
      Unix.mkdir retry_dir 0o700;
      let inputs =
        {
          Pie.Assistant.Service.keyboard;
          audio = dummy_audio ();
          source_name = Some "test-source";
        }
      in
      let workflow =
        Pie.Assistant.Service.run_with_inputs ~sw:outer
          ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~inputs ~config_control
          ~stt:(fun _ _ _ -> Eta.Effect.pure "ok")
          ~deliver:(fun _ _ _ -> Eta.Effect.unit)
          ~socket_path ~retry_dir ~min_duration_ms:0 ()
      in
      Eio.Fiber.fork ~sw:outer (fun () ->
          let rec wait n =
            if Atomic.get entered then ()
            else if n <= 0 then ()
            else (
              Eio.Time.sleep clock 0.05;
              wait (n - 1))
          in
          wait 40;
          Eio.Time.sleep clock 0.1;
          Atomic.set release true);
      try
        Eio.Time.with_timeout_exn clock 7.0 (fun () ->
            match
              Eta_eio.run_once ~sw:outer ~clock
                (Eta.Effect.with_background ~name:"pie.config-boundary.reload"
                   workflow (fun () ->
                     let rec wait pred fail_msg n =
                       if pred () then Eta.Effect.unit
                       else if n <= 0 then
                         Eta.Effect.sync (fun () -> Alcotest.fail fail_msg)
                       else
                         Eta.Effect.sync (fun () -> Eio.Time.sleep clock 0.05)
                         |> Eta.Effect.bind (fun () ->
                             wait pred fail_msg (n - 1))
                     in
                     wait
                       (fun () -> Atomic.get loads > 0)
                       "initial load missing" 40
                     |> Eta.Effect.bind (fun () ->
                         Eta.Effect.sync (fun () ->
                             Atomic.set stamp_state [ Some (2, 2, 2, 2.0) ]))
                     |> Eta.Effect.bind (fun () ->
                         wait
                           (fun () -> Atomic.get entered)
                           "reload never entered after_acquire" 40)
                     |> Eta.Effect.bind (fun () ->
                         Eta.Effect.sync (fun () ->
                             Alcotest.(check bool)
                               "not settled while reload thread held" false
                               (Atomic.get load_returned);
                             Alcotest.(check bool)
                               "fd still held" false (Atomic.get fd_closed)))))
            with
            | Ok () ->
                Alcotest.(check bool)
                  "loader returned" true (Atomic.get load_returned);
                Alcotest.(check bool) "fd closed" true (Atomic.get fd_closed)
            | Error _ -> Alcotest.fail "unexpected workflow error")
      with Eio.Time.Timeout -> Alcotest.fail "7s deadline")

let test_cancel_during_acquire_not_config_error () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let closed = Atomic.make 0 in
      let close fd =
        Atomic.incr closed;
        Unix.close fd
      in
      let after_acquire _fd = raise Test_cancel in
      let read_io = with_io ~after_acquire ~close () in
      match load_io ~config_path read_io with
      | exception Test_cancel ->
          Alcotest.(check int) "fd closed on cancel" 1 (Atomic.get closed)
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.fail ("cancel wrapped as Config_error: " ^ msg)
      | _ -> Alcotest.fail "expected Test_cancel to propagate")

exception Close_defect

let test_cancel_plus_close_eio_preserves_both () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let closes = Atomic.make 0 in
      let close fd =
        let n = Atomic.fetch_and_add closes 1 in
        Unix.close fd;
        if n = 0 then raise (Unix.Unix_error (Unix.EIO, "close", config_path))
        else Alcotest.fail "close retried after Linux error"
      in
      let after_acquire _fd = raise Test_cancel in
      let read_io = with_io ~after_acquire ~close () in
      match load_io ~config_path read_io with
      | exception
          Pie.Config.Stt.Compound (Test_cancel, Unix.Unix_error (Unix.EIO, _, _))
        ->
          Alcotest.(check int) "close once" 1 (Atomic.get closes)
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.fail ("cancel+close wrapped as Config_error: " ^ msg)
      | exception Test_cancel -> Alcotest.fail "close Unix error dropped"
      | exception exn -> Alcotest.fail ("unexpected: " ^ Printexc.to_string exn)
      | _ -> Alcotest.fail "expected Compound (Test_cancel, close EIO)")

let test_config_error_plus_close_defect_preserves_both () =
  with_sandbox (fun dir ->
      ignore (write_valid_prompts dir);
      let config_path = write_toml dir "provider = \"codex-realtime\"\n" in
      let closes = Atomic.make 0 in
      let read _fd _buf _off _len =
        raise (Unix.Unix_error (Unix.EIO, "read", config_path))
      in
      let close fd =
        let n = Atomic.fetch_and_add closes 1 in
        Unix.close fd;
        if n = 0 then raise Close_defect
        else Alcotest.fail "close retried after defect"
      in
      let read_io = with_io ~read ~close () in
      match load_io ~config_path read_io with
      | exception
          Pie.Config.Stt.Compound (Pie.Config.Stt.Config_error msg, Close_defect)
        ->
          Alcotest.(check int) "close once" 1 (Atomic.get closes);
          Alcotest.(check bool)
            "primary is read Config_error" true
            (contains ~needle:"failed to read" msg)
      | exception Pie.Config.Stt.Config_error msg ->
          Alcotest.fail ("close defect dropped: " ^ msg)
      | exception Close_defect -> Alcotest.fail "primary Config_error dropped"
      | exception exn -> Alcotest.fail ("unexpected: " ^ Printexc.to_string exn)
      | _ -> Alcotest.fail "expected Compound (Config_error, Close_defect)")

let () =
  isolate_process_env ();
  Alcotest.run "config-boundary"
    [
      ( "prompts",
        [
          Alcotest.test_case "absent prompts use built-in defaults" `Quick
            test_absent_prompts_use_builtin_defaults;
          Alcotest.test_case "regular prompt content is used" `Quick
            test_regular_prompt_content_is_used;
          Alcotest.test_case "symlink to regular prompt is used" `Quick
            test_symlink_to_regular_prompt_is_used;
          Alcotest.test_case "directory prompt is Config_error not default"
            `Quick test_directory_prompt_is_config_error_not_default;
          Alcotest.test_case "directory prompt fd count flat before GC over 100"
            `Quick test_directory_prompt_fd_count_flat_before_gc;
          Alcotest.test_case "FIFO prompt without writer is bounded" `Quick
            test_fifo_prompt_without_writer_is_bounded;
          Alcotest.test_case "empty existing prompt is not absence" `Quick
            test_empty_existing_prompt_is_not_absence;
          Alcotest.test_case "oversize prompt is rejected" `Quick
            test_oversize_prompt_rejected;
          Alcotest.test_case "exactly 1MiB prompt is accepted" `Quick
            test_exactly_1mib_prompt_accepted;
          Alcotest.test_case "unreadable prompt is Config_error not default"
            `Quick test_unreadable_prompt_is_config_error_not_default;
        ] );
      ( "toml",
        [
          Alcotest.test_case "missing TOML is not found" `Quick
            test_missing_toml_is_not_found;
          Alcotest.test_case "directory TOML is not missing" `Quick
            test_toml_directory_is_not_missing;
          Alcotest.test_case "FIFO TOML without writer is bounded" `Quick
            test_fifo_toml_without_writer_is_bounded;
          Alcotest.test_case "oversize TOML is rejected" `Quick
            test_oversize_toml_rejected;
          Alcotest.test_case "exactly 1MiB TOML is accepted" `Quick
            test_exactly_1mib_toml_accepted;
          Alcotest.test_case "successful loads leave fd count flat" `Quick
            test_valid_regular_toml_load_fd_count_flat;
        ] );
      ( "thresholds",
        [
          Alcotest.test_case "RMS/peak 0 disables gate" `Quick
            test_rms_zero_disables_gate;
          Alcotest.test_case "RMS/peak 1 is accepted" `Quick
            test_rms_one_is_accepted;
          Alcotest.test_case "negative RMS is rejected" `Quick
            test_rms_negative_rejected;
          Alcotest.test_case "peak above 1 is rejected" `Quick
            test_peak_above_one_rejected;
          Alcotest.test_case "overflow RMS is rejected" `Quick
            test_rms_overflow_rejected;
          Alcotest.test_case "wrong-type RMS is rejected" `Quick
            test_rms_wrong_type_rejected;
          Alcotest.test_case "wrong-type peak is rejected" `Quick
            test_peak_wrong_type_rejected;
          Alcotest.test_case "duplicate key bindings are rejected" `Quick
            test_duplicate_bindings_rejected;
        ] );
      ( "reader-races",
        [
          Alcotest.test_case "mid-read EIO fd count flat before GC" `Quick
            test_midread_eio_fd_count_flat;
          Alcotest.test_case "path swap to FIFO while fd held still loads"
            `Quick test_path_swap_to_fifo_while_fd_held;
          Alcotest.test_case "path swap to dir while fd held still loads" `Quick
            test_path_swap_to_dir_while_fd_held;
          Alcotest.test_case "growing file after fstat hits 1MiB cap" `Quick
            test_growing_file_hits_cap;
          Alcotest.test_case "close error after success is Config_error once"
            `Quick test_close_error_after_success;
          Alcotest.test_case "read and close errors both preserved" `Quick
            test_read_and_close_errors_both_preserved;
          Alcotest.test_case "cancel during acquire is not Config_error" `Quick
            test_cancel_during_acquire_not_config_error;
          Alcotest.test_case "Test_cancel plus close EIO preserves both" `Quick
            test_cancel_plus_close_eio_preserves_both;
          Alcotest.test_case "Config_error plus close defect preserves both"
            `Quick test_config_error_plus_close_defect_preserves_both;
          Alcotest.test_case
            "service reload cancel settles after barrier release" `Slow
            test_service_reload_cancel_settles_after_barrier;
        ] );
    ]
