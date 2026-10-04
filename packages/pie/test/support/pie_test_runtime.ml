(* ^nwake-9uvx: keep the posix tick alive through each operation's protected
   child cleanup, without changing production runtime ownership or stderr. *)
let run ~(caps : < Ox_caps.Cap.env ; .. >) operation =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          Ox_app.posix_wakeup ~caps ~sw env;
          operation env))

(* Generic owner tests inject recognition through supported two-pass dependencies. *)
let pipeline ~stt =
  let next = ref 0 in
  {
    Pie.Assistant.Dictation.resolve_glossary =
      (fun () -> ({ Pie.Glossary.text = ""; paths = []; warnings = [] }, None));
    pass1 =
      (fun config binding _ pcm ->
        stt config
          (match binding.Pie.Config.Stt.mode with
          | Transcribe -> Pie.Assistant.Service.Transcribe
          | Translate -> Pie.Assistant.Service.Translate)
          pcm);
    pass2 =
      (fun ~on_response:_ _ _ _ text -> Pie.Stt.Correction.of_result (Ok text));
    one_pass =
      (fun ~on_response:_ config binding _ pcm ->
        Pie.Stt.Correction.of_result
          (Result.map_error
             (fun _ ->
               Pie.Stt.Correction.Provider Pie.Stt.Correction.Invalid_request)
             (stt config
                (match binding.Pie.Config.Stt.mode with
                | Transcribe -> Pie.Assistant.Service.Transcribe
                | Translate -> Pie.Assistant.Service.Translate)
                pcm)));
    store_response = (fun ~id:_ ~first:_ _ -> Ok "test-response");
    append_transcribed = (fun _ -> Ok ());
    append_finished = (fun _ -> Ok ());
    warn = (fun _ -> ());
    report_notification =
      Pie.Desktop.Notification.report ~notify:(fun _ _ -> ()) ~log:(fun _ -> ());
    fresh_id =
      (fun () ->
        incr next;
        Printf.sprintf "test-%d" !next);
    now = (fun () -> 0.);
    log_pass = (fun _ ~model:_ ~duration_ms:_ -> ());
  }

module Service = struct
  include Pie.Assistant.Service

  let on_press ~caps ~sw ~net ~clock ~logger ~coordinator ~config ?store
      ?retry_dir ?pipeline:deps ?binding ?on_owner ?focused_window ~stt ~deliver
      mode =
    Pie.Assistant.Service.on_press ~caps ~sw ~net ~clock ~logger ~coordinator
      ~config ?store ?retry_dir
      ~pipeline:(Option.value deps ~default:(pipeline ~stt))
      ?binding ?on_owner ?focused_window ~stt ~deliver mode

  let run_with_inputs ~caps ~sw ~net ~fs ~clock ~logger ~process_mgr ~mono_clock
      ~inputs ~config_control ~stt ~deliver ~socket_path ~retry_dir ?config_path
      ?initial_config ?pipeline:deps ?ptt_transcribe_keysym
      ?ptt_translate_keysym ?min_duration_ms ?queue_pcm_bytes ?capture_pcm_bytes
      ?queue_capacity ?audio_dir ?history_path () =
    Pie.Assistant.Service.run_with_inputs ~caps ~sw ~net ~fs ~clock ~logger
      ~process_mgr ~mono_clock ~inputs ~config_control ~stt ~deliver
      ~socket_path ~retry_dir ?config_path ?initial_config
      ~pipeline:(Option.value deps ~default:(pipeline ~stt))
      ?ptt_transcribe_keysym ?ptt_translate_keysym ?min_duration_ms
      ?queue_pcm_bytes ?capture_pcm_bytes ?queue_capacity ?audio_dir
      ?history_path ()
end
