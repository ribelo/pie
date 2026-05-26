let ( let* ) eff f = Eta.Effect.bind f eff

let read_stdin_line ~stdin =
  Eta.Effect.sync (fun () ->
    Eio.Buf_read.parse_exn ~max_size:4096 Eio.Buf_read.line stdin)

let drain_until_interrupt ~stdin recv =
  let chunks_ref = ref [] in
  let rec recv_loop () =
    let* chunk = recv () in
    let* () = Eta.Effect.sync (fun () -> chunks_ref := chunk :: !chunks_ref) in
    recv_loop ()
  in
  let* () =
    Eta.Effect.race [
      Eta.Effect.map (fun _ -> ()) (read_stdin_line ~stdin);
      recv_loop ();
    ]
  in
  Eta.Effect.sync (fun () ->
    let data = List.rev !chunks_ref in
    let data_len = List.fold_left (fun acc b -> acc + Bytes.length b) 0 data in
    Printf.printf "Captured %d bytes in %d chunks\n%!" data_len (List.length data);
    let pcm = Bytes.create data_len in
    let offset = ref 0 in
    List.iter (fun chunk ->
      Bytes.blit chunk 0 pcm !offset (Bytes.length chunk);
      offset := !offset + Bytes.length chunk
    ) data;
    let normalized = Audio.Pcm.normalize_pcm_for_stt pcm in
    normalized.normalized_bytes)

let run ~sw ~net ~clock ~process_mgr ~stdin ~mode () =
  Pulse.Client.with_client ~sw ~net ~clock @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info recv ->
  let* () = Eta.Effect.sync (fun () ->
    Printf.printf "PTT %s mode: press Enter to start, Enter to stop (Ctrl-C to exit)...\n%!"
      (match mode with `Transcribe -> "transcribe" | `Translate -> "translate"))
  in
  let* _ = read_stdin_line ~stdin in
  let* () = Eta.Effect.sync (fun () -> Printf.printf "Recording...\n%!") in
  let* pcm_bytes = drain_until_interrupt ~stdin recv in
  let* () = Eta.Effect.sync (fun () -> Printf.printf "Transcribing...\n%!") in
  (* B8 TODO: mode (`Transcribe | `Translate) is only used in the prompt.
     Wire model / language to differ between modes, or collapse the
     ptt-transcribe / ptt-translate subcommands into one. *)
  let api_key =
    match Sys.getenv_opt "OPENROUTER_API_KEY" with
    | Some k -> k
    | None -> failwith "OPENROUTER_API_KEY not set"
  in
  let* text = Stt.Openrouter.transcribe ~process_mgr ~api_key ~model:"whisper-1" ~language:"en" pcm_bytes in
  let* result = Input.Text_injection.inject ~process_mgr text in
  Eta.Effect.sync (fun () ->
    Printf.printf "Transcript: %s\n%!" text;
    Printf.printf "Typed %d chars via %s\n%!" (String.length result.text)
      (match result.backend with | Wtype -> "wtype" | Xdotool -> "xdotool"))
