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
    if data_len > 0 then begin
      let pcm = Bytes.create data_len in
      let offset = ref 0 in
      List.iter (fun chunk ->
        Bytes.blit chunk 0 pcm !offset (Bytes.length chunk);
        offset := !offset + Bytes.length chunk
      ) data;
      let normalized = Audio.Pcm.normalize_pcm_for_stt pcm in
      normalized.normalized_bytes
    end else
      Bytes.empty)

let rec loop ~process_mgr ~stdin client recv =
  let* () = Eta.Effect.sync (fun () ->
    Printf.printf "\nPress Enter to record (Ctrl-C to exit)...\n%!")
  in
  let* _ = read_stdin_line ~stdin in
  let* () = Eta.Effect.sync (fun () -> Printf.printf "Recording... (Enter to stop)\n%!") in
  let* pcm_bytes = drain_until_interrupt ~stdin recv in
  let data_len = Bytes.length pcm_bytes in
  if data_len > 0 then
    let api_key =
      match Sys.getenv_opt "OPENROUTER_API_KEY" with
      | Some k -> k
      | None -> failwith "OPENROUTER_API_KEY not set"
    in
    let* text = Stt.Openrouter.transcribe ~process_mgr ~api_key ~model:"whisper-1" ~language:"en" pcm_bytes in
    let* result = Input.Text_injection.inject ~process_mgr text in
    let* () = Eta.Effect.sync (fun () ->
      Printf.printf "Transcript: %s\n%!" text;
      if String.length (String.trim text) > 0 then
        Printf.printf "Typed %d chars via %s (%s)\n%!"
          (String.length result.text)
          (match result.backend with | Wtype -> "wtype" | Xdotool -> "xdotool")
          (match result.session_type with | Wayland -> "Wayland" | X11 -> "X11"))
    in
    loop ~process_mgr ~stdin client recv
  else
    loop ~process_mgr ~stdin client recv

let run ~sw ~net ~clock ~process_mgr ~stdin () =
  Pulse.Client.with_client ~sw ~net ~clock @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info recv ->
  let* () = Eta.Effect.sync (fun () ->
    Printf.printf
      "pie assistant mode (PTT only — wakeword requires ONNX runtime)\n\
       Press Enter to start recording, Enter to stop...\n%!")
  in
  loop ~process_mgr ~stdin client recv
