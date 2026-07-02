open Eta.Syntax

let run ~sw ~net ~clock ~output_path ~duration_sec ~raw () =
  Pulse.Client.with_client ~sw ~net ~clock @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info recv ->
  let chunks_ref = ref [] in
  let total_ref = ref 0 in
  let rec recv_loop () =
    let* chunk = recv () in
    let processed =
      if raw then chunk
      else (Audio.Pcm.normalize_pcm_for_stt chunk).normalized_bytes
    in
    let* () = Eta.Effect.sync (fun () ->
      chunks_ref := processed :: !chunks_ref;
      incr total_ref)
    in
    recv_loop ()
  in
  let timeout =
    Eta.Effect.delay (Eta.Duration.ms (int_of_float (duration_sec *. 1000.0))) Eta.Effect.unit
  in
  let* () = Eta.Effect.sync (fun () ->
    Printf.printf "Recording to %s (%.0f seconds, raw=%b)...\n%!"
      output_path duration_sec raw)
  in
  let* () = Eta.Effect.race [ timeout; recv_loop () ] in
  Eta.Effect.sync (fun () ->
    let chunks = List.rev !chunks_ref in
    let total = !total_ref in
    let data_len = List.fold_left (fun acc b -> acc + Bytes.length b) 0 chunks in
    let header = Audio.Pcm.build_pcm_wav_header ~data_length:data_len ~sample_rate:16000 in
    let oc = open_out_bin output_path in
    output_bytes oc header;
    List.iter (output_bytes oc) chunks;
    close_out oc;
    Printf.printf "Recorded %d bytes (%d chunks) → %s\n%!" data_len total output_path)
