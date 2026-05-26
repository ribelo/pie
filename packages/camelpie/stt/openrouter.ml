type error = [`Stt_error of string]

let ( let* ) eff f = Eta.Effect.bind f eff

let api_url = "https://openrouter.ai/api/v1/audio/transcriptions"

let transcribe_file_impl ~process_mgr ~api_key ~model ~language file_path =
  let args = [
    "-s"; "-X"; "POST"; api_url;
    "-H"; "Authorization: Bearer " ^ api_key;
    "-F"; "file=@" ^ file_path;
    "-F"; "model=" ^ model;
    "-F"; "language=" ^ language;
    "-F"; "response_format=json"
  ] in
  let* raw = Sh.run_capture ~process_mgr ~prog:"curl" ~args in
  Eta.Effect.sync (fun () ->
    let json = Yojson.Safe.from_string raw in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "text" fields with
       | Some (`String text) -> Ok text
       | _ ->
         (match List.assoc_opt "error" fields with
          | Some (`Assoc err_fields) ->
            let msg = match List.assoc_opt "message" err_fields with
              | Some (`String m) -> m
              | _ -> "unknown OpenRouter error"
            in
            Error (`Stt_error msg)
          | _ -> Error (`Stt_error "unexpected response format")))
    | _ -> Error (`Stt_error "unexpected response format"))
  |> Eta.Effect.bind (function
    | Ok text -> Eta.Effect.pure text
    | Error e -> Eta.Effect.fail e)

let transcribe ~process_mgr ~api_key ~model ~language pcm_bytes =
  let* tmp_wav =
    Eta.Effect.sync (fun () ->
      let tmp = Filename.temp_file "pie_stt" ".wav" in
      let header = Audio.Pcm.build_pcm_wav_header ~data_length:(Bytes.length pcm_bytes) ~sample_rate:16000 in
      let oc = open_out_bin tmp in
      output_bytes oc header;
      output_bytes oc pcm_bytes;
      close_out oc;
      tmp)
  in
  let* text =
    transcribe_file_impl ~process_mgr ~api_key ~model ~language tmp_wav
    |> Eta.Effect.catch (fun e ->
      Eta.Effect.sync (fun () -> Sys.remove tmp_wav)
      |> Eta.Effect.bind (fun () -> Eta.Effect.fail e))
  in
  let* () = Eta.Effect.sync (fun () -> Sys.remove tmp_wav) in
  Eta.Effect.pure text
