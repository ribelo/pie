let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let normalize value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun c ->
       let c = Char.lowercase_ascii c in
       if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
         Buffer.add_char buffer c
       else if c = ' ' || c = '\n' || c = '\r' || c = '\t' then
         Buffer.add_char buffer ' ')
    value;
  Buffer.contents buffer
  |> String.split_on_char ' '
  |> List.filter (fun part -> part <> "")
  |> String.concat " "

let usage () =
  Printf.eprintf
    "usage: live_openrouter_transcribe RAW_S16LE_16K_MONO.pcm EXPECTED_TEXT [MODEL]\n%!";
  exit 64

let render_error = function
  | `Stt_error message -> message

let () =
  if Array.length Sys.argv < 3 || Array.length Sys.argv > 4 then usage ();
  let pcm_path = Sys.argv.(1) in
  let expected = Sys.argv.(2) in
  let model =
    if Array.length Sys.argv = 4 then Sys.argv.(3)
    else "openrouter/xiaomi/mimo-v2.5"
  in
  let resolved_model =
    Camelpie.Config.Stt.resolve_model_provider
      ~default_provider:Camelpie.Config.Stt.Openrouter
      model
  in
  if resolved_model.provider <> Camelpie.Config.Stt.Openrouter then (
    Printf.eprintf "model must resolve to OpenRouter: %s\n%!" model;
    exit 64
  );
  let pcm = Bytes.of_string (read_file pcm_path) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let rt =
    Eta_eio.Runtime.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~capture_backtrace:true
      ()
  in
  let effect =
    Camelpie.Stt.Openrouter.transcribe_with_prompt
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~model:resolved_model.model
      ~language:"English"
      ~prompt_template:"Transcribe the spoken English words."
      ~sample_rate:16000
      pcm
    |> Eta.Effect.timeout_as
         (Eta.Duration.seconds 90)
         ~on_timeout:(`Stt_error "live OpenRouter transcription timed out")
  in
  match Eta_eio.Runtime.run rt effect with
  | Ok transcript ->
    let expected_normalized = normalize expected in
    let transcript_normalized = normalize transcript in
    let matched = String.equal expected_normalized transcript_normalized in
    Printf.printf "expected=%S\n" expected_normalized;
    Printf.printf "transcript=%S\n" transcript;
    Printf.printf "normalized_transcript=%S\n" transcript_normalized;
    Printf.printf "match=%b\n%!" matched;
    if not matched then exit 2
  | Error cause ->
    let message =
      Format.asprintf
        "%a"
        (Eta.Cause.pp (fun fmt error ->
           Format.pp_print_string fmt (render_error error)))
        cause
    in
    Printf.eprintf "error=%s\n%!" message;
    exit 1
