(* Live one-pass check against OpenRouter (not part of the suite):
   live_one_pass AUDIO_FILE FORMAT PROMPT_FILE GLOSSARY_FILE [MODEL] [PROVIDER]
   prints the model's text and the duration. Needs OPENROUTER_API_KEY. *)
let caps = Ox_test.powerbox ()

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let () =
  let argc = Array.length Sys.argv in
  if argc < 5 || argc > 7 then (
    prerr_endline
      "usage: live_one_pass AUDIO_FILE wav|mp3 PROMPT_FILE GLOSSARY_FILE \
       [MODEL] [PROVIDER]";
    exit 64);
  let audio = Bytes.of_string (read_file Sys.argv.(1)) in
  let audio_format = Sys.argv.(2) in
  let prompt_template = read_file Sys.argv.(3) in
  let glossary = read_file Sys.argv.(4) in
  let model = if argc > 5 then Sys.argv.(5) else "google/gemini-3.8-flash" in
  let provider_order =
    if argc > 6 then [ Sys.argv.(6) ] else [ "Google AI Studio" ]
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let logger =
    Ox_observability.Logger.create ~service:"pie-live-one-pass" ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~reporter:
        (Ox_observability.Logger.Reporter.journal (Eio.Stdenv.stderr env))
      ()
  in
  let started = Eio.Time.now clock in
  let result =
    Pie.Stt.Correction.run ~caps ~sw ~net:(Eio.Stdenv.net env) ~clock ~logger
      ~model ~prompt_template ~transcript:Pie.Stt.Correction.audio_cue ~audio
      ~audio_format ~provider_order ~glossary ~language:"Polish"
      ~source_language:"Polish" ~target_language:"English"
      ~reasoning_effort:"low" ~timeout:(Ox.Duration.seconds 120) ()
  in
  let ms = int_of_float ((Eio.Time.now clock -. started) *. 1000.) in
  match result with
  | Ok text -> Printf.printf "ms=%d\n%s\n%!" ms text
  | Error failure ->
      Printf.eprintf "ms=%d error=%s\n%!" ms
        (Pie.Stt.Correction.diagnostic failure);
      exit 1
