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
  Buffer.contents buffer |> String.split_on_char ' '
  |> List.filter (fun part -> part <> "")
  |> String.concat " "

let usage () =
  Printf.eprintf
    "usage: live_openrouter_translate RAW_S16LE_16K_MONO.pcm EXPECTED_TEXT \
     [MODEL]\n\
     %!";
  exit 64

let render_error = function `Stt_error message -> message

let () =
  if Array.length Sys.argv < 3 || Array.length Sys.argv > 4 then usage ();
  let pcm_path = Sys.argv.(1) in
  let expected = Sys.argv.(2) in
  let model =
    if Array.length Sys.argv = 4 then Sys.argv.(3)
    else "openrouter/xiaomi/mimo-v2.5"
  in
  let resolved_model =
    Pie.Config.Stt.resolve_model_provider
      ~default_provider:Pie.Config.Stt.Openrouter model
  in
  if resolved_model.provider <> Pie.Config.Stt.Openrouter then (
    Printf.eprintf "model must resolve to OpenRouter: %s\n%!" model;
    exit 64);
  let pcm = Bytes.of_string (read_file pcm_path) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let logger =
    Ox_observability.Logger.create ~service:"pie-live-test" ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~reporter:
        (Ox_observability.Logger.Reporter.journal (Eio.Stdenv.stderr env))
      ()
  in
  let result =
    try
      Eio.Time.with_timeout_exn clock 90.0 (fun () ->
          Pie.Stt.Openrouter.translate_with_prompt ~sw ~net:(Eio.Stdenv.net env)
            ~clock ~logger ~model:resolved_model.model
            ~source_language:"English" ~target_language:"English"
            ~prompt_template:
              "Translate the spoken audio from {{source_language}} to \
               {{target_language}}."
            ~sample_rate:16000 pcm)
    with Eio.Time.Timeout -> Error (`Stt_error "live translation timed out")
  in
  match result with
  | Ok translation ->
      let expected_normalized = normalize expected in
      let translation_normalized = normalize translation in
      let matched = String.equal expected_normalized translation_normalized in
      Printf.printf "expected=%S\n" expected_normalized;
      Printf.printf "translation=%S\n" translation;
      Printf.printf "normalized_translation=%S\n" translation_normalized;
      Printf.printf "match=%b\n%!" matched;
      if not matched then exit 2
  | Error error ->
      let message = render_error error in
      Printf.eprintf "error=%s\n%!" message;
      exit 1
