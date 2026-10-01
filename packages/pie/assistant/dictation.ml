let ( let* ) = Result.bind

type pass = Pass1 | Pass2 | One_pass

type dependencies = {
  resolve_glossary : unit -> Glossary.t * History.window option;
  pass1 :
    Config.Stt.t ->
    Config.Stt.binding ->
    Glossary.t ->
    bytes ->
    (string, Ptt_loop.error) result;
  pass2 :
    Config.Stt.t ->
    Config.Stt.binding ->
    Glossary.t ->
    string ->
    (string, Stt.Correction.failure) result;
  one_pass :
    Config.Stt.t ->
    Config.Stt.binding ->
    Glossary.t ->
    bytes ->
    (string, Stt.Correction.failure) result;
  append_transcribed :
    History.transcribed -> (unit, [ `History of string ]) result;
  append_finished : History.finished -> (unit, [ `History of string ]) result;
  warn : string -> unit;
  notify_correction_failure : Stt.Correction.failure -> unit;
  notify_request_failure : Stt.Correction.failure -> unit;
  fresh_id : unit -> string;
  now : unit -> float;
  log_pass : pass -> model:string -> duration_ms:int -> unit;
}

type t = {
  deps : dependencies;
  config : Config.Stt.t;
  binding : Config.Stt.binding;
  glossary : Glossary.t;
  window : History.window option;
  id : string;
  mutable pass1_started : float option;
  mutable pass2_duration : int option;
  mutable finished : bool;
  mutable audio_path : string option;
  mutable one_pass_failure : Stt.Correction.failure option;
}

let glossary t = t.glossary
let binding t = t.binding
let window t = t.window
let id t = t.id

let note_audio_path t path =
  if t.audio_path = None then t.audio_path <- Some path

let create deps ~config ~binding =
  let glossary, window = deps.resolve_glossary () in
  List.iter
    (fun warning ->
      deps.warn
        (match warning with
        | Glossary.Unreadable { path; message } -> path ^ ": " ^ message
        | Focus_unavailable message -> message))
    glossary.warnings;
  {
    deps;
    config;
    binding;
    glossary;
    window;
    id = deps.fresh_id ();
    pass1_started = None;
    pass2_duration = None;
    finished = false;
    audio_path = None;
    one_pass_failure = None;
  }

let begin_pass1 t = t.pass1_started <- Some (t.deps.now ())
let one_pass t = t.config.pipeline = Config.Stt.One_pass

let attempt_model t =
  match t.config.pipeline with
  | One_pass -> (
      match t.binding.mode with
      | Config.Stt.Transcribe -> t.config.transcription_model
      | Translate -> t.config.translation_model)
  | Legacy | Two_pass -> t.config.transcription_model

let pass2_requested t =
  t.config.pipeline = Config.Stt.Two_pass && t.binding.correct

let recognize t pcm =
  if t.pass1_started = None then begin_pass1 t;
  if not (one_pass t) then t.deps.pass1 t.config t.binding t.glossary pcm
  else
    match t.deps.one_pass t.config t.binding t.glossary pcm with
    | Ok text -> Ok text
    | Error failure ->
        t.one_pass_failure <- Some failure;
        Error (`Ptt_error ("one-pass:" ^ Stt.Correction.diagnostic failure))

let duration t started =
  max 0 (int_of_float ((t.deps.now () -. started) *. 1000.))

let blank text = String.trim text = ""

let history_warning t = function
  | Ok () -> ()
  | Error (`History message) -> t.deps.warn ("history write failed: " ^ message)

let finish t ?final_text ?diagnostic ?delivery outcome =
  if not t.finished then (
    t.finished <- true;
    let pass2_model =
      if pass2_requested t then
        Option.map
          (fun (c : Config.Stt.correction) -> c.model)
          t.config.correction
      else None
    in
    history_warning t
      (t.deps.append_finished
         {
           History.id = t.id;
           time = t.deps.now ();
           final_text;
           pass2_model;
           pass2_duration_ms = t.pass2_duration;
           outcome;
           delivery;
           diagnostic;
           audio_path = t.audio_path;
         }))

let fail_unfinished t = finish t ~diagnostic:"stt:attempt-failed" History.Failed

let with_publication error result =
  match result with
  | Ok () -> Error error
  | Error finalizer ->
      let cause = function
        | `Ptt_error _ as e -> Ox.Cause.Fail e
        | `Ptt_cause cause -> cause
      in
      Error
        (`Ptt_cause
           (Ox.Cause.Suppressed
              { primary = cause error; finalizer = cause finalizer }))

let run t ~recognize ~deliver ~on_audio_failure ~on_text_failure ~on_success =
  if t.pass1_started = None then begin_pass1 t;
  let raw = recognize () in
  let pass1_duration_ms = duration t (Option.get t.pass1_started) in
  t.deps.log_pass
    (if one_pass t then One_pass else Pass1)
    ~model:(attempt_model t) ~duration_ms:pass1_duration_ms;
  let fail_audio error diagnostic =
    finish t ~diagnostic History.Failed;
    with_publication error (on_audio_failure ())
  in
  let fail_one_pass error failure =
    t.deps.notify_request_failure failure;
    fail_audio error (Stt.Correction.diagnostic failure)
  in
  match raw with
  | Error error when one_pass t ->
      let failure =
        Option.value t.one_pass_failure
          ~default:(Stt.Correction.Provider Stt.Correction.Invalid_request)
      in
      fail_one_pass error failure
  | Error error -> fail_audio error "stt:pass1-failed"
  | Ok raw when blank raw && one_pass t ->
      fail_one_pass (`Ptt_error "stt:no_transcript") Stt.Correction.Blank
  | Ok raw when blank raw ->
      fail_audio (`Ptt_error "stt:no_transcript") "stt:no_transcript"
  | Ok raw
    when one_pass t && String.length raw > Dictation_limits.result_utf8_bytes ->
      fail_one_pass (`Ptt_error "stt:result_budget") Stt.Correction.Over_budget
  | Ok raw when String.length raw > Dictation_limits.result_utf8_bytes ->
      fail_audio (`Ptt_error "stt:result_budget") "stt:result_budget"
  | Ok raw -> (
      history_warning t
        (t.deps.append_transcribed
           {
             History.id = t.id;
             time = t.deps.now ();
             mode =
               (match t.binding.mode with
               | Config.Stt.Transcribe -> History.Transcribe
               | Translate -> History.Translate);
             binding =
               History.describe_binding ~keysym:t.binding.keysym
                 ~shift:t.binding.shift ~pass2:(pass2_requested t);
             window = t.window;
             glossary_paths = t.glossary.paths;
             pass1_model = attempt_model t;
             pass1_duration_ms;
             raw_text = raw;
           });
      let corrected =
        if not (pass2_requested t) then Ok raw
        else
          let started = t.deps.now () in
          let result = t.deps.pass2 t.config t.binding t.glossary raw in
          let duration_ms = duration t started in
          t.pass2_duration <- Some duration_ms;
          let model =
            match t.config.correction with Some c -> c.model | None -> ""
          in
          t.deps.log_pass Pass2 ~model ~duration_ms;
          match result with
          | Ok text when blank text -> Error Stt.Correction.Blank
          | Ok text when String.length text > Dictation_limits.result_utf8_bytes
            ->
              Error Stt.Correction.Over_budget
          | Ok text -> Ok (String.trim text)
          | Error _ as error -> error
      in
      let deliver_final text outcome diagnostic =
        match deliver text with
        | Ok delivery ->
            let result = on_success () in
            finish t ~final_text:text ?diagnostic ~delivery outcome;
            result
        | Error error ->
            let result = on_text_failure text ~dispatch_uncertain:true in
            finish t ~final_text:text ~diagnostic:"delivery:failed"
              History.Delivery_failed;
            with_publication error result
      in
      match corrected with
      | Ok text -> deliver_final text History.Delivered None
      | Error failure -> (
          let diagnostic = Stt.Correction.diagnostic failure in
          t.deps.notify_correction_failure failure;
          match t.binding.mode with
          | Config.Stt.Transcribe ->
              deliver_final raw History.Delivered_raw_after_correction_failure
                (Some diagnostic)
          | Translate ->
              let result = on_text_failure raw ~dispatch_uncertain:false in
              finish t ~diagnostic History.Correction_failed;
              with_publication (`Ptt_error ("correction:" ^ diagnostic)) result)
      )
