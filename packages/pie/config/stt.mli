(** Speech-to-text configuration.

    [One_pass], the default when [pipeline] is absent, sends the recording, the
    binding's prompt and the glossary to one OpenRouter audio model. [Two_pass]
    is pass 1 then optional pass 2. [streaming = true] is rejected with
    [One_pass]. One-pass models must use the [openrouter] provider.

    [[bindings]] is authoritative when present. Without it, [bindings] is the
    two keys derived from [ptt_transcribe_keysym] / [ptt_transcribe_shift] and
    [ptt_translate_keysym] / [ptt_translate_shift]. With it, those PTT fields
    are only a compatibility view: the first binding of each mode, or the
    built-in default (65478 or 65479, shift false) when that mode is absent.
    Those defaults are not extra bindings. Callers that still read the PTT
    fields keep compiling; new code should use [bindings]. *)

type provider = Codex_realtime | Openrouter
type pipeline = One_pass | Two_pass

type newline_action = Input.Text_injection.newline_action =
  | Collapse_to_space
  | Shift_enter
  | Ctrl_j

type newline_rule = Input.Text_injection.newline_rule = {
  app_id : string option;
  title_regex : string option;
  action : newline_action;
}

type entry_method = Input.Text_injection.entry_method =
  | Paste
      (** The only entry method. A file may still say ["type"]; that spelling is
          not a constructor. *)

type text_entry_rule = Input.Text_injection.text_entry_rule = {
  app_id : string option;
  title_regex : string option;
  method_ : entry_method;
}

type mode =
  | Transcribe
  | Translate
      (** A binding's recording mode. With [Two_pass], translation is a pass-2
          prompt, not a second recognizer. With [One_pass], a translate binding
          uses [translation_model] and the translate prompt. *)

type prompt_rule = {
  app_id : string option;
  title_regex : string option;
  mode : mode option;
  prompt_file : string;
  prompt : string;
}

type correction = {
  model : string;
      (** OpenRouter model id, without the [openrouter/] prefix. Pass 2 sends
          this id. *)
  reasoning_effort : string option;
      (** [none], [minimal], [low], [medium], [high] or [xhigh]. Omitted from
          the file is [None]; [none] is kept as [Some "none"]. *)
  timeout_seconds : float;
  temperature : float;
}
(** Pass 2. Absent on {!t} when [correction_model] is not set. Not used by
    [One_pass]. *)

type binding = {
  keysym : int;
  shift : bool;
  mode : mode;
  correct : bool;
      (** Whether pass 2 runs, when [pipeline] is [Two_pass]. Defaults to true
          when [correction_model] is set, otherwise false. With [One_pass],
          [false] on a transcribe binding selects the verbatim prompt; it does
          not skip the request. *)
  prompt_named : bool;
      (** The binding set [prompt]. A missing named file is an error. A missing
          default file falls back to the embedded prompt. *)
  prompt_file : string;  (** File name inside the prompts directory. *)
  prompt : string;
      (** Resolved prompt text. Pass 2 and one pass send this as the
          instructions. The transcript, or a short cue plus the recording, is
          the user message. *)
}

type t = {
  provider : provider;
  transcription_model : string;
  translation_model : string;
  transcription_language : string;
  translation_source_language : string;
  translation_target_language : string;
  ptt_release_grace_ms : int;
  ptt_min_capture_rms : float;
  ptt_min_capture_peak : float;
  ptt_transcribe_keysym : int;
  ptt_translate_keysym : int;
  ptt_transcribe_shift : bool;
  ptt_translate_shift : bool;
  thinking_effort : string option;
      (** One-pass reasoning effort. Pass 2 uses [correction.reasoning_effort].
      *)
  newline_rules : newline_rule list;
  prompt_rules : prompt_rule list;
  text_entry_rules : text_entry_rule list;
      (** Per-window delivery. [method] is always paste. A file value ["type"]
          is accepted only for compatibility: the rule pastes, and {!load} or
          {!decode} warns, naming that rule's index and its [app_id] and/or
          [title_regex]. *)
  pipeline : pipeline;
  streaming : bool;
      (** Stream pass-1 audio while the key is held. Defaults to true when the
          resolved pass-1 provider is [codex-realtime], and false for
          [openrouter]. [true] with an openrouter pass-1 model is rejected.
          [true] with [One_pass] is rejected. Meaningful only for [Two_pass]
          Codex realtime. *)
  correction : correction option;
  bindings : binding list;
  audio_retention_hours : float;
      (** How long settled audio stays in the archive. Default 24. [0] disables
          archiving of new attempts and deletes nothing. Negative values are
          rejected. *)
  openrouter_provider_order : string list option;
      (** OpenRouter [provider.order] for one-pass and pass-2 requests. Absent
          omits the field. Fallbacks stay allowed. *)
  one_pass_timeout_seconds : float;
      (** Bounds the one-pass Responses call. [correction_timeout_seconds] does
          not apply. Default {!default_one_pass_timeout_seconds}. *)
}

val config_error_message : exn -> string option
(** Recognise a configuration validation failure without publishing its
    exception constructor. *)

module For_test : sig
  val config_error : string -> exn
  (** Construct a configuration failure for reload and validation tests. *)
end

type resolved_model = { provider : provider; model : string }

val provider_of_string : string -> provider
val string_of_provider : provider -> string
val string_of_pipeline : pipeline -> string
val string_of_mode : mode -> string

val resolve_model_provider :
  default_provider:provider -> string -> resolved_model

val find_binding : t -> keysym:int -> shift:bool -> binding option

val select_prompt : t -> binding -> window:Niri.window_info option -> binding
(** First rule matching the key-press window and optional mode wins. Without a
    match (including absent focus), retains the resolved binding/pipeline
    default. Both matchers have the same semantics as text-entry rules. *)

val effective_reasoning_effort : t -> binding -> string option
(** Configured first-request effort. Retries may raise it to medium. *)

val default_audio_retention_hours : float
(** [24.]. [0] disables archiving and does not delete existing files. *)

val default_correction_timeout_seconds : float
(** [8.]. *)

val default_one_pass_timeout_seconds : float
(** [30.].

    Measured one-pass tails were about 5 s at p90 and about 11 s at worst on
    16-46 s clips (docs/research/2026-09-30-dictation-models). 30 seconds covers
    that with room for a longer utterance. Raise the key for a very long
    recording; a timeout keeps the audio for [pie retry]. The 120 s OpenRouter
    transcription deadline is not used here. *)

val default_correction_temperature : float
(** [0.2]. *)

val default_correction_prompt_file : string
(** ["correction.md"]. *)

val default_correction_translate_prompt_file : string
(** ["correction-translate.md"]. *)

val default_one_pass_prompt_file : string
(** ["one-pass.md"]. *)

val default_one_pass_verbatim_prompt_file : string
(** ["one-pass-verbatim.md"]. *)

val default_one_pass_translate_prompt_file : string
(** ["one-pass-translate.md"]. *)

val default_correction_prompt : string
val default_correction_translate_prompt : string
val default_one_pass_prompt : string
val default_one_pass_verbatim_prompt : string
val default_one_pass_translate_prompt : string
val newline_action_of_string : string -> newline_action
val string_of_newline_action : newline_action -> string

val entry_method_of_string : string -> entry_method
(** ["paste"] and the compatibility spelling ["type"] are paste. Any other value
    raises a validation failure recognised by {!config_error_message}. The
    compatibility warning is emitted by {!load} and {!decode}, which know the
    rule. *)

val string_of_entry_method : entry_method -> string
(** ["paste"]. *)

val max_config_file_bytes : int

val decode : ?warn:(string -> unit) -> Yojson.Safe.t -> (t, string) result
(** Decode the JSON object produced from TOML. After a successful decode, [warn]
    is called once for each text_entry_rules entry whose [method] was ["type"].
    The default ignores warnings. {!door_spec} uses that default; startup passes
    the process logger. *)

val door_spec : t Ox_app.Config.t
(** [door_spec.of_yojson] does not warn. Pass {!decode} with a logger when the
    process should report a retired [method]. *)

val default_source :
  caps:< Ox_caps.Cap.env ; .. > -> unit -> Ox_app.Config.source
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and source-path construction implementation depends on
    [Ox_caps], [Ox_app.Config], [Filename] and [Stdlib]. Cancellation: not a
    cancellation point in Pie; native and helper callback behavior depends on
    [Ox_caps], [Ox_app.Config], [Filename] and [Stdlib]. Finalizers: creates no
    owned resource and registers no release hook. Locking: no lock is acquired.
*)

val source_path : Ox_app.Config.source -> string

val load :
  caps:< Ox_caps.Cap.env ; .. > ->
  fs:_ Eio.Path.t ->
  ?source:Ox_app.Config.source ->
  ?read:(string -> (string, Ox_app.Config.read_error) result) ->
  ?warn:(string -> unit) ->
  unit ->
  (t, Ox_app.Config.error) result
(** [warn] receives compatibility warnings after a successful decode. The
    default ignores them. A caller that logs passes its reporter; this function
    does not print.

    Suspension: may suspend in [Ox_app.Config] filesystem reads; custom read and
    warning behavior depends on the [read] and [warn] callbacks. Cancellation:
    propagates external [Eio.Cancel.Cancelled]; read and validation result
    classification depends on [Ox_app.Config], [read] and [warn]. Finalizers:
    read-resource cleanup is delegated to [Ox_app.Config] and [Eio.Path]; custom
    resource cleanup depends on the [read] callback; no Pie switch hook is
    registered. Locking: no Pie mutex is held around [read] or [warn];
    dependency locking is outside Pie. Cancellation safety: unsafe because
    progress is lost if [read] or [warn] has non-replayable effects; this loader
    does not roll back callback effects. *)
