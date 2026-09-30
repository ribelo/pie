(** Speech-to-text configuration.

    [two_pass = false], the default, is today's single pass: the same models,
    keys, audio prompts and prompt files as before the two-pass keys existed.
    [streaming] is still checked, then ignored. Pass-2 prompt files are not
    read.

    [[bindings]] is authoritative when present. Without it, [bindings] is the
    two keys derived from [ptt_transcribe_keysym] / [ptt_transcribe_shift] and
    [ptt_translate_keysym] / [ptt_translate_shift]. With it, those legacy fields
    are only a compatibility view: the first binding of each mode, or the
    built-in default (65478 or 65479, shift false) when that mode is absent.
    Those defaults are not extra bindings. Callers that still read the legacy
    fields keep compiling; new code should use [bindings]. *)

type provider = Codex_realtime | Openrouter

type newline_action = Input.Text_injection.newline_action =
  | Collapse_to_space
  | Shift_enter
  | Ctrl_j

type newline_rule = Input.Text_injection.newline_rule = {
  app_id : string option;
  title_regex : string option;
  action : newline_action;
}

type entry_method = Input.Text_injection.entry_method = Type | Paste

type text_entry_rule = Input.Text_injection.text_entry_rule = {
  app_id : string option;
  title_regex : string option;
  method_ : entry_method;
}

type mode =
  | Transcribe
  | Translate
      (** A binding's pass-1 mode. With [two_pass], translation is a pass-2
          prompt, not a second recognizer. *)

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
(** Pass 2. Absent on {!t} when [correction_model] is not set. *)

type binding = {
  keysym : int;
  shift : bool;
  mode : mode;
  correct : bool;
      (** Whether pass 2 runs. Defaults to true when [correction_model] is set,
          otherwise false. *)
  prompt_named : bool;
      (** The binding set [prompt]. A missing named file is an error. A missing
          default file falls back to the embedded prompt. *)
  prompt_file : string;  (** File name inside the prompts directory. *)
  prompt : string;
      (** Resolved pass-2 prompt text. The pipeline sends this as the
          instructions and the transcript as the user message. *)
}

type t = {
  provider : provider;
  transcription_model : string;
  translation_model : string;
  transcription_language : string;
  translation_source_language : string;
  translation_target_language : string;
  ptt_min_capture_rms : float;
  ptt_min_capture_peak : float;
  ptt_transcribe_keysym : int;
  ptt_translate_keysym : int;
  ptt_transcribe_shift : bool;
  ptt_translate_shift : bool;
  thinking_effort : string option;
  transcription_prompt : string;
  translation_prompt : string;
  newline_rules : newline_rule list;
  text_entry_rules : text_entry_rule list;
  two_pass : bool;
  streaming : bool;
      (** Stream pass-1 audio while the key is held. Defaults to true when the
          resolved pass-1 provider is [codex-realtime], and false for
          [openrouter]. [true] with an openrouter pass-1 model is rejected, even
          when [two_pass] is false. Meaningful only when [two_pass]. *)
  correction : correction option;
  bindings : binding list;
  audio_retention_hours : float;
      (** How long settled audio stays in the archive. Default 24. [0] disables
          archiving of new attempts and deletes nothing. Negative values are
          rejected. *)
}

exception Config_error of string

type resolved_model = { provider : provider; model : string }

val provider_of_string : string -> provider
val string_of_provider : provider -> string
val string_of_mode : mode -> string

val resolve_model_provider :
  default_provider:provider -> string -> resolved_model

val find_binding : t -> keysym:int -> shift:bool -> binding option

val default_audio_retention_hours : float
(** [24.]. [0] disables archiving and does not delete existing files. *)

val default_correction_timeout_seconds : float
(** [8.]. *)

val default_correction_temperature : float
(** [0.2]. *)

val default_correction_prompt_file : string
(** ["correction.md"]. *)

val default_correction_translate_prompt_file : string
(** ["correction-translate.md"]. *)

val default_correction_prompt : string
val default_correction_translate_prompt : string
val newline_action_of_string : string -> newline_action
val string_of_newline_action : newline_action -> string
val entry_method_of_string : string -> entry_method
val string_of_entry_method : entry_method -> string
val max_config_file_bytes : int
val door_spec : t Ox_app.Config.t

val default_source :
  caps:< Ox_caps.Cap.env ; .. > -> unit -> Ox_app.Config.source

val source_path : Ox_app.Config.source -> string

val load :
  caps:< Ox_caps.Cap.env ; .. > ->
  fs:_ Eio.Path.t ->
  ?source:Ox_app.Config.source ->
  ?read:(string -> (string, Ox_app.Config.read_error) result) ->
  unit ->
  (t, Ox_app.Config.error) result
