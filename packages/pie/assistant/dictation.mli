(** Opt-in one-pass and two-pass attempts. Legacy attempts do not create this
    context. *)

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
      (** One Responses call with the recording. Not used for two-pass. *)
  append_transcribed :
    History.transcribed -> (unit, [ `History of string ]) result;
  append_finished : History.finished -> (unit, [ `History of string ]) result;
  warn : string -> unit;
  notify_correction_failure : Stt.Correction.failure -> unit;
  notify_request_failure : Stt.Correction.failure -> unit;
      (** One-pass request failed. The attempt keeps the audio and records
          outcome failed. *)
  fresh_id : unit -> string;
  now : unit -> float;
  log_pass : pass -> model:string -> duration_ms:int -> unit;
}

type t

val create :
  dependencies -> config:Config.Stt.t -> binding:Config.Stt.binding -> t
(** Resolve the glossary and capture the focused window once, at admission. *)

val glossary : t -> Glossary.t
val binding : t -> Config.Stt.binding
val id : t -> string

val note_audio_path : t -> string -> unit
(** Record the archive path before the finished event is written. A path already
    noted is kept. *)

val begin_pass1 : t -> unit

val recognize : t -> bytes -> (string, Ptt_loop.error) result
(** Two-pass calls [pass1]. One-pass calls [one_pass] and remembers a typed
    failure for {!run}. *)

val fail_unfinished : t -> unit
(** Append [failed] if no outcome was recorded, including failures during
    capture. *)

val run :
  t ->
  recognize:(unit -> (string, Ptt_loop.error) result) ->
  deliver:(string -> (unit, Ptt_loop.error) result) ->
  on_audio_failure:(unit -> (unit, Ptt_loop.error) result) ->
  on_text_failure:
    (string -> dispatch_uncertain:bool -> (unit, Ptt_loop.error) result) ->
  on_success:(unit -> (unit, Ptt_loop.error) result) ->
  (unit, Ptt_loop.error) result
(** Two-pass: pass 1, history, optional pass 2, delivery. One-pass: one request,
    then the same text as both the transcribed event and the final text. A
    one-pass failure notifies, publishes audio, and records [failed]. *)
