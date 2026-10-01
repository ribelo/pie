(** Append-only transcript history.

    The attempt pipeline appends a transcribed event when pass 1 returns text,
    and a finished event when the attempt settles. A write error is returned so
    the caller can log a warning and continue; it does not decide delivery.

    Events are JSON Lines at {!path} ([XDG_DATA_HOME/pie/history.jsonl]). The
    file is mode 0600. Missing parent directories are created at 0700 and
    existing directories are left unchanged. Each append is one [O_APPEND] write
    of a complete line, then [fsync]. If the file does not already end with a
    newline, that write starts with a newline so a torn tail cannot stick to the
    next event.

    Reading a missing file returns an empty list. A line that does not parse is
    skipped, including a torn or malformed last line, so one bad line cannot
    hide the rest. Events that share an attempt id merge into one {!entry}.
    Entries are newest first: the time of each attempt's earliest event, with a
    later line winning a tie. *)

type mode = Transcribe | Translate

val string_of_mode : mode -> string
val mode_of_string : string -> mode option

type outcome =
  | Delivered
  | Delivered_raw_after_correction_failure
  | Correction_failed
  | Delivery_failed
  | Failed

val string_of_outcome : outcome -> string
val outcome_of_string : string -> outcome option

type delivery = Pasted | Clipboard

val string_of_delivery : delivery -> string
val delivery_of_string : string -> delivery option

type window = { id : int option; app_id : string option; title : string option }
(** The niri id is absent in records written by older versions. *)

type transcribed = {
  id : string;
  time : float;  (** Unix seconds, finite and non-negative. *)
  mode : mode;
  binding : string;  (** Human description, for example {!describe_binding}. *)
  window : window option;
  glossary_paths : string list;
  pass1_model : string;
  pass1_duration_ms : int;
  raw_text : string;
}

type retry = {
  first_diagnostic : string;
  final_diagnostic : string;
  audio_format : string option;
  reasoning_effort : string option;
}

type finished = {
  id : string;
  time : float;
  final_text : string option;
  pass2_model : string option;
  pass2_duration_ms : int option;
  outcome : outcome;
  delivery : delivery option;
  diagnostic : string option;
      (** Short reason. Absent when there is nothing to say. *)
  audio_path : string option;
  response_path : string option;
  first_response_path : string option;
  retry : retry option;
      (** Archive file for this attempt. Absent on lines written before the
          archive existed, and until encoding has recorded a path. *)
}

type entry = {
  id : string;
  time : float;
  mode : mode option;
  binding : string option;
  window : window option;
  glossary_paths : string list;
  pass1_model : string option;
  pass1_duration_ms : int option;
  raw_text : string option;
  final_text : string option;
  pass2_model : string option;
  pass2_duration_ms : int option;
  outcome : outcome option;
  delivery : delivery option;
  diagnostic : string option;
  finished_at : float option;
  audio_path : string option;
  response_path : string option;
  first_response_path : string option;
  retry : retry option;
}
(** Merged view of one attempt. Fields known only after pass 1 or pass 2 are
    optional, so a transcript can be recovered before the attempt finishes.
    [time] is the transcribed time when that event exists, otherwise the
    finished time. *)

val path : caps:< Ox_caps.Cap.env ; .. > -> string
(** [XDG_DATA_HOME/pie/history.jsonl], using {!Paths.data_dir}. *)

val fresh_id : caps:< Ox_caps.Cap.time ; .. > -> unit -> string
(** An attempt id safe to store and to pass to [pie history show]. Unique within
    this process. *)

val describe_binding : keysym:int -> shift:bool -> pass2:bool -> string
(** [keysym=N], plus [+shift] and [ pass2] when those flags are set. *)

val append_transcribed :
  caps:< Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  path:string ->
  transcribed ->
  (unit, [> `History of string ]) result

val append_finished :
  caps:< Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  path:string ->
  finished ->
  (unit, [> `History of string ]) result

type audio_recorded = { id : string; time : float; path : string }

val append_audio :
  caps:< Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  path:string ->
  audio_recorded ->
  (unit, [> `History of string ]) result
(** Record the archive path after encoding. A later line wins. Does not change
    the attempt outcome. *)

val read :
  caps:< Ox_caps.Cap.fs_read ; .. > ->
  path:string ->
  (entry list, [> `History of string ]) result
(** Newest first. Does not create the file. A symbolic link is refused. A
    regular file is read even when its mode is looser than 0600, so a transcript
    can still be recovered. *)

val format_listing_line : entry -> string
(** Local time ([YYYY-MM-DD HH:MM:SS]), mode, outcome, and the first line of
    final text, or raw text when there is no final text. The text is truncated
    to 80 characters. A missing mode or outcome is [-]. *)

val json_of_entry : entry -> Yojson.Safe.t
(** Stable object: [id], [time] (UTC [YYYY-MM-DDTHH:MM:SS.sssZ]), [mode],
    [binding], [window] ([{id, appId, title}] or null), [glossaryPaths],
    [pass1Model], [pass1DurationMs], [rawText], [finalText], [pass2Model],
    [pass2DurationMs], [outcome], [delivery], [diagnostic], [finishedAt],
    [audioPath]. Absent fields are null. *)

val json_of_entries : entry list -> Yojson.Safe.t
