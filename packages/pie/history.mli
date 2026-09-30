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

type window = { app_id : string option; title : string option }

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

type finished = {
  id : string;
  time : float;
  final_text : string option;
  pass2_model : string option;
  pass2_duration_ms : int option;
  outcome : outcome;
  diagnostic : string option;
      (** Short reason. Absent when there is nothing to say. *)
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
  diagnostic : string option;
  finished_at : float option;
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
    [binding], [window] ([{appId, title}] or null), [glossaryPaths],
    [pass1Model], [pass1DurationMs], [rawText], [finalText], [pass2Model],
    [pass2DurationMs], [outcome], [diagnostic], [finishedAt]. Absent fields are
    null. *)

val json_of_entries : entry list -> Yojson.Safe.t
