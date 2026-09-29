type recording_mode = Ptt_transcribe | Ptt_translate
type attempt

val attempt_mode : attempt -> recording_mode
val attempt_id : attempt -> int
val signal_cancel : attempt -> unit
val await_cancel : attempt -> unit
val cancel_signalled : attempt -> bool

type start_result = Started of attempt | Busy of recording_mode | Disabled

type snapshot = {
  phase : Phase.t;
  enabled : bool;
  active : bool;
  mode : string;
  started_at : string option;
  updated_at : string;
  transcript_path : string option;
  last_error : string option;
}

type t

val current_attempt : t -> attempt option
val string_of_mode : recording_mode -> string
val mode_of_string : string -> recording_mode option
val iso_of_unix : float -> string
val json_of_snapshot : snapshot -> Yojson.Safe.t

val create :
  caps:
    < Ox_caps.Cap.env
    ; Ox_caps.Cap.fs_write
    ; Ox_caps.Cap.time
    ; Ox_caps.Cap.stderr
    ; .. > ->
  ?logger:Ox_observability.Logger.t ->
  ?persist_path:string ->
  unit ->
  t

val snapshot : t -> snapshot
val is_active : t -> bool
val set_phase : t -> attempt -> Phase.activity -> bool

val try_start :
  t -> recording_mode -> ?transcript_path:string -> unit -> start_result

val release : t -> attempt -> bool
val set_enabled : t -> bool -> unit
val toggle_enabled : t -> bool
val set_error : t -> string -> unit
val clear : t -> unit
