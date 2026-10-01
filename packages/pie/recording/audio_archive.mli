(** Compact audio archive for settled attempts.

    Files live in {!dir} ([XDG_DATA_HOME/pie/audio]), created at 0700 when
    missing. Existing directories are left unchanged. Each file is mode 0600,
    written to a temporary name in that directory and renamed into place.
    Symbolic links are never followed: a link is not encoded through, decoded,
    or deleted.

    [audio_retention_hours = 0] disables new archives and does not delete
    existing files. A positive retention deletes only regular files whose names
    match an archive id plus [.opus], [.wav], [.response.json] or
    [.response-1.json], and only when they are older than the retention. Pruning
    is a directory scan; it does not walk out of the archive directory. *)

val sample_rate : int
(** 16000. Captured PCM and decoded archives are 16 kHz mono s16le. *)

val encode_timeout_s : float
(** Bound for one encode or decode. The caller applies it so a stuck encoder
    cannot outlive the service switch. *)

val dir : caps:< Ox_caps.Cap.env ; .. > -> string
(** [XDG_DATA_HOME/pie/audio]. *)

val is_safe_id : string -> bool
(** A single path component: no separators, not [.] or [..], at most 128
    characters. *)

val resolve_ffmpeg : caps:< Ox_caps.Cap.env ; .. > -> string option
(** [PIE_FFMPEG] when that variable is a non-empty path. A missing or
    non-executable [PIE_FFMPEG] is [None] and does not fall back to [PATH]. When
    the variable is unset or blank, [ffmpeg] on [PATH]. *)

val destination : dir:string -> id:string -> ext:[ `Opus | `Wav ] -> string
(** [dir/id.opus] or [dir/id.wav]. Does not check that [id] is safe. *)

val predict_path :
  caps:< Ox_caps.Cap.env ; .. > -> dir:string -> id:string -> string
(** The path {!store} tries first: Opus when {!resolve_ffmpeg} finds an encoder,
    WAV otherwise. An encoder failure still writes WAV. *)

type located = { path : string; ext : [ `Opus | `Wav ] }

val locate :
  caps:< Ox_caps.Cap.env ; .. > ->
  dir:string ->
  id:string ->
  (located, string) result
(** [Error] names the problem: unknown id, no file, a symbolic link, or no
    decoder for an Opus file when no WAV sibling exists. *)

val encode_mp3 :
  caps:< Ox_caps.Cap.env ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  bytes ->
  (bytes, string) result
(** Encode 16 kHz mono s16le PCM to a 32 kbit/s MP3 in memory with the encoder
    {!resolve_ffmpeg} finds, for upload to a provider. [Error] names the problem
    (no encoder, encoder failure); the caller falls back to WAV. *)

val store :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  dir:string ->
  id:string ->
  bytes ->
  (string, string) result
(** Encode 16 kHz mono s16le PCM to Opus, or WAV when no encoder is found or
    encoding fails. Returns the path written. *)

val store_response :
  caps:< Ox_caps.Cap.fs_write ; .. > ->
  dir:string ->
  id:string ->
  first:bool ->
  bytes ->
  (string, string) result
(** Store private provider evidence next to audio, atomically at 0600. [first]
    selects [.response-1.json], otherwise [.response.json]. The same retention
    policy prunes both. Bodies are private data, never diagnostics. *)

val load :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  (bytes, string) result
(** Decode Opus with the same encoder to 16 kHz mono s16le, or read a WAV
    written by {!store}. Refuses symbolic links. *)

val prune :
  caps:< Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  dir:string ->
  retention_hours:float ->
  now:float ->
  (int, string) result
(** Delete expired archive files. [retention_hours <= 0] deletes nothing and
    does not require the directory to exist. Returns how many files were
    removed. *)
