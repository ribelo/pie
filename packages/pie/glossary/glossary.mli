(** Glossaries for one dictation attempt.

    The global glossary is [glossary.md] in the directory that holds [stt.toml],
    unless [PIE_GLOSSARY] names another file. The project glossary is
    [.pie/glossary.md] in the nearest ancestor of the focused window's working
    directory, walking up through [$HOME] inclusive, or through [/] when the
    working directory is outside [$HOME].

    Files are read on every call, symlinks are followed, and there is no size
    cap. A missing file is skipped with no warning. A path that exists but
    cannot be read (permissions, a directory, a fifo, a dangling symlink) is a
    warning and is skipped. An unreadable project glossary stops the walk: it is
    the project glossary, and a higher one is not used instead. Resolution
    itself never fails, so a glossary problem cannot stop dictation. *)

type unreadable = {
  path : string;  (** Absolute path that exists and could not be read. *)
  message : string;  (** Why the read failed. *)
}

type warning =
  | Unreadable of unreadable
  | Focus_unavailable of string
      (** Niri could not report the focused window. The global glossary is still
          resolved. *)

type t = {
  text : string;
      (** Global glossary, then project glossary, each under a Markdown heading
          naming its absolute path. [""] when neither file was read. *)
  paths : string list;
      (** Absolute paths of the files included in [text], in that order. *)
  warnings : warning list;
  cwd : string option;
      (** Working directory the project glossary was resolved from. *)
}

val resolve :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  pid:int option ->
  t
(** [resolve ~caps ~pid] resolves glossaries for a focused-window process id.
    [pid] is [None] when there is no window. The working directory is the cwd of
    the most recently started leaf among that process and its descendants (start
    time is field 22 of [/proc/<pid>/stat]); if no leaf cwd can be read, the
    window process's own cwd is used. Unreadable processes are skipped. Does not
    call niri. *)

val resolve_focused :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  ?on_focus:(Niri.window_info option -> unit) ->
  unit ->
  t
(** [resolve_focused ~caps ~process_mgr ()] is [resolve] for the process id of
    niri's focused window. A missing window is [resolve ~pid:None]. A niri
    failure is that same resolution plus a [Focus_unavailable] warning. *)
