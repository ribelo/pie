(** One global glossary for every dictation attempt.

    The file is [glossary.md] in Pie's XDG configuration directory (normally
    [~/.config/pie]), unless [PIE_GLOSSARY] names another file. No project or
    focused-window discovery is performed. Files are read in full on every call,
    symlinks are followed, and there is no size cap. Missing or unreadable files
    warn and are skipped, so a glossary problem cannot stop dictation. *)

type unreadable = {
  path : string;
      (** Absolute path that could not be read, including missing files. *)
  message : string;  (** Why the read failed. *)
}

type warning = Unreadable of unreadable

type t = {
  text : string;
      (** File contents under a Markdown heading naming the absolute path. [""]
          when the file was not read. *)
  paths : string list;
      (** Zero or one absolute path, included only when the file was read. *)
  warnings : warning list;
}

type status = {
  path : string;
  exists : bool option;
      (** [None] when filesystem permissions prevent checking existence. A
          dangling symlink exists, but cannot be read. *)
  line_count : int option;
      (** Counts physical lines, including a final unterminated line. [Some 0]
          for a missing or empty file; [None] for an unreadable file. *)
  warnings : warning list;
}

val resolve :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  unit ->
  t
(** Read the global glossary anew for this attempt. *)

val status :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  unit ->
  status
(** Inspect the same file used by [resolve], without changing it. *)
