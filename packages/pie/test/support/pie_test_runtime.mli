val run : (Eio_unix.Stdenv.base -> 'a) -> 'a
(** Own a test Eio runtime with the Door's silent posix wakeup daemon on an
    outer switch. The daemon remains alive through nested cancellation and
    cleanup, and disappears when the operation finishes. ^nwake-9uvx *)
