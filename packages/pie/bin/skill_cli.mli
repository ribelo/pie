open Cmdliner

val command :
  caps:
    < Ox_caps.Cap.env
    ; Ox_caps.Cap.fs_read
    ; Ox_caps.Cap.fs_write
    ; Ox_caps.Cap.stdin
    ; Ox_caps.Cap.stdout
    ; Ox_caps.Cap.stderr
    ; .. > ->
  (unit -> int) Cmd.t
(** [pie skill install]. Returns 0, 64, or 70. Does not enter a service switch.
*)
