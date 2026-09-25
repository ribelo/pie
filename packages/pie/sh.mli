val find_executable : string -> string option

val discarded_stderr : unit -> Eio.Flow.sink_ty Eio.Resource.t

val failure_message : prog:string -> exn -> string

val run :
  process_mgr:_ Eio.Process.mgr ->
  prog:string ->
  args:string list ->
  (unit, [> `Subprocess_error of string ]) Eta.Effect.t

val run_capture :
  process_mgr:_ Eio.Process.mgr ->
  prog:string ->
  args:string list ->
  (string, [> `Subprocess_error of string ]) Eta.Effect.t
