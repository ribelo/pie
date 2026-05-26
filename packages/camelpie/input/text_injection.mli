type backend = Wtype | Xdotool
type session_type = Wayland | X11
type result = {
  session_type : session_type;
  backend : backend;
  text : string;
}

val normalize_text : string -> string
val choose_backend : session_type -> backend
val detect_session : unit -> session_type

val type_text :
  process_mgr:_ Eio.Process.mgr ->
  backend:backend -> string ->
  (unit, [> `Injection_error of string | `Wtype_error of string | `Xdotool_error of string ] as 'err) Eta.Effect.t

val inject :
  process_mgr:_ Eio.Process.mgr ->
  string ->
  (result, [> `Injection_error of string | `Wtype_error of string | `Xdotool_error of string ] as 'err) Eta.Effect.t
