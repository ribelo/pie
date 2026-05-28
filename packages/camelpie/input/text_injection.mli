type backend = Wtype | Xdotool
type session_type = Wayland | X11
type result = {
  session_type : session_type;
  backend : backend;
  text : string;
}

type newline_action =
  | Collapse_to_space
  | Shift_enter
  | Ctrl_j

type newline_rule = {
  app_id : string option;
  title_regex : string option;
  action : newline_action;
}

val normalize_text : string -> string
val collapse_line_breaks : string -> string
val newline_action_for_window : newline_rule list -> Niri.window_info option -> newline_action
val choose_backend : session_type -> backend
val detect_session : unit -> session_type

val type_text :
  ?newline_action:newline_action ->
  process_mgr:_ Eio.Process.mgr ->
  backend:backend -> string ->
  (unit, [> `Injection_error of string | `Wtype_error of string | `Xdotool_error of string ] as 'err) Eta.Effect.t

val inject :
  ?newline_rules:newline_rule list ->
  process_mgr:_ Eio.Process.mgr ->
  string ->
  (result, [> `Injection_error of string | `Wtype_error of string | `Xdotool_error of string ] as 'err) Eta.Effect.t
