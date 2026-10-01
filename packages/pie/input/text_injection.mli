type backend = Wtype | Xdotool
type session_type = Wayland | X11
type entry_method = Paste

type outcome =
  | Clipboard_only
  | Clipboard_key_attempted
      (** Clipboard-only is a successful delivery with no keystroke. A paste key
          is best-effort dispatch, not a compositor or application receipt. *)

type result = {
  session_type : session_type;
  backend : backend;
  method_ : entry_method;
  text : string;
  outcome : outcome;
}

type newline_action = Collapse_to_space | Shift_enter | Ctrl_j

type newline_rule = {
  app_id : string option;
  title_regex : string option;
  action : newline_action;
}

type text_entry_rule = {
  app_id : string option;
  title_regex : string option;
  method_ : entry_method;
}

val normalize_text : string -> string
val collapse_line_breaks : string -> string

val matches_window :
  app_id:string option -> title_regex:string option -> Niri.window_info -> bool
(** Conjunctive matcher shared by window rules. App ids compare
    case-insensitively; titles use a Perl regular expression. *)

val newline_action_for_window :
  newline_rule list -> Niri.window_info option -> newline_action

val entry_method_for_window :
  text_entry_rule list -> Niri.window_info option -> entry_method

val prepare_text :
  method_:entry_method -> newline_action:newline_action -> string -> string

val choose_backend : session_type -> backend

val detect_session :
  caps:< Ox_caps.Cap.env ; .. > ->
  unit ->
  (session_type, ([> `Injection_error of string ] as 'err)) Stdlib.result

type injection_error =
  [ `Clipboard_error of string
  | `Injection_error of string
  | `Wtype_error of string
  | `Xdotool_error of string ]

val deliver :
  copy:(string -> (unit, injection_error) Stdlib.result) ->
  paste:(unit -> (unit, injection_error) Stdlib.result) ->
  newline:(newline_action -> (unit, injection_error) Stdlib.result) ->
  focused_window:
    (unit -> (Niri.window_info option, [ `Niri_error of string ]) Stdlib.result) ->
  target_window:Niri.window_info option ->
  newline_rules:newline_rule list ->
  string ->
  (string * outcome, injection_error) Stdlib.result
(** Effect seam for target-window delivery. Every paste follows a clipboard
    offer. A missing target or focus lookup failure copies only. *)

val inject :
  caps:< Ox_caps.Cap.env ; .. > ->
  ?clipboard_owner:Wayland.Clipboard.t ->
  ?newline_rules:newline_rule list ->
  ?text_entry_rules:text_entry_rule list ->
  ?target_window:Niri.window_info option ->
  clock:_ Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  string ->
  ( result,
    ([ `Clipboard_error of string
     | `Injection_error of string
     | `Wtype_error of string
     | `Xdotool_error of string ]
     as
     'err) )
  Stdlib.result
