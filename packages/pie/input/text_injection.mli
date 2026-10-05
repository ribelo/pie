type backend = Wtype | Xdotool
type session_type = Wayland | X11
type entry_method = Paste

type outcome =
  | Clipboard_only
  | Clipboard_key_attempted
      (** Clipboard-only is a successful delivery with no keystroke. A paste key
          is sent only after the offered text is the selection; the key itself
          is not an application receipt. *)

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
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and executable search implementation depends on
    [Ox_caps], [Unix], [Filename] and [Stdlib]. Cancellation: not a cancellation
    point in Pie; native and helper callback behavior depends on [Ox_caps],
    [Unix], [Filename] and [Stdlib]. Finalizers: creates no owned resource and
    registers no release hook. Locking: no lock is acquired. *)

type injection_error =
  [ `Clipboard_error of string
  | `Injection_error of string
  | `Wtype_error of string
  | `Xdotool_error of string ]

val deliver :
  copy:(string -> (unit, injection_error) Stdlib.result) ->
  paste:(unit -> (unit, injection_error) Stdlib.result) ->
  ?confirm:(string -> bool) ->
  newline:(newline_action -> (unit, injection_error) Stdlib.result) ->
  focused_window:
    (unit -> (Niri.window_info option, [ `Niri_error of string ]) Stdlib.result) ->
  target_window:Niri.window_info option ->
  newline_rules:newline_rule list ->
  string ->
  (string * outcome, injection_error) Stdlib.result
(** Effect seam for target-window delivery. Every paste follows a clipboard
    offer. A missing target, a focus lookup failure, or [confirm] returning
    false copies only and sends no paste key. [confirm] defaults to success. *)

val inject :
  caps:< Ox_caps.Cap.env ; .. > ->
  ?clipboard_owner:Wayland.Clipboard.t ->
  ?newline_rules:newline_rule list ->
  ?text_entry_rules:text_entry_rule list ->
  ?target_window:Niri.window_info option ->
  ?read_selection:(unit -> string option) ->
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
(** [read_selection] is one clipboard read. Omitted, it runs
    [wl-paste --no-newline --type text/plain;charset=utf-8]. A paste key is sent
    only after a read returns exactly the offered text, polling every 20 ms for
    at most 1000 ms. Otherwise the offer stays on the clipboard and the outcome
    is [Clipboard_only].

    Suspension: may suspend while [Eio.Process] runs injection/focus
    subprocesses; native executable and environment lookup depend on [Ox_caps],
    [Unix] and [Stdlib]. Cancellation: propagates [Eio.Cancel.Cancelled] and
    [Eio.Exn.Multiple]; ordinary subprocess errors become injection errors;
    dependency behavior depends on [Eio.Process]. Finalizers: subprocess
    lifecycle is delegated to [Eio.Process]; no Pie switch hook is registered.
    Locking: no Pie mutex is acquired; dependency locking is outside Pie.
    Cancellation safety: unsafe because progress is lost if text or focus was
    already changed; retry may inject duplicate text. *)
