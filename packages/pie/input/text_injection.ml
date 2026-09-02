type backend = Wtype | Xdotool
type session_type = Wayland | X11
type entry_method = Type | Paste

type result = {
  session_type : session_type;
  backend : backend;
  method_ : entry_method;
  text : string;
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

let normalize_text text = String.trim text

let split_line_breaks text =
  let len = String.length text in
  let rec loop start i acc =
    if i >= len then List.rev (String.sub text start (len - start) :: acc)
    else
      match text.[i] with
      | '\r' ->
          let next =
            if i + 1 < len && text.[i + 1] = '\n' then i + 2 else i + 1
          in
          loop next next (String.sub text start (i - start) :: acc)
      | '\n' -> loop (i + 1) (i + 1) (String.sub text start (i - start) :: acc)
      | _ -> loop start (i + 1) acc
  in
  loop 0 0 []

let collapse_line_breaks text =
  text |> split_line_breaks |> List.map String.trim
  |> List.filter (fun part -> String.length part > 0)
  |> String.concat " "

let lower_ascii text = String.lowercase_ascii text

let title_matches_regex ~pattern title =
  try Re.execp (Re.Perl.compile_pat pattern) title
  with Re.Perl.Parse_error -> false

let matches_window ~app_id ~title_regex (window : Niri.window_info) =
  let app_matches =
    match app_id with
    | None -> true
    | Some expected -> (
        match window.app_id with
        | Some actual ->
            String.equal (lower_ascii actual) (lower_ascii expected)
        | None -> false)
  in
  let title_matches =
    match title_regex with
    | None -> true
    | Some pattern -> (
        match window.title with
        | Some title -> title_matches_regex ~pattern title
        | None -> false)
  in
  app_matches && title_matches

let newline_action_for_window rules focused_window =
  match focused_window with
  | None -> Collapse_to_space
  | Some window -> (
      match
        List.find_opt
          (fun (rule : newline_rule) ->
            matches_window ~app_id:rule.app_id ~title_regex:rule.title_regex
              window)
          rules
      with
      | Some rule -> rule.action
      | None -> Collapse_to_space)

let entry_method_for_window rules focused_window =
  match focused_window with
  | None -> Type
  | Some window -> (
      match
        List.find_opt
          (fun (rule : text_entry_rule) ->
            matches_window ~app_id:rule.app_id ~title_regex:rule.title_regex
              window)
          rules
      with
      | Some rule -> rule.method_
      | None -> Type)

let choose_backend = function Wayland -> Wtype | X11 -> Xdotool

let detect_session () =
  match Sys.getenv_opt "XDG_SESSION_TYPE" with
  | Some "wayland" -> Eta.Effect.pure Wayland
  | Some "x11" -> Eta.Effect.pure X11
  | _ ->
      if Sys.getenv_opt "WAYLAND_DISPLAY" <> None then Eta.Effect.pure Wayland
      else if Sys.getenv_opt "DISPLAY" <> None then Eta.Effect.pure X11
      else
        Eta.Effect.fail (`Injection_error "could not detect graphical session")

let press_newline ~process_mgr ~backend action =
  match (backend, action) with
  | _, Collapse_to_space -> Eta.Effect.unit
  | Wtype, Shift_enter ->
      Wayland.Wtype.key ~process_mgr ~modifiers:[ "shift" ] "Return"
  | Wtype, Ctrl_j -> Wayland.Wtype.key ~process_mgr ~modifiers:[ "ctrl" ] "j"
  | Xdotool, Shift_enter -> X11.Xdotool.key ~process_mgr "shift+Return"
  | Xdotool, Ctrl_j -> X11.Xdotool.key ~process_mgr "ctrl+j"

let type_segment ~process_mgr ~backend segment =
  if String.length segment = 0 then Eta.Effect.unit
  else
    match backend with
    | Wtype -> Wayland.Wtype.type_text ~process_mgr segment
    | Xdotool -> X11.Xdotool.type_text ~process_mgr segment

let type_lines ~process_mgr ~backend ~newline_action text =
  let open Eta.Syntax in
  let rec loop = function
    | [] -> Eta.Effect.unit
    | [ segment ] -> type_segment ~process_mgr ~backend segment
    | segment :: rest ->
        let* () = type_segment ~process_mgr ~backend segment in
        let* () = press_newline ~process_mgr ~backend newline_action in
        loop rest
  in
  loop (split_line_breaks text)

let normalize_for_action action text =
  match action with
  | Collapse_to_space -> collapse_line_breaks text
  | Shift_enter | Ctrl_j -> normalize_text text

let prepare_text ~method_ ~newline_action text =
  match method_ with
  | Type -> normalize_for_action newline_action (normalize_text text)
  | Paste -> normalize_text text

let type_text ?(newline_action = Collapse_to_space) ~process_mgr ~backend text =
  let normalized = normalize_text text in
  if String.length normalized = 0 then
    Eta.Effect.fail (`Injection_error "no text to inject after normalization")
  else
    let typed = normalize_for_action newline_action normalized in
    if String.length typed = 0 then
      Eta.Effect.fail (`Injection_error "no text to inject after normalization")
    else if newline_action = Collapse_to_space then
      type_segment ~process_mgr ~backend typed
    else type_lines ~process_mgr ~backend ~newline_action typed

let focused_window_for_rules ~process_mgr ~newline_rules ~text_entry_rules =
  if newline_rules = [] && text_entry_rules = [] then Eta.Effect.pure None
  else
    Niri.focused_window ~process_mgr ()
    |> Eta.Effect.bind_error (fun (`Niri_error message) ->
        if text_entry_rules = [] then Eta.Effect.pure None
        else
          Eta.Effect.fail
            (`Injection_error
               ("failed to identify focused application: " ^ message)))
    |> Eta.Effect.bind (function
      | None when text_entry_rules <> [] ->
          Eta.Effect.fail
            (`Injection_error "niri reported no focused application")
      | focused_window -> Eta.Effect.pure focused_window)

let inject ?sw ?(newline_rules = []) ?(text_entry_rules = []) ~process_mgr text
    =
  let open Eta.Syntax in
  let* session = detect_session () in
  let backend = choose_backend session in
  let* () =
    match (session, text_entry_rules) with
    | X11, _ :: _ ->
        Eta.Effect.fail
          (`Injection_error "text_entry_rules require a Niri Wayland session")
    | _ -> Eta.Effect.unit
  in
  let* focused_window =
    focused_window_for_rules ~process_mgr ~newline_rules ~text_entry_rules
  in
  let newline_action = newline_action_for_window newline_rules focused_window in
  let method_ = entry_method_for_window text_entry_rules focused_window in
  let prepared = prepare_text ~method_ ~newline_action text in
  if String.length prepared = 0 then
    Eta.Effect.fail (`Injection_error "no text to inject after normalization")
  else
    let+ () =
      match (method_, session, sw) with
      | Type, _, _ -> type_text ~newline_action ~process_mgr ~backend prepared
      | Paste, Wayland, Some sw ->
          Wayland.Clipboard.paste ~sw ~process_mgr prepared
      | Paste, Wayland, None ->
          Eta.Effect.fail
            (`Injection_error "paste entry requires an Eio switch")
      | Paste, X11, _ ->
          Eta.Effect.fail
            (`Injection_error "paste entry is not supported on X11")
    in
    { session_type = session; backend; method_; text = prepared }
