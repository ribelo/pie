type backend = Wtype | Xdotool
type session_type = Wayland | X11
type entry_method = Paste
type outcome = Clipboard_only | Clipboard_key_attempted

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
  | None -> Paste
  | Some window -> (
      match
        List.find_opt
          (fun (rule : text_entry_rule) ->
            matches_window ~app_id:rule.app_id ~title_regex:rule.title_regex
              window)
          rules
      with
      | Some rule -> rule.method_
      | None -> Paste)

let choose_backend = function Wayland -> Wtype | X11 -> Xdotool

let detect_session ~caps () =
  match Ox_caps.Cap_sys.getenv_opt ~caps "XDG_SESSION_TYPE" with
  | Some "wayland" -> Ok Wayland
  | Some "x11" -> Ok X11
  | _ ->
      if Ox_caps.Cap_sys.getenv_opt ~caps "WAYLAND_DISPLAY" <> None then
        Ok Wayland
      else if Ox_caps.Cap_sys.getenv_opt ~caps "DISPLAY" <> None then Ok X11
      else Error (`Injection_error "could not detect graphical session")

let press_newline ~caps ~process_mgr ~backend action =
  match (backend, action) with
  | _, Collapse_to_space -> Ok ()
  | Wtype, Shift_enter ->
      Wayland.Wtype.key ~caps ~process_mgr ~modifiers:[ "shift" ] "Return"
  | Wtype, Ctrl_j ->
      Wayland.Wtype.key ~caps ~process_mgr ~modifiers:[ "ctrl" ] "j"
  | Xdotool, Shift_enter -> X11.Xdotool.key ~caps ~process_mgr "shift+Return"
  | Xdotool, Ctrl_j -> X11.Xdotool.key ~caps ~process_mgr "ctrl+j"

let normalize_for_action action text =
  match action with
  | Collapse_to_space -> collapse_line_breaks text
  | Shift_enter | Ctrl_j -> normalize_text text

let prepare_text ~method_:Paste ~newline_action text =
  normalize_for_action newline_action (normalize_text text)

type injection_error =
  [ `Clipboard_error of string
  | `Injection_error of string
  | `Wtype_error of string
  | `Xdotool_error of string ]

(* The clipboard is the destination unless the key-press window is still
   focused and the offered text is the selection. This seam deliberately
   cannot focus a window or type characters. *)
let deliver ~copy ~paste ?(confirm = fun _ -> true) ~newline ~focused_window
    ~target_window ~newline_rules text =
  let open Result.Syntax in
  let text = normalize_text text in
  if text = "" then
    Error (`Injection_error "no text to inject after normalization")
  else
    let current =
      match focused_window () with Ok window -> window | Error _ -> None
    in
    let same_window =
      match (target_window, current) with
      | Some (target : Niri.window_info), Some (current : Niri.window_info) ->
          target.id = current.id
      | _ -> false
    in
    if not same_window then
      let* () = copy text in
      Ok (text, Clipboard_only)
    else
      let action = newline_action_for_window newline_rules target_window in
      let prepared = prepare_text ~method_:Paste ~newline_action:action text in
      let* () = copy prepared in
      let paste_if_confirmed offered =
        if not (confirm offered) then Ok `Unconfirmed
        else
          let* () = paste () in
          Ok `Pasted
      in
      let paste_segment segment =
        if segment = "" then Ok `Pasted
        else
          let* () = copy segment in
          paste_if_confirmed segment
      in
      let rec paste_lines = function
        | [] -> Ok `Pasted
        | [ segment ] -> paste_segment segment
        | segment :: rest -> (
            let* step = paste_segment segment in
            match step with
            | `Unconfirmed -> Ok `Unconfirmed
            | `Pasted ->
                let* () = newline action in
                paste_lines rest)
      in
      let injected =
        match action with
        | Collapse_to_space -> paste_if_confirmed prepared
        | Shift_enter | Ctrl_j -> paste_lines (split_line_breaks prepared)
      in
      (* Segment pastes temporarily change the clipboard. Restore the complete
         delivery even when a segment paste, confirmation, or newline fails. *)
      let restored =
        match action with
        | Collapse_to_space -> Ok ()
        | Shift_enter | Ctrl_j -> copy prepared
      in
      let* step = injected in
      let* () = restored in
      match step with
      | `Pasted -> Ok (prepared, Clipboard_key_attempted)
      | `Unconfirmed -> Ok (prepared, Clipboard_only)

(* wl-copy publishes the selection only after stdin EOF and a compositor
   round-trip. Poll until the selection is exactly [text]. *)
let selection_poll_interval = 0.020
let selection_confirm_bound = 1.0

let await_selection ~mono_clock ~read text =
  let started = Eio.Time.Mono.now mono_clock in
  let elapsed () =
    Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now mono_clock))
    /. 1e9
  in
  let rec loop () =
    if elapsed () >= selection_confirm_bound then false
    else
      match read () with
      | Some got when String.equal got text -> true
      | Some _ | None ->
          let before = Eio.Time.Mono.now mono_clock in
          let remaining = selection_confirm_bound -. elapsed () in
          if remaining <= 0. then false
          else (
            Eio.Time.Mono.sleep mono_clock
              (min selection_poll_interval remaining);
            (* A clock that does not advance must not spin. *)
            if Mtime.compare (Eio.Time.Mono.now mono_clock) before <= 0 then
              false
            else loop ())
  in
  loop ()

let read_wl_paste ~process_mgr prog () =
  match
    Sh.run_capture ~process_mgr ~prog
      ~args:[ "--no-newline"; "--type"; "text/plain;charset=utf-8" ]
  with
  | Ok text -> Some text
  | Error _ -> None

let inject ~caps ?clipboard_owner ?(newline_rules = []) ?(text_entry_rules = [])
    ?(target_window = None) ?read_selection ~mono_clock ~process_mgr text =
  let open Result.Syntax in
  let detected = detect_session ~caps () in
  let session =
    match detected with Ok session -> session | Error _ -> Wayland
  in
  let backend = choose_backend session in
  let copy text =
    match clipboard_owner with
    | Some owner ->
        (Wayland.Clipboard.offer owner text
          :> (unit, injection_error) Stdlib.result)
    | None -> Error (`Injection_error "paste entry requires a clipboard owner")
  in
  let focused_window () = Niri.focused_window ~process_mgr () in
  let target_window =
    match detected with Ok Wayland -> target_window | Ok X11 | Error _ -> None
  in
  let confirm offered =
    match read_selection with
    | Some read -> await_selection ~mono_clock ~read offered
    | None -> (
        match Sh.find_executable ~caps "wl-paste" with
        | None -> false
        | Some prog ->
            await_selection ~mono_clock
              ~read:(read_wl_paste ~process_mgr prog)
              offered)
  in
  let paste () =
    (Wayland.Wtype.key ~caps ~process_mgr ~modifiers:[ "ctrl" ] "v"
      :> (unit, injection_error) Stdlib.result)
  in
  let newline action =
    (press_newline ~caps ~process_mgr ~backend action
      :> (unit, injection_error) Stdlib.result)
  in
  let* prepared, outcome =
    deliver ~copy ~paste ~confirm ~newline ~focused_window ~target_window
      ~newline_rules text
  in
  let method_ = entry_method_for_window text_entry_rules target_window in
  Ok { session_type = session; backend; method_; text = prepared; outcome }
