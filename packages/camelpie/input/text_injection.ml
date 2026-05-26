type backend = Wtype | Xdotool

type session_type = Wayland | X11

type result = {
  session_type : session_type;
  backend : backend;
  text : string;
}

let normalize_text text = String.trim text

let choose_backend = function
  | Wayland -> Wtype
  | X11 -> Xdotool

let detect_session () =
  match Sys.getenv_opt "XDG_SESSION_TYPE" with
  | Some "wayland" -> Wayland
  | Some "x11" -> X11
  | _ ->
    if Sys.getenv_opt "WAYLAND_DISPLAY" <> None then Wayland
    else if Sys.getenv_opt "DISPLAY" <> None then X11
    else failwith "could not detect graphical session"

let type_text ~process_mgr ~backend text =
  let normalized = normalize_text text in
  if String.length normalized = 0 then
    Eta.Effect.fail (`Injection_error "no text to inject after normalization")
  else
    match backend with
    | Wtype -> Wayland.Wtype.type_text ~process_mgr normalized
    | Xdotool -> X11.Xdotool.type_text ~process_mgr normalized

let inject ~process_mgr text =
  Eta.Effect.sync (fun () -> detect_session ())
  |> Eta.Effect.bind (fun session ->
    let backend = choose_backend session in
    type_text ~process_mgr ~backend text
    |> Eta.Effect.map (fun () ->
      { session_type = session; backend; text = normalize_text text }))
