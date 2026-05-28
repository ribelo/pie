type error = [`Portal_error of string]

open Eta.Syntax

let busctl ~process_mgr args =
  Sh.run_capture ~process_mgr ~prog:"busctl" ~args:("--user" :: args)
  |> Eta.Effect.map (fun out -> (0, out))
  |> Eta.Effect.catch (fun (`Subprocess_error msg) ->
    Eta.Effect.pure (1, msg))

let create_session ~process_mgr () =
  let* (_status, out) =
    let token =
      Printf.sprintf "pie_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday ()))
    in
    busctl ~process_mgr [
      "call"; "org.freedesktop.portal.Desktop";
      "/org/freedesktop/portal/desktop";
      "org.freedesktop.portal.GlobalShortcuts"; "CreateSession";
      "a{sv}"; "2"; "handle_token"; "s"; token;
      "session_handle_token"; "s"; (token ^ "_session")
    ]
  in
  if String.length out = 0 then
    Eta.Effect.fail (`Portal_error "CreateSession failed — is xdg-desktop-portal running?")
  else
    match String.split_on_char '"' out with
    | _ :: path :: _ -> Eta.Effect.pure path
    | _ -> Eta.Effect.fail (`Portal_error ("unexpected busctl output: " ^ out))

let bind_shortcut ~process_mgr session_handle shortcut_id description preferred_trigger =
  let* (status, out) =
    busctl ~process_mgr [
      "call"; "org.freedesktop.portal.Desktop";
      "/org/freedesktop/portal/desktop";
      "org.freedesktop.portal.GlobalShortcuts"; "BindShortcuts";
      "oa(sa{sv})sa{sv}";
      "1"; session_handle;
      "1"; shortcut_id; "2"; "description"; "s"; description;
      "preferred_trigger"; "s"; preferred_trigger;
      ""; "0"
    ]
  in
  Eta.Effect.pure (status = 0 && String.length out > 0)

let monitor ~process_mgr () =
  Sh.run ~process_mgr ~prog:"busctl"
    ~args:["--user"; "--no-pager"; "monitor"; "org.freedesktop.portal.Desktop"]
  |> Eta.Effect.map_error (fun (`Subprocess_error msg) ->
    `Portal_error ("busctl monitor failed: " ^ msg))
