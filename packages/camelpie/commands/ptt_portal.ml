let ( let* ) eff f = Eta.Effect.bind f eff

let run ~process_mgr () =
  let* session = Wayland.Portal.create_session ~process_mgr () in
  let* () = Eta.Effect.sync (fun () ->
    Printf.printf "Created portal session: %s\n%!" session)
  in
  let* bound = Wayland.Portal.bind_shortcut ~process_mgr session "ptt" "Push-to-talk" "<Ctrl><Shift>P" in
  if bound then
    let* () = Eta.Effect.sync (fun () ->
      Printf.printf "Bound shortcut 'ptt' to <Ctrl><Shift>P\n%!")
    in
    Wayland.Portal.monitor ~process_mgr ()
  else
    Eta.Effect.fail (`Portal_error "BindShortcuts failed")
