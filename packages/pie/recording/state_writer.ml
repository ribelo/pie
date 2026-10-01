let create ~sw ~on_error () =
  let initialized = ref false in
  let pending = ref None in
  let changed = Eio.Condition.create () in
  let write_pending () =
    match !pending with
    | None -> false
    | Some write ->
        pending := None;
        (try Fs.blocking write with exn -> on_error exn);
        true
  in
  let rec drain () = if write_pending () then drain () in
  Eio.Switch.on_release sw (fun () -> Eio.Cancel.protect drain);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        if not (write_pending ()) then Eio.Condition.await_no_mutex changed;
        loop ()
      in
      loop ());
  fun write ->
    if not !initialized then (
      Fs.blocking write;
      initialized := true)
    else (
      pending := Some write;
      Eio.Condition.broadcast changed)
