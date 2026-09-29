(* ^nwake-9uvx: keep the posix tick alive through each operation's protected
   child cleanup, without changing production runtime ownership or stderr. *)
let run operation =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          Ox_app.posix_wakeup ~sw env;
          operation env))
