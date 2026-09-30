open Cmdliner

let exits =
  Cmd.Exit.defaults
  @ [
      Cmd.Exit.info 64
        ~doc:"Discovery, confirmation, or the install path could not proceed.";
      Cmd.Exit.info 70 ~doc:"A filesystem error prevented installation.";
    ]

let command ~caps =
  let path =
    Arg.(
      value
      & pos 0 (some string) None
      & info [] ~docv:"PATH"
          ~doc:
            "Skills root. The skill is installed at PATH/pie-glossary/. Omit \
             to discover user-level agent skill directories under HOME.")
  in
  let force =
    Arg.(
      value & flag
      & info [ "f"; "force" ]
          ~doc:"Install or replace every selected target without confirmation.")
  in
  let no_input =
    Arg.(
      value & flag
      & info [ "no-input" ]
          ~doc:"Never prompt. Fail when confirmation is required.")
  in
  let dry_run =
    Arg.(
      value & flag
      & info [ "n"; "dry-run" ]
          ~doc:
            "Report each planned action without changing files or asking, even \
             with --force.")
  in
  let pretty =
    Arg.(
      value & flag
      & info [ "pretty" ]
          ~doc:"Indent each JSON result. Default: one compact line per target.")
  in
  let confirm action destination =
    let verb =
      match action with
      | Skill_install.Install -> "Install"
      | Skill_install.Replace -> "Replace"
    in
    Ox_caps.Cap_stdlib.eprintf ~caps "%s Pie glossary skill at %s? [y/N] %!"
      verb destination;
    match
      input_line (Ox_caps.Cap_stdlib.stdin ~caps)
      |> String.trim |> String.lowercase_ascii
    with
    | "y" | "yes" -> true
    | _ | (exception End_of_file) -> false
  in
  let action_name = function
    | Skill_install.Installed -> "installed"
    | Skill_install.Replaced -> "replaced"
    | Skill_install.Current -> "current"
    | Skill_install.Declined -> "skipped"
    | Skill_install.Planned_install -> "would install"
    | Skill_install.Planned_replace -> "would replace"
  in
  let show_outcome ~pretty outcome =
    let json =
      `Assoc
        [
          ("action", `String (action_name outcome.Skill_install.disposition));
          ("path", `String outcome.Skill_install.path);
        ]
    in
    let text =
      if pretty then Yojson.Safe.pretty_to_string json
      else Yojson.Safe.to_string json
    in
    Ox_caps.Cap_stdlib.print_endline ~caps text
  in
  let failure_message failure =
    match failure.Skill_install.path with
    | None -> failure.Skill_install.message
    | Some path -> path ^ ": " ^ failure.Skill_install.message
  in
  let exit_of failures =
    if
      List.exists
        (fun failure -> failure.Skill_install.kind = Skill_install.Internal)
        failures
    then 70
    else 64
  in
  let run pretty path force no_input dry_run () =
    let home =
      match Ox_caps.Cap_sys.getenv_opt ~caps "HOME" with
      | Some home -> home
      | None -> ""
    in
    match
      Skill_install.run ~caps ~home ~path ~force ~no_input ~dry_run
        ~stdin_is_tty:(Unix.isatty Unix.stdin) ~confirm
    with
    | Ok outcomes ->
        List.iter (show_outcome ~pretty) outcomes;
        0
    | Error error ->
        List.iter (show_outcome ~pretty) error.Skill_install.completed;
        List.iter
          (fun failure ->
            Ox_caps.Cap_stdlib.prerr_endline ~caps (failure_message failure))
          error.failures;
        exit_of error.failures
    | exception exn ->
        Ox_caps.Cap_stdlib.prerr_endline ~caps (Printexc.to_string exn);
        70
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Installs the bundled glossary skill, which teaches an agent to write \
         .pie/glossary.md. Prints one JSON object per target with fields \
         action and path. action is installed, replaced, current, skipped, \
         would install, or would replace.";
      `P
        "Without PATH, targets are HOME/.claude/skills, HOME/.codex/skills, \
         HOME/.pi/agent/skills and HOME/.agents/skills, but only when that \
         agent's configuration directory exists. Destinations that resolve to \
         the same directory are processed once.";
      `P
        "A fresh explicit PATH is installed without asking. Replacing an \
         existing skill, or installing into a discovered directory, asks once \
         per target when stdin is a terminal. The default answer is No. \
         --force supplies that consent. Without a terminal, or with \
         --no-input, a target that needs consent fails and is left unchanged.";
      `S Manpage.s_examples;
      `Pre "pie skill install";
      `Noblank;
      `Pre "pie skill install ~/.agents/skills";
      `Noblank;
      `Pre "pie skill install --dry-run";
      `Noblank;
      `Pre "pie skill install --force ~/.claude/skills";
      `Noblank;
      `Pre "pie skill install --no-input --force";
    ]
  in
  let install =
    Cmd.v
      (Cmd.info "install"
         ~doc:
           "Install the bundled glossary skill. Prints one JSON object per \
            target."
         ~man ~exits)
      Term.(const run $ pretty $ path $ force $ no_input $ dry_run)
  in
  Cmd.group
    ~default:Term.(ret (const (`Help (`Pager, None))))
    (Cmd.info "skill" ~doc:"Agent skill operations." ~exits)
    [ install ]
