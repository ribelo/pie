type requested_action = Install | Replace

type disposition =
  | Installed
  | Replaced
  | Current
  | Declined
  | Planned_install
  | Planned_replace

type outcome = { path : string; disposition : disposition }
type error_kind = Usage | Internal
type failure = { path : string option; kind : error_kind; message : string }
type error = { completed : outcome list; failures : failure list }

let skill_dir_name = "pie-glossary"

let rec mkdir_p ~caps path =
  if path = "" || Sys.file_exists path then ()
  else (
    mkdir_p ~caps (Filename.dirname path);
    Ox_caps.Cap_unix.mkdir ~caps path 0o755)

let path_exists path =
  match Unix.lstat path with
  | _ -> true
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let rec remove_path ~caps path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stats when stats.Unix.st_kind = Unix.S_DIR ->
      Ox_caps.Cap_sys.readdir ~caps path
      |> Array.iter (fun name -> remove_path ~caps (Filename.concat path name));
      Ox_caps.Cap_unix.rmdir ~caps path
  | _ -> Ox_caps.Cap_unix.unlink ~caps path

let exception_message = function
  | Sys_error message -> message
  | Unix.Unix_error (error, function_, argument) ->
      Printf.sprintf "%s(%s): %s" function_ argument (Unix.error_message error)
  | exn -> Printexc.to_string exn

let write_bundle ~caps destination =
  List.iter
    (fun (relative, contents) ->
      let target = Filename.concat destination relative in
      mkdir_p ~caps (Filename.dirname target);
      Ox_caps.Cap_stdlib.Out_channel.with_open_bin ~caps target (fun channel ->
          output_string channel contents))
    Pie_skill_data.files

let unused_sibling destination label =
  let rec choose attempt =
    let candidate =
      Printf.sprintf "%s.%s-%d-%d" destination label (Unix.getpid ()) attempt
    in
    if path_exists candidate then choose (attempt + 1) else candidate
  in
  choose 0

let replace_bundle ~caps destination =
  let staging = unused_sibling destination "tmp" in
  let backup = unused_sibling destination "old" in
  (match write_bundle ~caps staging with
  | () -> ()
  | exception exn ->
      remove_path ~caps staging;
      raise exn);
  let existed = path_exists destination in
  if existed then Ox_caps.Cap_unix.rename ~caps destination backup;
  match Ox_caps.Cap_unix.rename ~caps staging destination with
  | () ->
      if existed then
        match remove_path ~caps backup with
        | () -> None
        | exception exn ->
            Some
              ( backup,
                "replacement completed but backup cleanup failed: "
                ^ exception_message exn )
      else None
  | exception exn ->
      remove_path ~caps staging;
      if existed then Ox_caps.Cap_unix.rename ~caps backup destination;
      raise exn

let rec canonicalize_missing path suffix =
  match Unix.realpath path with
  | existing -> List.fold_left Filename.concat existing suffix
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      let parent = Filename.dirname path in
      if String.equal parent path then
        raise (Sys_error (path ^ " does not exist"))
      else canonicalize_missing parent (Filename.basename path :: suffix)

let canonicalize path = canonicalize_missing path []

let supported_mappings =
  [
    (".claude", [ "skills" ]);
    (".codex", [ "skills" ]);
    (".pi", [ "agent"; "skills" ]);
    (".agents", [ "skills" ]);
  ]

let failure ?path kind message = { path; kind; message }

let discovered_roots ~home =
  supported_mappings
  |> List.fold_left
       (fun (roots, failures) (configuration, suffix) ->
         let configuration_root = Filename.concat home configuration in
         match Unix.stat configuration_root with
         | stats when stats.Unix.st_kind = Unix.S_DIR ->
             ( List.fold_left Filename.concat configuration_root suffix :: roots,
               failures )
         | _ ->
             ( roots,
               failure ~path:configuration_root Internal
                 (configuration_root
                ^ " is not an agent configuration directory")
               :: failures )
         | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (roots, failures)
         | exception exn ->
             ( roots,
               failure ~path:configuration_root Internal (exception_message exn)
               :: failures ))
       ([], [])
  |> fun (roots, failures) -> (List.rev roots, List.rev failures)

let deduplicate roots =
  let rec loop seen acc failures = function
    | [] -> (List.rev acc, List.rev failures)
    | root :: rest -> (
        match canonicalize root with
        | canonical ->
            if List.mem canonical seen then loop seen acc failures rest
            else loop (canonical :: seen) (root :: acc) failures rest
        | exception exn ->
            loop seen acc
              (failure ~path:root Internal (exception_message exn) :: failures)
              rest)
  in
  loop [] [] [] roots

type installed_node =
  | Directory of string
  | File of string * string
  | Other of string

let relative_to ~root path =
  let prefix = root ^ Filename.dir_sep in
  String.sub path (String.length prefix)
    (String.length path - String.length prefix)

let rec installed_nodes ~caps ~root path acc =
  Ox_caps.Cap_sys.readdir ~caps path
  |> Array.to_list |> List.sort String.compare
  |> List.fold_left
       (fun acc name ->
         let child = Filename.concat path name in
         let relative = relative_to ~root child in
         match Unix.lstat child with
         | stats when stats.Unix.st_kind = Unix.S_DIR ->
             installed_nodes ~caps ~root child (Directory relative :: acc)
         | stats when stats.Unix.st_kind = Unix.S_REG ->
             File
               ( relative,
                 Ox_caps.Cap_stdlib.In_channel.with_open_bin ~caps child
                   In_channel.input_all )
             :: acc
         | _ -> Other relative :: acc)
       acc

let expected_nodes =
  let rec directories relative acc =
    let parent = Filename.dirname relative in
    if String.equal parent "." || String.equal parent "" then acc
    else directories parent (Directory parent :: acc)
  in
  Pie_skill_data.files
  |> List.fold_left
       (fun acc (relative, contents) ->
         File (relative, contents) :: directories relative acc)
       []
  |> List.sort_uniq compare

let is_current ~caps destination =
  match Unix.lstat destination with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | stats when stats.Unix.st_kind <> Unix.S_DIR -> false
  | _ ->
      let actual = installed_nodes ~caps ~root:destination destination [] in
      List.sort compare actual = expected_nodes

let process_target ~caps ~automatic ~force ~no_input ~dry_run ~stdin_is_tty
    ~confirm root =
  let destination = Filename.concat root skill_dir_name in
  let exists = path_exists destination in
  if is_current ~caps destination then
    Ok ({ path = destination; disposition = Current }, None)
  else if dry_run then
    let disposition = if exists then Planned_replace else Planned_install in
    Ok ({ path = destination; disposition }, None)
  else
    let action = if exists then Replace else Install in
    let requires_consent = automatic || exists in
    if requires_consent && (not force) && (no_input || not stdin_is_tty) then
      Error
        (failure ~path:destination Usage
           (Printf.sprintf "confirmation required for %s; rerun with --force"
              destination))
    else if requires_consent && (not force) && not (confirm action destination)
    then Ok ({ path = destination; disposition = Declined }, None)
    else
      let cleanup_failure = replace_bundle ~caps destination in
      let disposition = if exists then Replaced else Installed in
      let cleanup_failure =
        Option.map
          (fun (path, message) -> failure ~path Internal message)
          cleanup_failure
      in
      Ok ({ path = destination; disposition }, cleanup_failure)

let run ~caps ~home ~path ~force ~no_input ~dry_run ~stdin_is_tty ~confirm =
  let fail failure = Error { completed = []; failures = [ failure ] } in
  let automatic, roots, discovery_failures =
    match path with
    | Some "" ->
        ( false,
          [],
          [ failure Usage "skill installation PATH must not be empty" ] )
    | Some root ->
        let root =
          if Filename.is_relative root then Filename.concat (Sys.getcwd ()) root
          else root
        in
        (false, [ root ], [])
    | None when home = "" || Filename.is_relative home ->
        ( true,
          [],
          [
            failure Usage
              "HOME must be an absolute path for automatic discovery";
          ] )
    | None -> (
        let roots, discovery_failures = discovered_roots ~home in
        match (roots, discovery_failures) with
        | [], [] ->
            let checked =
              supported_mappings
              |> List.map (fun (configuration, _) ->
                  Filename.concat home configuration)
              |> String.concat ", "
            in
            ( true,
              [],
              [
                failure Usage
                  ("no supported agent configuration found; checked: " ^ checked);
              ] )
        | roots, discovery_failures ->
            let roots, failures = deduplicate roots in
            (true, roots, discovery_failures @ failures))
  in
  if roots = [] then
    match discovery_failures with
    | _ :: _ -> Error { completed = []; failures = discovery_failures }
    | [] -> fail (failure Internal "no install target was selected")
  else
    let outcomes, failures =
      roots
      |> List.fold_left
           (fun (outcomes, failures) root ->
             match
               process_target ~caps ~automatic ~force ~no_input ~dry_run
                 ~stdin_is_tty ~confirm root
             with
             | Ok (outcome, None) -> (outcome :: outcomes, failures)
             | Ok (outcome, Some failure) ->
                 (outcome :: outcomes, failure :: failures)
             | Error failure -> (outcomes, failure :: failures)
             | exception exn ->
                 ( outcomes,
                   failure
                     ~path:(Filename.concat root skill_dir_name)
                     Internal (exception_message exn)
                   :: failures ))
           ([], discovery_failures)
    in
    let outcomes = List.rev outcomes in
    let failures = List.rev failures in
    if failures = [] then Ok outcomes
    else Error { completed = outcomes; failures }
