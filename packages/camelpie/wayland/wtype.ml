type error = [`Wtype_error of string]

let normalize_for_typing text = String.trim text

open Eta.Syntax

let key ?(modifiers = []) ~process_mgr key =
  match Sh.find_executable "wtype" with
  | None -> Eta.Effect.fail (`Wtype_error "wtype not found in PATH")
  | Some wtype ->
    let args =
      List.concat_map (fun modifier -> ["-M"; modifier]) modifiers
      @ ["-k"; key]
      @ List.concat_map (fun modifier -> ["-m"; modifier]) (List.rev modifiers)
    in
    Sh.run ~process_mgr ~prog:wtype ~args
    |> Eta.Effect.map_error (fun (`Subprocess_error msg) -> `Wtype_error msg)

let type_text ?(mode = `Auto) ?(timeout_sec = 30.) ~process_mgr text =
  let normalized = normalize_for_typing text in
  if String.length normalized = 0 then Eta.Effect.unit
  else
    match Sh.find_executable "wtype" with
    | None -> Eta.Effect.fail (`Wtype_error "wtype not found in PATH")
    | Some wtype ->
      let use_clipboard =
        match mode with
        | `Clipboard -> true
        | `Direct -> false
        | `Auto ->
          String.exists (fun c -> c > '~' || c = '\'' || c = '"' || c = '`' || c = '$') normalized
      in
      if use_clipboard then
        match Sh.find_executable "wl-copy" with
        | None ->
          Sh.run ~process_mgr ~prog:wtype ~args:["--"; normalized]
          |> Eta.Effect.map_error (fun (`Subprocess_error msg) -> `Wtype_error msg)
        | Some wl_copy ->
          let* prev =
            Sh.run_capture ~process_mgr ~prog:"wl-paste" ~args:["-n"]
            |> Eta.Effect.catch (fun _ -> Eta.Effect.pure "")
          in
          let* () =
            Sh.run ~process_mgr ~prog:wl_copy
              ~args:["-n"; "-t"; "text/plain;charset=utf-8"; "--"; normalized]
            |> Eta.Effect.map_error (fun (`Subprocess_error msg) ->
              `Wtype_error ("wl-copy failed: " ^ msg))
          in
          let* () =
            Sh.run ~process_mgr ~prog:wtype
              ~args:["-M"; "ctrl"; "-k"; "v"; "-m"; "ctrl"]
            |> Eta.Effect.map_error (fun (`Subprocess_error msg) ->
              `Wtype_error ("wtype paste failed: " ^ msg))
          in
          if String.length prev > 0 then
            Sh.run ~process_mgr ~prog:wl_copy
              ~args:["-n"; "-t"; "text/plain;charset=utf-8"; "--"; prev]
            |> Eta.Effect.map_error (fun (`Subprocess_error msg) ->
              `Wtype_error ("wl-copy restore failed: " ^ msg))
          else
            Eta.Effect.unit
      else
        Sh.run ~process_mgr ~prog:wtype ~args:["--"; normalized]
        |> Eta.Effect.map_error (fun (`Subprocess_error msg) -> `Wtype_error msg)
