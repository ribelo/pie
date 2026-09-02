type error = [ `Clipboard_error of string ]
type snapshot = Empty | Text of string

let owner_start_delay = Eta.Duration.ms 25
let paste_hold = Eta.Duration.ms 250
let clipboard_mime = "text/plain;charset=utf-8"
let clipboard_error message = Eta.Effect.fail (`Clipboard_error message)

let subprocess_error context = function
  | `Subprocess_error message -> `Clipboard_error (context ^ ": " ^ message)

let text_mime_type types =
  let types =
    types |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (fun value -> value <> "")
  in
  let preferred =
    [
      "text/plain;charset=utf-8"; "text/plain"; "UTF8_STRING"; "STRING"; "TEXT";
    ]
  in
  List.find_map
    (fun candidate -> List.find_opt (String.equal candidate) types)
    preferred

let list_types ~process_mgr ~wl_paste =
  Eta.Effect.sync_exn
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let stdout, sink = Eio.Process.pipe ~sw process_mgr in
      let child =
        Eio.Process.spawn ~sw process_mgr ~stdout:sink
          [ wl_paste; "--list-types" ]
      in
      Eio.Flow.close sink;
      let output =
        Eio.Buf_read.parse_exn Eio.Buf_read.take_all stdout ~max_size:65536
      in
      Eio.Flow.close stdout;
      (Eio.Process.await child, output))
    ~catch:(function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
          `Clipboard_error
            ("failed to inspect clipboard: " ^ Printexc.to_string exn))

let snapshot ~process_mgr ~wl_paste =
  let open Eta.Syntax in
  let* status, types = list_types ~process_mgr ~wl_paste in
  match status with
  | `Exited 1 when String.trim types = "" -> Eta.Effect.pure Empty
  | `Exited 0 -> (
      match text_mime_type types with
      | None when String.trim types = "" -> Eta.Effect.pure Empty
      | None ->
          clipboard_error
            "clipboard contains no plain-text representation; refusing to \
             overwrite it"
      | Some mime ->
          let+ text =
            Sh.run_capture ~process_mgr ~prog:wl_paste
              ~args:[ "--no-newline"; "--type"; mime ]
            |> Eta.Effect.map_error
                 (subprocess_error "failed to save clipboard text")
          in
          Text text)
  | status ->
      clipboard_error
        (Format.asprintf "failed to inspect clipboard: wl-paste %a"
           Eio.Process.pp_status status)

let copy_text ~process_mgr ~wl_copy ~context text =
  Eta.Effect.sync_exn
    (fun () ->
      Eio.Process.run process_mgr
        ~stdin:(Eio.Flow.string_source text)
        [ wl_copy; "--type"; clipboard_mime ])
    ~catch:(function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> `Clipboard_error (context ^ ": " ^ Printexc.to_string exn))

let restore ~process_mgr ~wl_copy = function
  | Empty ->
      Sh.run ~process_mgr ~prog:wl_copy ~args:[ "--clear" ]
      |> Eta.Effect.map_error (subprocess_error "failed to clear clipboard")
  | Text text ->
      copy_text ~process_mgr ~wl_copy ~context:"failed to restore clipboard"
        text

let spawn_transcript ~sw ~process_mgr ~wl_copy text =
  Eta.Effect.sync_exn
    (fun () ->
      Eio.Process.spawn ~sw process_mgr
        ~stdin:(Eio.Flow.string_source text)
        [ wl_copy; "--foreground"; "--type"; clipboard_mime ])
    ~catch:(function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
          `Clipboard_error
            ("failed to offer transcript on clipboard: "
           ^ Printexc.to_string exn))

let stop_process process =
  Eta.Effect.sync (fun () ->
      Eio.Process.signal process Sys.sigkill;
      ignore (Eio.Process.await process))

let paste ~sw ~process_mgr text =
  let open Eta.Syntax in
  match (Sh.find_executable "wl-copy", Sh.find_executable "wl-paste") with
  | None, _ -> clipboard_error "wl-copy not found in PATH"
  | _, None -> clipboard_error "wl-paste not found in PATH"
  | Some wl_copy, Some wl_paste ->
      let* previous = snapshot ~process_mgr ~wl_paste in
      let restored = ref false in
      let restore_once () =
        if !restored then Eta.Effect.unit
        else
          let+ () = restore ~process_mgr ~wl_copy previous in
          restored := true
      in
      let body =
        let* process = spawn_transcript ~sw ~process_mgr ~wl_copy text in
        let consume =
          Eta.Effect.delay owner_start_delay
            (let* () = Wtype.key ~process_mgr ~modifiers:[ "ctrl" ] "v" in
             Eta.Effect.delay paste_hold Eta.Effect.unit)
          |> Eta.Effect.finally (stop_process process)
        in
        let* outcome = Eta.Effect.to_result consume in
        let* () = restore_once () in
        match outcome with
        | Ok () -> Eta.Effect.unit
        | Error error -> Eta.Effect.fail error
      in
      body |> Eta.Effect.finally (restore_once () |> Eta.Effect.ignore_errors)
