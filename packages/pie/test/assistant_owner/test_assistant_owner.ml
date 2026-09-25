(* PIE003 owner workflow tests.
   Buffered record-to-delivery uses a fake stt callback on the Openrouter
   clip branch selected from published config. Streaming Codex goes through
   production with_transcription_stream with a private child env and a local
   WS stand-in. *)

let transcribe_keysym = 65478
let stream_child_env = "PIE_OWNER_STREAM_CHILD"

exception Cancel_service

let base_config ?(provider = Pie.Config.Stt.Openrouter)
    ?(model = "openrouter/owner-a") () : Pie.Config.Stt.t =
  {
    provider;
    transcription_model = model;
    translation_model = model;
    transcription_language = "English";
    translation_source_language = "English";
    translation_target_language = "English";
    ptt_min_capture_rms = 0.0;
    ptt_min_capture_peak = 0.0;
    ptt_transcribe_keysym = transcribe_keysym;
    ptt_translate_keysym = 65479;
    ptt_transcribe_shift = false;
    ptt_translate_shift = false;
    thinking_effort = None;
    transcription_prompt = "Transcribe in {{language}}.";
    translation_prompt =
      "Translate from {{source_language}} to {{target_language}}.";
    newline_rules = [];
    text_entry_rules = [];
  }

let loud_pcm =
  let b = Bytes.create 640 in
  for i = 0 to 319 do
    Bytes.set_int16_le b (i * 2) 20000
  done;
  b

let key ~released : Pie.Keyboard.Monitor.key_event =
  { released; state = 0; keysym = transcribe_keysym; unichar = 0; keycode = 75 }

let wait_until ?(steps = 40) ~clock pred =
  let rec loop n =
    if pred () then ()
    else if n <= 0 then Alcotest.fail "deadline"
    else (
      Eio.Time.sleep clock 0.05;
      loop (n - 1))
  in
  loop steps

let pp_run_error fmt = function
  | `Pulse_error error ->
      Format.pp_print_string fmt (Pie.Pulse.Client.error_message error)
  | `Ptt_error msg -> Format.pp_print_string fmt msg

let fail_cause cause =
  Alcotest.failf "workflow failed: %a" (Eta.Cause.pp pp_run_error) cause

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      Array.iter
        (fun n -> if n <> "." && n <> ".." then rm_rf (Filename.concat path n))
        (try Sys.readdir path with Sys_error _ -> [||]);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let with_dir f =
  let dir = Filename.temp_file "pie-owner" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

type audio_cmd = Chunk of bytes | Fail of Pie.Pulse.Client.error

let make_audio () =
  let cmd = Eio.Stream.create 16 in
  let rec source () =
    Eta_stream.Stream.from_effect
      (Eta.Effect.sync (fun () -> Eio.Stream.take cmd)
      |> Eta.Effect.bind (function
        | Chunk bytes -> Eta.Effect.pure bytes
        | Fail error -> Eta.Effect.fail (`Pulse_error error)))
    |> Eta_stream.Stream.flat_map (fun chunk ->
        Eta_stream.Stream.concat (Eta_stream.Stream.succeed chunk) (source ()))
  in
  (cmd, source ())

let offer_pcm cmd = Eio.Stream.add cmd (Chunk loud_pcm)
let fail_audio cmd error = Eio.Stream.add cmd (Fail error)

let open_fds_for path =
  try
    Sys.readdir "/proc/self/fd"
    |> Array.to_list
    |> List.filter (fun n ->
        try Unix.readlink (Filename.concat "/proc/self/fd" n) = path
        with _ -> false)
    |> List.length
  with Sys_error _ -> 0

let pcm_files dir =
  let active = Filename.concat dir "active" in
  try
    Sys.readdir active |> Array.to_list
    |> List.filter_map (fun name ->
        if String.length name >= 5 && String.sub name 0 5 = "slot-" then
          let slot = Filename.concat active name in
          let draft = Filename.concat slot "draft.pcm" in
          let payload = Filename.concat slot "payload" in
          if Sys.file_exists draft then Some draft
          else if Sys.file_exists payload then Some payload
          else Some slot
        else None)
  with Sys_error _ -> []

let status_bool ~net ~sw socket_path key =
  let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> Eio.Flow.close socket)
    (fun () ->
      Eio.Flow.copy_string "{\"method\":\"Status\"}\n" socket;
      let line =
        Eio.Buf_read.of_flow ~max_size:65536 socket |> Eio.Buf_read.line
      in
      match Yojson.Safe.from_string line with
      | `Assoc fields -> (
          match List.assoc_opt key fields with
          | Some (`Bool b) -> b
          | _ -> false)
      | _ -> false)

let status_last_error ~net ~sw socket_path =
  let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> Eio.Flow.close socket)
    (fun () ->
      Eio.Flow.copy_string "{\"method\":\"Status\"}\n" socket;
      let line =
        Eio.Buf_read.of_flow ~max_size:65536 socket |> Eio.Buf_read.line
      in
      match Yojson.Safe.from_string line with
      | `Assoc fields -> (
          match List.assoc_opt "lastError" fields with
          | Some (`String s) -> Some s
          | _ -> None)
      | _ -> None)

let owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir ~config_control ~stt
    ~deliver ~keyboard ~audio =
  let socket_path = Filename.concat dir "control.sock" in
  let retry_dir = Filename.concat dir "recordings" in
  (try Unix.mkdir retry_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let inputs =
    { Pie.Assistant.Service.keyboard; audio; source_name = Some "test-source" }
  in
  let workflow =
    Pie.Assistant.Service.run_with_inputs ~sw ~net ~process_mgr ~mono_clock
      ~inputs ~config_control ~stt ~deliver ~socket_path ~retry_dir
      ~min_duration_ms:0 ()
  in
  (socket_path, retry_dir, workflow)

let with_workflow_background ~sw ~clock workflow assertions =
  match
    Eta_eio.run_once ~sw ~clock
      (Eta.Effect.with_background ~name:"pie.owner.workflow" workflow (fun () ->
           Eta.Effect.sync assertions))
  with
  | Ok () -> ()
  | Error cause when Eta.Cause.is_interrupt_only cause -> ()
  | Error cause -> fail_cause cause

let press_cycle ~clock ~keyboard ~audio_q ~delivered =
  Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
  offer_pcm audio_q;
  Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
  wait_until ~clock (fun () -> Atomic.get delivered <> [])

let test_record_to_delivery_last_good_then_reload () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let gen = Atomic.make 0 in
      let fail_reload = Atomic.make false in
      let model = Atomic.make "openrouter/owner-a" in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _attempts =
        if Atomic.get fail_reload then
          raise (Pie.Config.Stt.Config_error "synthetic reload failure");
        (base_config ~model:(Atomic.get model) (), stamp ())
      in
      let stt config _mode _bytes =
        Eta.Effect.pure config.Pie.Config.Stt.transcription_model
      in
      let deliver _config _mode text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let _socket, _retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          press_cycle ~clock ~keyboard ~audio_q ~delivered;
          Alcotest.(check (list string))
            "first delivery uses last-good model" [ "openrouter/owner-a" ]
            (Atomic.get delivered);
          Atomic.set delivered [];
          Atomic.set fail_reload true;
          Atomic.set model "openrouter/owner-b";
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          press_cycle ~clock ~keyboard ~audio_q ~delivered;
          Alcotest.(check (list string))
            "failed reload keeps last-good model" [ "openrouter/owner-a" ]
            (Atomic.get delivered);
          Atomic.set delivered [];
          Atomic.set fail_reload false;
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          press_cycle ~clock ~keyboard ~audio_q ~delivered;
          Alcotest.(check (list string))
            "successful reload reaches delivery" [ "openrouter/owner-b" ]
            (Atomic.get delivered)))

let test_required_input_failure_sets_last_error () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let delivered = Atomic.make [] in
      let stt _ _ _ = Eta.Effect.pure "should-not-deliver" in
      let deliver _config _mode text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          wait_until ~clock (fun () -> pcm_files retry_dir <> []);
          fail_audio audio_q
            (Pie.Pulse.Client.Protocol_error "SECRET_MARKER_user_audio"));
      match Eta_eio.run_once ~sw ~clock workflow with
      | Ok () -> Alcotest.fail "input failure must fail the workflow"
      | Error _ -> (
          Alcotest.(check (list string))
            "no delivery from truncated audio" [] (Atomic.get delivered);
          match status_last_error ~net ~sw socket_path with
          | Some msg ->
              Alcotest.(check string)
                "lastError is pulse protocol category" "pulse:protocol" msg;
              Alcotest.(check bool)
                "lastError omits injected payload" false
                (let needle = "SECRET_MARKER_user_audio" in
                 let rec loop i =
                   i <= String.length msg - String.length needle
                   && (String.sub msg i (String.length needle) = needle
                      || loop (i + 1))
                 in
                 loop 0)
          | None ->
              Alcotest.fail "expected coordinator lastError after input failure"
          ))

let test_recording_overload_cancels_attempt () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let delivered = Atomic.make [] in
      let stt _ _ _ = Eta.Effect.pure "should-not-deliver" in
      let deliver _config _mode text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          wait_until ~clock (fun () -> pcm_files retry_dir <> []);
          let path = List.hd (pcm_files retry_dir) in
          wait_until ~clock (fun () -> open_fds_for path > 0);
          fail_audio audio_q
            (Pie.Pulse.Client.Recording_overload
               { stream_index = 1; capacity = 64 }));
      match Eta_eio.run_once ~sw ~clock workflow with
      | Ok () -> Alcotest.fail "overload must fail the workflow"
      | Error _ -> (
          Alcotest.(check (list string))
            "no delivery from overloaded audio" [] (Atomic.get delivered);
          (match pcm_files retry_dir with
          | [] -> ()
          | path :: _ ->
              Alcotest.(check int)
                "retained writer closed" 0 (open_fds_for path));
          match status_last_error ~net ~sw socket_path with
          | Some msg ->
              Alcotest.(check string)
                "lastError is recording_overload category"
                "pulse:recording_overload" msg
          | None -> Alcotest.fail "expected lastError after Recording_overload"))

let test_cancel_waits_for_blocked_loader () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let mono_clock = Eio.Stdenv.mono_clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_dir (fun dir ->
      let keyboard = Pie.Keyboard.Monitor.create () in
      let _audio_q, audio = make_audio () in
      let entered = Atomic.make false in
      let left = Atomic.make false in
      let gate = Mutex.create () in
      Mutex.lock gate;
      let gen = Atomic.make 0 in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _attempts =
        if Atomic.get gen = 0 then (base_config (), stamp ())
        else (
          Atomic.set entered true;
          Mutex.lock gate;
          Atomic.set left true;
          Mutex.unlock gate;
          (base_config ~model:"openrouter/after" (), stamp ()))
      in
      let stt _ _ _ = Eta.Effect.pure "x" in
      let deliver _ _ _ = Eta.Effect.unit in
      let service_done, resolve_done = Eio.Promise.create () in
      let cancel_service = ref (fun () -> ()) in
      Eio.Switch.run @@ fun outer ->
      Eio.Fiber.fork ~sw:outer (fun () ->
          (match
             Eio.Switch.run @@ fun service_sw ->
             (cancel_service :=
                fun () ->
                  Eio.Fiber.fork ~sw:service_sw (fun () -> raise Cancel_service));
             let _socket, _retry, workflow =
               owner_workflow ~sw:service_sw ~net ~process_mgr ~mono_clock ~dir
                 ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
             in
             ignore (Eta_eio.run_once ~sw:service_sw ~clock workflow)
           with
          | () -> ()
          | exception Cancel_service -> ()
          | exception exn ->
              let name = Printexc.to_string exn in
              if String.length name >= 7 && String.sub name 0 7 = "Cancel." then
                ()
              else raise exn);
          Eio.Promise.resolve resolve_done ());
      Eio.Time.sleep clock 0.2;
      Atomic.incr gen;
      wait_until ~clock (fun () -> Atomic.get entered);
      Alcotest.(check bool)
        "loader still in systhread before release" false (Atomic.get left);
      !cancel_service ();
      Unix.sleepf 0.15;
      Alcotest.(check bool)
        "cancel still waiting on blocked loader" false (Atomic.get left);
      Mutex.unlock gate;
      wait_until ~clock (fun () -> Atomic.get left);
      Eio.Promise.await service_done;
      Alcotest.(check bool)
        "loader returned before service switch finished" true (Atomic.get left))

let test_cancelled_attempt_releases_recording_fd () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "unused" in
      let deliver _ _ _ = Eta.Effect.unit in
      let _socket, retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          wait_until ~clock (fun () -> pcm_files retry_dir <> []);
          let path = List.hd (pcm_files retry_dir) in
          wait_until ~clock (fun () -> open_fds_for path > 0);
          let socket =
            Eio.Net.connect ~sw net (`Unix (Filename.concat dir "control.sock"))
          in
          Eio.Flow.copy_string "{\"method\":\"Pause\"}\n" socket;
          Eio.Flow.close socket;
          wait_until ~clock (fun () -> open_fds_for path = 0);
          Alcotest.(check int)
            "recording fd released on cancel" 0 (open_fds_for path)))

let has_suffix suffix s =
  let sl = String.length suffix in
  let n = String.length s in
  n >= sl && String.sub s (n - sl) sl = suffix

let read_http_head flow =
  let scratch = Cstruct.create 1 in
  let buffer = Buffer.create 512 in
  let rec loop () =
    match Eio.Flow.single_read flow scratch with
    | 0 -> raise End_of_file
    | _ ->
        Buffer.add_char buffer (Cstruct.get_char scratch 0);
        let contents = Buffer.contents buffer in
        if has_suffix "\r\n\r\n" contents then contents else loop ()
    | exception End_of_file -> raise End_of_file
  in
  loop ()

let header_value name raw =
  let expected = String.lowercase_ascii name in
  raw |> String.split_on_char '\n'
  |> List.find_map (fun line ->
      let line =
        let len = String.length line in
        if len > 0 && Char.equal line.[len - 1] '\r' then
          String.sub line 0 (len - 1)
        else line
      in
      match String.split_on_char ':' line with
      | key :: rest when String.lowercase_ascii (String.trim key) = expected ->
          Some (String.trim (String.concat ":" rest))
      | _ -> None)

let write_ws_text flow payload =
  Eta_http_eio.Ws.Codec.encode
    { fin = true; opcode = Text; payload = Bytes.of_string payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_close flow =
  let payload = Bytes.create 2 in
  Bytes.set payload 0 (Char.chr 3);
  Bytes.set payload 1 (Char.chr 232);
  Eta_http_eio.Ws.Codec.encode { fin = true; opcode = Close; payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n\
     Upgrade: websocket\r\n\
     Connection: keep-alive, Upgrade\r\n\
     Sec-WebSocket-Accept: "
    ^ Eta_http_eio.Ws.Codec.accept_key ~sha1:Eta_http_tls_openssl.sha1 key
    ^ "\r\n\r\n"
  in
  Eio.Flow.copy_string response flow

let upgrade flow =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> failwith "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key

let stream_stage_env = "PIE_OWNER_STREAM_STAGE_DIR"

let mark_stage dir name =
  let oc =
    open_out_gen
      [ Open_creat; Open_trunc; Open_wronly ]
      0o600 (Filename.concat dir name)
  in
  close_out oc

let listed_stages dir =
  try Sys.readdir dir |> Array.to_list |> List.sort String.compare
  with Sys_error _ -> []

let stage_body dir name =
  try
    let ic = open_in_bin (Filename.concat dir name) in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  with _ -> ""

let stage_report dir =
  listed_stages dir
  |> List.map (fun name ->
      let body = String.trim (stage_body dir name) in
      if body = "" then name else name ^ "=" ^ body)
  |> String.concat ","

let json_event_type payload =
  match Yojson.Safe.from_string payload with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with Some (`String t) -> t | _ -> "")
  | _ -> ""
  | exception _ -> ""

type ws_frame = Text of string | Close | Other

let read_ws_frame flow =
  let read_exact len =
    let cs = Cstruct.create len in
    let rec loop off =
      if off < len then
        match Eio.Flow.single_read flow (Cstruct.sub cs off (len - off)) with
        | 0 -> raise End_of_file
        | n -> loop (off + n)
    in
    loop 0;
    Cstruct.to_bytes cs
  in
  let header = read_exact 2 in
  let opcode = Char.code (Bytes.get header 0) land 0x0f in
  let masked = Char.code (Bytes.get header 1) land 0x80 <> 0 in
  let len0 = Char.code (Bytes.get header 1) land 0x7f in
  let payload_len =
    if len0 = 126 then
      let ext = read_exact 2 in
      (Char.code (Bytes.get ext 0) lsl 8) lor Char.code (Bytes.get ext 1)
    else if len0 = 127 then (
      let ext = read_exact 8 in
      let v = ref 0 in
      for i = 0 to 7 do
        v := (!v lsl 8) lor Char.code (Bytes.get ext i)
      done;
      !v)
    else len0
  in
  let mask_key = if masked then Some (read_exact 4) else None in
  let payload = read_exact payload_len in
  (match mask_key with
  | Some key ->
      for i = 0 to payload_len - 1 do
        let b =
          Char.code (Bytes.get payload i)
          lxor Char.code (Bytes.get key (i mod 4))
        in
        Bytes.set payload i (Char.chr b)
      done
  | None -> ());
  match opcode with
  | 1 -> Text (Bytes.to_string payload)
  | 8 -> Close
  | _ -> Other

let session_updated = {|{"type":"session.updated"}|}

let transcript_done =
  {|{"type":"conversation.item.input_audio_transcription.completed","transcript":"synthetic-stream"}|}

let response_done =
  {|{"type":"response.done","response":{"status":"completed"}}|}

(* gpt-realtime-2 is Conversation mode: await session.updated, then audio
   append/commit, then response.create. Transcript_done does not stop receive;
   response.done does. Dumping those frames at upgrade time is a fixture error. *)
let serve_codex_conversation ~clock flow stage_dir =
  mark_stage stage_dir "tcp_accept";
  upgrade flow;
  mark_stage stage_dir "ws_upgrade";
  let rec loop ~committed =
    match read_ws_frame flow with
    | exception End_of_file -> ()
    | Close -> ()
    | Other -> loop ~committed
    | Text payload -> (
        match json_event_type payload with
        | "session.update" ->
            mark_stage stage_dir "session_update";
            write_ws_text flow session_updated;
            mark_stage stage_dir "session_ready";
            loop ~committed
        | "input_audio_buffer.append" | "session.input_audio_buffer.append" ->
            mark_stage stage_dir "audio_append";
            loop ~committed
        | "input_audio_buffer.commit" ->
            mark_stage stage_dir "commit";
            loop ~committed:true
        | "response.create" ->
            mark_stage stage_dir "response_create";
            if committed then (
              write_ws_text flow transcript_done;
              write_ws_text flow response_done;
              mark_stage stage_dir "transcript_sent";
              Eio.Time.sleep clock 0.2;
              write_ws_close flow)
            else loop ~committed
        | _ -> loop ~committed)
  in
  loop ~committed:false

let write_synthetic_codex_home dir =
  let home = Filename.concat dir "codex" in
  Unix.mkdir home 0o700;
  let oc = open_out_bin (Filename.concat home "auth.json") in
  output_string oc {|{"tokens":{"access_token":"synthetic-owner-token"}}|};
  close_out oc;
  home

(* Production streaming fetches niri focused-window before WS connect. Write
   the stub next to the test executable so it is not on a noexec temp fs. *)
let write_synthetic_niri () =
  let dir =
    Filename.concat (Filename.dirname Sys.executable_name) "niri-stub"
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat dir "niri" in
  let oc = open_out path in
  output_string oc "#!/bin/sh\n";
  output_string oc
    "touch \"$PIE_OWNER_STREAM_STAGE_DIR/niri_ran\" 2>/dev/null\n";
  output_string oc "echo null\n";
  close_out oc;
  Unix.chmod path 0o755;
  dir

let env_without keys =
  Unix.environment () |> Array.to_list
  |> List.filter (fun entry ->
      not
        (List.exists
           (fun key ->
             String.length entry >= String.length key + 1
             && String.sub entry 0 (String.length key) = key
             && entry.[String.length key] = '=')
           keys))
  |> Array.of_list

let run_stream_child () =
  let delivered = Atomic.make [] in
  let stage_dir = Sys.getenv_opt stream_stage_env in
  let mark name =
    match stage_dir with Some dir -> mark_stage dir name | None -> ()
  in
  let mark_text name text =
    match stage_dir with
    | None -> ()
    | Some dir ->
        let oc =
          open_out_gen
            [ Open_creat; Open_trunc; Open_wronly ]
            0o600 (Filename.concat dir name)
        in
        output_string oc text;
        close_out oc
  in
  mark_text "override"
    (Option.value
       (Sys.getenv_opt "CODEX_REALTIME_BASE_URL_OVERRIDE")
       ~default:"unset");
  mark_text "has_api_key"
    (match Sys.getenv_opt "OPENAI_API_KEY" with
    | Some v when String.trim v <> "" -> "yes"
    | _ -> "no");
  mark_text "codex_home"
    (Option.value (Sys.getenv_opt "CODEX_HOME") ~default:"unset");
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ =
        ( base_config ~provider:Pie.Config.Stt.Codex_realtime
            ~model:"gpt-realtime-2" (),
          stamp () )
      in
      let stt = Pie.Assistant.Service.production_stt ~sw ~net ~process_mgr in
      let deliver _config _mode text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let _socket, _retry, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      mark "workflow_started";
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.4;
          mark "press";
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          mark "pcm";
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          (* Capture post_roll_ms is 2000; finish_stream then waits for WS. *)
          wait_until ~steps:160 ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "production streaming branch delivered" [ "synthetic-stream" ]
            (Atomic.get delivered);
          mark "delivered"))

let spawn_stream_child ~port ~codex_home ~niri_dir ~stage_dir =
  let path =
    match Sys.getenv_opt "PATH" with
    | Some path -> niri_dir ^ ":" ^ path
    | None -> niri_dir
  in
  let env =
    Array.append
      (env_without
         [
           "OPENAI_API_KEY";
           "CODEX_HOME";
           "CODEX_REALTIME_BASE_URL_OVERRIDE";
           "PATH";
           stream_child_env;
           stream_stage_env;
         ])
      [|
        "OPENAI_API_KEY=synthetic-owner-token";
        "CODEX_HOME=" ^ codex_home;
        "PATH=" ^ path;
        "CODEX_REALTIME_BASE_URL_OVERRIDE=ws://127.0.0.1:" ^ string_of_int port;
        stream_child_env ^ "=1";
        stream_stage_env ^ "=" ^ stage_dir;
      |]
  in
  let exe = Sys.executable_name in
  let pid =
    Unix.create_process_env exe [| exe |] env Unix.stdin Unix.stdout Unix.stderr
  in
  snd (Unix.waitpid [] pid)

let test_streaming_codex_private_child () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let net = Eio.Stdenv.net env in
      let stage_dir = Filename.concat dir "stages" in
      Unix.mkdir stage_dir 0o700;
      let codex_home = write_synthetic_codex_home dir in
      let niri_dir = write_synthetic_niri () in
      let socket =
        Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, port) -> port
        | `Unix _ -> Alcotest.fail "expected TCP listener"
      in
      let served, resolve_served = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
          let flow, _addr = Eio.Net.accept ~sw socket in
          serve_codex_conversation ~clock flow stage_dir;
          Eio.Promise.resolve resolve_served ());
      (* Unix.waitpid must not block the Eio loop or accept never runs. *)
      let status =
        Eio_unix.run_in_systhread ~label:"pie.owner.stream-child" (fun () ->
            spawn_stream_child ~port ~codex_home ~niri_dir ~stage_dir)
      in
      match status with
      | Unix.WEXITED 0 ->
          Alcotest.(check bool)
            "niri not invoked before provider" false
            (List.mem "niri_ran" (listed_stages stage_dir));
          Eio.Promise.await served
      | Unix.WEXITED n ->
          Alcotest.(check bool)
            "niri not invoked before provider" false
            (List.mem "niri_ran" (listed_stages stage_dir));
          Alcotest.failf "stream child exited %d stages=[%s]" n
            (stage_report stage_dir)
      | Unix.WSIGNALED s -> Alcotest.failf "stream child signaled %d" s
      | Unix.WSTOPPED s -> Alcotest.failf "stream child stopped %d" s)

let test_keyboard_input_lost_cancels_attempt () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let delivered = Atomic.make [] in
      let stt _ _ _ = Eta.Effect.pure "should-not-deliver" in
      let deliver _config _mode text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let _socket, retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q);
      let monitor_loss =
        Eta.Effect.sync (fun () ->
            Eio.Time.sleep clock 0.45;
            raise Pie.Keyboard.Monitor.Input_lost)
      in
      match
        Eta_eio.run_once ~sw ~clock
          (Eta.Effect.with_background ~name:"pie.owner.monitor" monitor_loss
             (fun () -> workflow))
      with
      | Ok () -> Alcotest.fail "Input_lost must fail the owner workflow"
      | Error _ -> (
          Alcotest.(check (list string))
            "no delivery after keyboard Input_lost" [] (Atomic.get delivered);
          match pcm_files retry_dir with
          | [] -> ()
          | path :: _ ->
              Alcotest.(check int)
                "attempt writer closed after Input_lost" 0 (open_fds_for path)))

let rpc ~net ~sw socket_path method_ =
  let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> Eio.Flow.close socket)
    (fun () ->
      Eio.Flow.copy_string
        (Printf.sprintf "{\"method\":\"%s\"}\n" method_)
        socket;
      ignore (Eio.Buf_read.of_flow ~max_size:65536 socket |> Eio.Buf_read.line))

let test_pause_latch_one_shot_resume_fresh_attempt () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let persist_path = Filename.concat dir "recording.json" in
      let coord = Pie.Recording.Coordinator.create ~persist_path () in
      let start () =
        match
          Pie.Recording.Coordinator.try_start coord
            Pie.Recording.Coordinator.Ptt_transcribe ()
        with
        | Pie.Recording.Coordinator.Started attempt -> attempt
        | _ -> Alcotest.fail "expected Started"
      in
      let a1 = start () in
      Alcotest.(check bool)
        "fresh attempt unsignalled" false
        (Pie.Recording.Coordinator.cancel_signalled a1);
      Pie.Recording.Coordinator.set_enabled coord false;
      Alcotest.(check bool)
        "pause signals current attempt" true
        (Pie.Recording.Coordinator.cancel_signalled a1);
      (match
         Eta_eio.run_once ~sw ~clock (Pie.Recording.Coordinator.await_cancel a1)
       with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "await_cancel after pause should return");
      Pie.Recording.Coordinator.set_enabled coord true;
      Alcotest.(check bool)
        "resume does not erase pause latch" true
        (Pie.Recording.Coordinator.cancel_signalled a1);
      ignore (Pie.Recording.Coordinator.release coord a1);
      let a2 = start () in
      Alcotest.(check bool)
        "next attempt has a fresh latch" false
        (Pie.Recording.Coordinator.cancel_signalled a2);
      Alcotest.(check bool)
        "old cancel signal cannot stop new same-mode attempt" false
        (Pie.Recording.Coordinator.cancel_signalled a2))

let test_pause_resume_fresh_attempt_delivers () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "owned-text" in
      let deliver _config _mode text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let socket_path, _retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      let status key =
        try status_bool ~net ~sw socket_path key with _ -> false
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          wait_until ~clock (fun () -> status "enabled");
          press_cycle ~clock ~keyboard ~audio_q ~delivered;
          Alcotest.(check (list string))
            "first attempt delivered" [ "owned-text" ] (Atomic.get delivered);
          wait_until ~clock (fun () -> not (status "active"));
          Atomic.set delivered [];
          rpc ~net ~sw socket_path "Pause";
          wait_until ~clock (fun () -> not (status "enabled"));
          rpc ~net ~sw socket_path "Resume";
          wait_until ~clock (fun () -> status "enabled");
          press_cycle ~clock ~keyboard ~audio_q ~delivered;
          Alcotest.(check (list string))
            "fresh attempt after Pause/Resume delivered" [ "owned-text" ]
            (Atomic.get delivered)))

let wrap_pause_recovery ~attempt body =
  let pause =
    Pie.Recording.Coordinator.await_cancel attempt
    |> Eta.Effect.bind (fun () -> Eta.Effect.fail `Owned_pause)
  in
  Eta.Effect.with_background ~name:"pie.attempt.pause.test" pause (fun () ->
      body |> Eta.Effect.map_error (fun err -> `Body_error err))
  |> Pie.Ptt_loop.recover_single_failure (function
    | `Owned_pause -> Some Eta.Effect.unit
    | `Body_error _ -> None)
  |> Eta.Effect.map_error (function
    | `Owned_pause -> `Ptt_error "paused attempt failed during cleanup"
    | `Body_error err -> err)

let ptt_messages cause =
  Eta.Cause.failures cause |> List.map (function `Ptt_error msg -> msg)

let test_composite_body_failures_preserved () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let persist_path = Filename.concat dir "recording.json" in
      let coord = Pie.Recording.Coordinator.create ~persist_path () in
      let attempt =
        match
          Pie.Recording.Coordinator.try_start coord
            Pie.Recording.Coordinator.Ptt_transcribe ()
        with
        | Started a -> a
        | _ -> Alcotest.fail "start"
      in
      let body =
        Eta.Effect.par
          (Eta.Effect.fail (`Ptt_error "one"))
          (Eta.Effect.fail (`Ptt_error "two"))
        |> Eta.Effect.map (fun _ -> ())
      in
      let run e =
        match Eta_eio.run_once ~sw ~clock e with
        | Ok () -> []
        | Error cause -> List.sort String.compare (ptt_messages cause)
      in
      let direct = run body in
      let wrapped = run (wrap_pause_recovery ~attempt body) in
      Alcotest.(check bool) "body itself failed" true (direct <> []);
      Alcotest.(check (list string))
        "wrapper preserves the body's typed Fail set" direct wrapped)

let test_owned_pause_plus_cleanup_is_not_ok () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let persist_path = Filename.concat dir "recording.json" in
      let coord = Pie.Recording.Coordinator.create ~persist_path () in
      let attempt =
        match
          Pie.Recording.Coordinator.try_start coord
            Pie.Recording.Coordinator.Ptt_transcribe ()
        with
        | Started a -> a
        | _ -> Alcotest.fail "start"
      in
      let body =
        Eta.Effect.never
        |> Eta.Effect.on_exit (fun _ ->
            Eta.Effect.fail (`Ptt_error "close failed"))
      in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Time.sleep clock 0.05;
          Pie.Recording.Coordinator.set_enabled coord false);
      match Eta_eio.run_once ~sw ~clock (wrap_pause_recovery ~attempt body) with
      | Ok () -> Alcotest.fail "owned pause plus cleanup failure must not be Ok"
      | Error _ -> ())

let test_pulse_overload_and_cleanup_failures_preserved () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let pulse =
    Eta.Effect.fail
      (`Pulse_error
         (Pie.Pulse.Client.Recording_overload
            { stream_index = 1; capacity = 64 }))
  in
  let cleanup =
    Eta.Effect.fail (`Ptt_error "recording:close retained recording")
  in
  match Eta_eio.run_once ~sw ~clock (pulse |> Eta.Effect.finally cleanup) with
  | Ok () -> Alcotest.fail "composite Pulse and cleanup must stay failed"
  | Error cause -> (
      let overload =
        Eta.Cause.failures cause
        |> List.exists (function
          | `Pulse_error (Pie.Pulse.Client.Recording_overload _) -> true
          | _ -> false)
      in
      Alcotest.(check bool) "Pulse Recording_overload remains" true overload;
      match cause with
      | Eta.Cause.Suppressed { finalizer; _ } ->
          Alcotest.(check bool)
            "cleanup failure remains as finalizer" true
            (not (Eta.Cause.Finalizer.is_interrupt_only finalizer))
      | _ ->
          Alcotest.failf "expected Suppressed cleanup, got %a"
            (Eta.Cause.pp pp_run_error)
            cause)

let test_status_observer_defect_composes_with_pulse () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let pulse =
    Eta.Effect.fail
      (`Pulse_error
         (Pie.Pulse.Client.Recording_overload
            { stream_index = 1; capacity = 64 }))
  in
  let observe = Eta.Effect.sync (fun () -> failwith "set_error persist") in
  match Eta_eio.run_once ~sw ~clock (pulse |> Eta.Effect.finally observe) with
  | Ok () -> Alcotest.fail "observer defect must not succeed"
  | Error cause -> (
      let overload =
        Eta.Cause.failures cause
        |> List.exists (function
          | `Pulse_error (Pie.Pulse.Client.Recording_overload _) -> true
          | _ -> false)
      in
      Alcotest.(check bool)
        "original Pulse Recording_overload remains" true overload;
      match cause with
      | Eta.Cause.Suppressed { finalizer; _ } ->
          Alcotest.(check bool)
            "observer defect is composed as finalizer" true
            (not (Eta.Cause.Finalizer.is_interrupt_only finalizer))
      | _ ->
          Alcotest.failf "observer defect replaced the Pulse cause: %a"
            (Eta.Cause.pp pp_run_error)
            cause)

let json_accepted line =
  match Yojson.Safe.from_string line with
  | `Assoc fields -> (
      match List.assoc_opt "accepted" fields with
      | Some (`Bool b) -> b
      | _ -> Alcotest.fail "missing accepted")
  | _ -> Alcotest.fail "expected object"

let rpc_line ~net ~sw socket_path body =
  let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> Eio.Flow.close socket)
    (fun () ->
      Eio.Flow.copy_string (body ^ "\n") socket;
      Eio.Buf_read.of_flow ~max_size:65536 socket |> Eio.Buf_read.line)

let test_service_pre_handle_failure_then_fresh_press () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let persist_path = Filename.concat dir "recording.json" in
      let coordinator = Pie.Recording.Coordinator.create ~persist_path () in
      let bad =
        Pie.Assistant.Service.on_press ~sw ~net ~process_mgr ~coordinator
          ~retry_dir:dir
          ~config:(base_config ~model:"openrouter/" ())
          "transcribe"
      in
      (match Eta_eio.run_once ~sw ~clock bad with
      | Ok (Pie.Ptt_loop.Scoped use) -> (
          match
            Eta_eio.run_once ~sw ~clock (use (fun _ -> Eta.Effect.unit))
          with
          | Error _ -> ()
          | Ok () -> Alcotest.fail "empty model must fail before handle")
      | Ok _ -> Alcotest.fail "empty model must fail before handle"
      | Error _ -> ());
      Alcotest.(check bool)
        "pre-handle failure released admission" false
        (Pie.Recording.Coordinator.snapshot coordinator)
          .Pie.Recording.Coordinator.active;
      let delivered = Atomic.make [] in
      let stt _ _ _ = Eta.Effect.pure "fresh-after-setup-fail" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let good =
        Pie.Assistant.Service.on_press ~sw ~net ~process_mgr ~coordinator
          ~retry_dir:dir ~stt ~deliver ~config:(base_config ()) "transcribe"
      in
      (match Eta_eio.run_once ~sw ~clock good with
      | Ok (Pie.Ptt_loop.Scoped use) -> (
          match
            Eta_eio.run_once ~sw ~clock
              (use (fun handle ->
                   handle.offer loud_pcm
                   |> Eta.Effect.bind (fun () ->
                       match handle.finish with
                       | Pie.Ptt_loop.Buffered f ->
                           f
                             {
                               Pie.Ptt_loop.duration_ms = 10;
                               pcm_bytes = loud_pcm;
                             }
                       | Pie.Ptt_loop.Streaming _ ->
                           Alcotest.fail "expected Buffered finish")))
          with
          | Ok () -> ()
          | Error cause -> fail_cause cause)
      | Ok _ -> Alcotest.fail "expected Scoped after fresh press"
      | Error cause -> fail_cause cause);
      Alcotest.(check (list string))
        "fresh press after pre-handle failure delivered"
        [ "fresh-after-setup-fail" ]
        (Atomic.get delivered))

let test_delayed_old_cleanup_preserves_new_same_mode_attempt () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let persist_path = Filename.concat dir "recording.json" in
      let coordinator = Pie.Recording.Coordinator.create ~persist_path () in
      let first =
        match
          Pie.Recording.Coordinator.try_start coordinator
            Pie.Recording.Coordinator.Ptt_transcribe ()
        with
        | Started attempt -> attempt
        | _ -> Alcotest.fail "first start"
      in
      let store =
        match Pie.Recording.Retry_store.Active.open_store ~root:dir with
        | Ok store -> store
        | Error (`Retry_store msg) -> Alcotest.fail msg
      in
      let owner_a =
        match
          Pie.Assistant.Service.create_attempt_owner ~store ~coordinator
            ~attempt:first Pie.Assistant.Service.Transcribe
        with
        | Ok owner -> owner
        | Error (`Ptt_error msg) -> Alcotest.fail msg
      in
      ignore (Pie.Recording.Coordinator.release coordinator first);
      let second =
        Pie.Assistant.Service.on_press ~sw ~net ~process_mgr ~coordinator
          ~retry_dir:dir ~config:(base_config ()) "transcribe"
      in
      (match Eta_eio.run_once ~sw ~clock second with
      | Ok (Pie.Ptt_loop.Scoped _) -> ()
      | Ok _ -> Alcotest.fail "expected Scoped for second attempt"
      | Error cause -> fail_cause cause);
      let second_id =
        match Pie.Recording.Coordinator.current_attempt coordinator with
        | Some attempt -> Pie.Recording.Coordinator.attempt_id attempt
        | None -> Alcotest.fail "second attempt missing"
      in
      (match
         Eta_eio.run_once ~sw ~clock
           (Pie.Assistant.Service.settle_attempt owner_a)
       with
      | Ok () -> ()
      | Error cause -> fail_cause cause);
      Alcotest.(check bool)
        "delayed first cleanup left second admitted" true
        (Pie.Recording.Coordinator.snapshot coordinator)
          .Pie.Recording.Coordinator.active;
      match Pie.Recording.Coordinator.current_attempt coordinator with
      | Some attempt ->
          Alcotest.(check int)
            "second attempt identity unchanged" second_id
            (Pie.Recording.Coordinator.attempt_id attempt)
      | None -> Alcotest.fail "second attempt cleared by stale settle")

let test_busy_keyboard_and_rpc_then_fresh_cycle () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "cycle-text" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          wait_until ~clock (fun () -> pcm_files retry_dir <> []);
          let line =
            rpc_line ~net ~sw socket_path
              "{\"method\":\"Action\",\"action\":\"ptt-start\"}"
          in
          Alcotest.(check bool)
            "RPC ptt-start rejected while keyboard attempt active" false
            (json_accepted line);
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          Atomic.set delivered [];
          press_cycle ~clock ~keyboard ~audio_q ~delivered;
          Alcotest.(check (list string))
            "fresh cycle after busy RPC/keyboard delivered" [ "cycle-text" ]
            (Atomic.get delivered)))

let test_cleanup_cancel_and_defect_both_visible () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let map_io = function
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | Unix.Unix_error _ | Sys_error _ ->
        `Ptt_error "recording:close retained recording"
    | exn -> raise exn
  in
  let defect = Eta.Effect.sync (fun () -> failwith "cleanup-defect") in
  let io =
    Eta.Effect.sync_exn
      (fun () -> raise (Unix.Unix_error (Unix.EIO, "close", "")))
      ~catch:map_io
  in
  match Eta_eio.run_once ~sw ~clock (defect |> Eta.Effect.finally io) with
  | Ok () -> Alcotest.fail "cancel and defect must stay failed"
  | Error cause -> (
      Alcotest.(check bool)
        "not two Ptt_error strings only" true
        (Eta.Cause.defects cause <> [] || Eta.Cause.interruptors cause <> []);
      match cause with
      | Eta.Cause.Suppressed { finalizer; _ } ->
          Alcotest.(check bool)
            "composed cleanup remains" true
            (not (Eta.Cause.Finalizer.is_interrupt_only finalizer))
      | Eta.Cause.Die _ -> Alcotest.fail "cancel replaced the composite"
      | _ -> ())

let test_capture_budget_sets_last_error () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mono_clock = Eio.Stdenv.mono_clock env in
      let net = Eio.Stdenv.net env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "should-not-deliver" in
      let deliver _ _ _ = Eta.Effect.unit in
      let socket_path = Filename.concat dir "control.sock" in
      let retry_dir = Filename.concat dir "recordings" in
      (try Unix.mkdir retry_dir 0o700
       with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      let workflow =
        Pie.Assistant.Service.run_with_inputs ~sw ~net ~process_mgr ~mono_clock
          ~inputs:
            {
              Pie.Assistant.Service.keyboard;
              audio;
              source_name = Some "test-source";
            }
          ~config_control:{ stamp; load } ~stt ~deliver ~socket_path ~retry_dir
          ~min_duration_ms:0 ~capture_pcm_bytes:8 ()
      in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true));
      match
        Eta_eio.run_once ~sw ~clock
          (workflow
          |> Eta.Effect.timeout_as (Eta.Duration.seconds 6)
               ~on_timeout:(`Ptt_error "timeout"))
      with
      | Ok () -> Alcotest.fail "capture budget must fail the workflow"
      | Error cause -> (
          match status_last_error ~net ~sw socket_path with
          | Some msg ->
              Alcotest.(check string)
                "budget lastError is content-free" "ptt:capture_budget" msg
          | None ->
              Alcotest.failf "expected lastError after capture budget; cause=%a"
                (Eta.Cause.pp pp_run_error)
                cause))

let peek_kind retry_dir =
  match Pie.Recording.Retry_store.Active.open_store ~root:retry_dir with
  | Error _ -> None
  | Ok store -> (
      match Pie.Recording.Retry_store.Active.peek store with
      | Ok (Some (Pie.Recording.Retry_store.Active.Audio _)) -> Some "audio"
      | Ok
          (Some
             (Pie.Recording.Retry_store.Active.Text { dispatch_uncertain; _ }))
        ->
          Some (if dispatch_uncertain then "text-uncertain" else "text")
      | _ -> None)

let rpc ~net ~sw socket_path body =
  let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> Eio.Flow.close socket)
    (fun () ->
      Eio.Flow.copy_string (body ^ "\n") socket;
      Eio.Buf_read.of_flow ~max_size:65536 socket |> Eio.Buf_read.line)

let test_provider_failure_publishes_audio_then_retries_provider () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let stt_n = Atomic.make 0 in
      let deliver_n = Atomic.make 0 in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Eta.Effect.fail (`Ptt_error "stt:unavailable")
      in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        Eta.Effect.unit
      in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          wait_until ~clock (fun () -> peek_kind retry_dir = Some "audio");
          Alcotest.(check int) "provider called once" 1 (Atomic.get stt_n);
          Alcotest.(check int) "no delivery" 0 (Atomic.get deliver_n);
          ignore
            (rpc ~net:(Eio.Stdenv.net env) ~sw socket_path
               {|{"method":"Action","action":"retry"}|});
          wait_until ~clock (fun () -> Atomic.get stt_n >= 2);
          Alcotest.(check int) "audio retry calls provider" 2 (Atomic.get stt_n);
          Alcotest.(check int) "still no delivery" 0 (Atomic.get deliver_n)))

let test_delivery_failure_publishes_text_retry_skips_provider () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let stt_n = Atomic.make 0 in
      let deliver_n = Atomic.make 0 in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Eta.Effect.pure "recognized-verbatim"
      in
      let delivered = ref [] in
      let deliver _ _ text =
        Atomic.incr deliver_n;
        delivered := text :: !delivered;
        Eta.Effect.fail (`Ptt_error "inject:failed")
      in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          wait_until ~clock (fun () ->
              peek_kind retry_dir = Some "text-uncertain");
          Alcotest.(check int) "provider once" 1 (Atomic.get stt_n);
          Alcotest.(check int) "delivery attempted" 1 (Atomic.get deliver_n);
          Alcotest.(check int)
            "no extra attempt before RPC" 1 (Atomic.get deliver_n);
          ignore
            (rpc ~net:(Eio.Stdenv.net env) ~sw socket_path
               {|{"method":"Action","action":"retry"}|});
          wait_until ~clock (fun () -> Atomic.get deliver_n >= 2);
          Alcotest.(check int)
            "explicit retry delivers once more" 2 (Atomic.get deliver_n);
          Alcotest.(check int)
            "explicit text retry skips provider" 1 (Atomic.get stt_n);
          Alcotest.(check (list string))
            "exact retained text"
            [ "recognized-verbatim"; "recognized-verbatim" ]
            (List.rev !delivered)))

let seed_text retry_dir text =
  (try Unix.mkdir retry_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  match Pie.Recording.Retry_store.Active.open_store ~root:retry_dir with
  | Error (`Retry_store msg) -> Alcotest.fail msg
  | Ok store -> (
      match
        Pie.Recording.Retry_store.Active.publish_text store
          ~mode:Pie.Recording.Retry_store.Transcribe ~text
          ~dispatch_uncertain:false
      with
      | Ok generation -> generation
      | Error (`Retry_store msg) -> Alcotest.fail msg)

let load_text_payload retry_dir =
  match Pie.Recording.Retry_store.Active.open_store ~root:retry_dir with
  | Error (`Retry_store msg) -> Alcotest.fail msg
  | Ok store -> (
      match Pie.Recording.Retry_store.Active.peek store with
      | Ok (Some (Pie.Recording.Retry_store.Active.Text { generation; _ })) -> (
          match Pie.Recording.Retry_store.Active.load_text store generation with
          | Ok text -> text
          | Error (`Retry_store msg) -> Alcotest.fail msg)
      | Ok (Some _) -> Alcotest.fail "expected Text candidate"
      | Ok None -> Alcotest.fail "expected retained text"
      | Error (`Retry_store msg) -> Alcotest.fail msg)

let test_blank_transcript_publishes_audio_without_inject () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let deliver_n = Atomic.make 0 in
      let stt_n = Atomic.make 0 in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Eta.Effect.pure (String.make 3 ' ')
      in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        Eta.Effect.unit
      in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let _socket, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          wait_until ~clock (fun () -> Atomic.get stt_n >= 1);
          Alcotest.(check int) "blank skips inject" 0 (Atomic.get deliver_n);
          wait_until ~clock (fun () -> peek_kind retry_dir = Some "audio")))

let test_abort_does_not_publish () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let stt _ _ _ = Eta.Effect.pure "should-not-run" in
      let deliver _ _ _ = Eta.Effect.unit in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          ignore
            (rpc ~net:(Eio.Stdenv.net env) ~sw socket_path
               {|{"method":"Pause"}|});
          Eio.Time.sleep clock 0.2;
          Alcotest.(check (option string))
            "abort publishes nothing" None (peek_kind retry_dir)))

let test_startup_text_retry_skips_provider () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let retry_dir = Filename.concat dir "recordings" in
      ignore (seed_text retry_dir "startup-retained");
      let stt_n = Atomic.make 0 in
      let delivered = ref [] in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Eta.Effect.pure "should-not-recognize"
      in
      let deliver _ _ text =
        delivered := text :: !delivered;
        Eta.Effect.unit
      in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let _audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          ignore
            (rpc ~net:(Eio.Stdenv.net env) ~sw socket_path
               {|{"method":"Action","action":"retry"}|});
          wait_until ~clock (fun () -> List.length !delivered >= 1);
          Alcotest.(check int)
            "startup text retry skips provider" 0 (Atomic.get stt_n);
          Alcotest.(check (list string))
            "startup text delivered" [ "startup-retained" ]
            (List.rev !delivered);
          Alcotest.(check (option string))
            "successful text retry clears candidate" None (peek_kind retry_dir)))

let test_successive_failures_keep_latest_only () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let n = Atomic.make 0 in
      let stt _ _ _ =
        let i = Atomic.fetch_and_add n 1 in
        Eta.Effect.pure (if i = 0 then "first-text" else "second-text")
      in
      let deliver _ _ _ = Eta.Effect.fail (`Ptt_error "inject:failed") in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let _socket, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          let cycle () =
            Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
            offer_pcm audio_q;
            Pie.Keyboard.Monitor.emit keyboard (key ~released:true)
          in
          Eio.Time.sleep clock 0.2;
          cycle ();
          wait_until ~clock (fun () ->
              peek_kind retry_dir = Some "text-uncertain");
          wait_until ~clock (fun () ->
              try
                not
                  (status_bool ~net:(Eio.Stdenv.net env) ~sw
                     (Filename.concat dir "control.sock")
                     "active")
              with _ -> false);
          cycle ();
          wait_until ~clock (fun () ->
              try load_text_payload retry_dir = "second-text" with _ -> false);
          Alcotest.(check string)
            "latest text only" "second-text"
            (load_text_payload retry_dir)))

let test_busy_and_disabled_retry_does_not_load_payload () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let stt _ _ _ = Eta.Effect.pure "held" in
      let deliver _ _ _ = Eta.Effect.never in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let socket_path, _retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          let busy =
            rpc ~net:(Eio.Stdenv.net env) ~sw socket_path
              {|{"method":"Action","action":"retry"}|}
          in
          Alcotest.(check bool)
            "busy retry rejected without payload load" true
            (String.length busy > 0);
          ignore
            (rpc ~net:(Eio.Stdenv.net env) ~sw socket_path
               {|{"method":"Pause"}|});
          let paused =
            rpc ~net:(Eio.Stdenv.net env) ~sw socket_path
              {|{"method":"Action","action":"retry"}|}
          in
          Alcotest.(check bool)
            "disabled retry rejected" true
            (String.length paused > 0)))

let dir_mode path = Unix.stat path |> fun st -> st.Unix.st_perm land 0o777

let test_nested_missing_parent_is_private () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let hist = Filename.concat dir "share" in
      Unix.mkdir hist 0o755;
      Unix.chmod hist 0o755;
      let pie_dir = Filename.concat hist "pie" in
      let retry_dir = Filename.concat pie_dir "failed-recordings" in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.fail (`Ptt_error "stt:unavailable") in
      let deliver _ _ _ = Eta.Effect.unit in
      let socket_path = Filename.concat dir "control.sock" in
      (try Unix.mkdir (Filename.concat dir "recordings") 0o700
       with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      let workflow =
        Pie.Assistant.Service.run_with_inputs ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~inputs:
            {
              Pie.Assistant.Service.keyboard;
              audio;
              source_name = Some "test-source";
            }
          ~config_control:{ stamp; load } ~stt ~deliver ~socket_path ~retry_dir
          ~min_duration_ms:0 ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          wait_until ~clock (fun () -> peek_kind retry_dir = Some "audio");
          Alcotest.(check int)
            "historical parent stays 0755" 0o755 (dir_mode hist);
          Alcotest.(check int)
            "created pie dir is 0700" 0o700 (dir_mode pie_dir);
          Alcotest.(check int)
            "created retry root is 0700" 0o700 (dir_mode retry_dir)))

let test_blueprint_on_press_does_not_publish () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let retry_dir = Filename.concat dir "recordings" in
      Unix.mkdir retry_dir 0o700;
      let persist_path = Filename.concat dir "recording-state.json" in
      let coordinator = Pie.Recording.Coordinator.create ~persist_path () in
      let stt _ _ _ = Eta.Effect.pure "x" in
      let deliver _ _ _ = Eta.Effect.unit in
      let _blueprint =
        Pie.Assistant.Service.on_press ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~coordinator ~retry_dir ~config:(base_config ()) ~stt ~deliver
          "transcribe"
      in
      Alcotest.(check (option string))
        "constructing on_press publishes nothing" None (peek_kind retry_dir))

let test_success_preserves_older_candidate () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let net = Eio.Stdenv.net env in
      let retry_dir = Filename.concat dir "recordings" in
      let gen0 = seed_text retry_dir "older-candidate" in
      let deliver_n = Atomic.make 0 in
      let stt _ _ _ = Eta.Effect.pure "fresh-success" in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        Eta.Effect.unit
      in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          wait_until ~clock (fun () -> Sys.file_exists socket_path);
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          wait_until ~clock (fun () ->
              offer_pcm audio_q;
              pcm_files retry_dir <> []);
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          wait_until ~clock (fun () -> Atomic.get deliver_n >= 1);
          wait_until ~clock (fun () ->
              not (status_bool ~net ~sw socket_path "active"));
          Alcotest.(check string)
            "older candidate payload retained" "older-candidate"
            (load_text_payload retry_dir);
          match Pie.Recording.Retry_store.Active.open_store ~root:retry_dir with
          | Error (`Retry_store msg) -> Alcotest.fail msg
          | Ok store -> (
              match Pie.Recording.Retry_store.Active.peek store with
              | Ok
                  (Some
                     (Pie.Recording.Retry_store.Active.Text { generation; _ }))
                when Pie.Recording.Retry_store.Active.generation_equal generation
                       gen0 ->
                  ()
              | _ -> Alcotest.fail "older generation changed after success")))

let test_publication_failure_keeps_primary () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let pub014_lock retry_dir =
        Filename.concat (Filename.concat retry_dir "active") "publish.lock"
      in
      let pub014_install retry_dir =
        let path = pub014_lock retry_dir in
        let saved = path ^ ".owner014-saved" in
        (try Unix.unlink saved with Unix.Unix_error _ -> ());
        match Unix.lstat path with
        | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
            Unix.mkdir path 0o700;
            `Created
        | { Unix.st_kind = Unix.S_REG; _ } ->
            Unix.rename path saved;
            Unix.mkdir path 0o700;
            `Replaced saved
        | { Unix.st_kind = Unix.S_DIR; _ } -> `Created
        | _ ->
            Unix.unlink path;
            Unix.mkdir path 0o700;
            `Created
      in
      let pub014_restore retry_dir save =
        let path = pub014_lock retry_dir in
        (try Unix.rmdir path with Unix.Unix_error _ -> ());
        match save with
        | `Created -> ()
        | `Replaced saved -> (
            try Unix.rename saved path with Unix.Unix_error _ -> ())
      in
      let rec pub014_has_stt = function
        | Eta.Cause.Fail (`Ptt_error "stt:unavailable") -> true
        | Sequential xs | Concurrent xs -> List.exists pub014_has_stt xs
        | Suppressed { primary; _ } -> pub014_has_stt primary
        | Fail _ | Die _ | Interrupt _ | Finalizer _ -> false
      in
      let rec pub014_has_pub_fail = function
        | Eta.Cause.Suppressed { finalizer = Eta.Cause.Finalizer.Fail _; _ } ->
            true
        | Sequential xs | Concurrent xs -> List.exists pub014_has_pub_fail xs
        | Suppressed { primary; _ } -> pub014_has_pub_fail primary
        | Finalizer f -> pub014_fin_fail f
        | Fail _ | Die _ | Interrupt _ -> false
      and pub014_fin_fail = function
        | Eta.Cause.Finalizer.Fail _ -> true
        | Sequential xs | Concurrent xs -> List.exists pub014_fin_fail xs
        | Suppressed { primary; finalizer } ->
            pub014_fin_fail primary || pub014_fin_fail finalizer
        | Finalizer f -> pub014_fin_fail f
        | Die _ | Interrupt _ -> false
      in
      let stt_n = Atomic.make 0 in
      let lock_save = ref `Created in
      let retry_dir_ref = ref "" in
      let stt _ _ _ =
        Eta.Effect.sync (fun () ->
            Atomic.incr stt_n;
            lock_save := pub014_install !retry_dir_ref)
        |> Eta.Effect.bind (fun () ->
               Eta.Effect.fail (`Ptt_error "stt:unavailable"))
      in
      let deliver _ _ _ = Eta.Effect.unit in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let _socket, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      retry_dir_ref := retry_dir;
      let never, _ = Eio.Promise.create () in
      Fun.protect
        ~finally:(fun () -> pub014_restore retry_dir !lock_save)
        (fun () ->
          match
            Eta_eio.run_once ~sw ~clock
              (Eta.Effect.with_background ~name:"pie.owner.publish-fail" workflow
                 (fun () ->
                   Eta.Effect.sync (fun () ->
                       wait_until ~clock (fun () -> Sys.file_exists _socket);
                       Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
                       wait_until ~clock (fun () ->
                           offer_pcm audio_q;
                           pcm_files retry_dir <> []);
                       Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
                       Eio.Promise.await never))
              |> Eta.Effect.timeout_as (Eta.Duration.seconds 8)
                   ~on_timeout:(`Ptt_error "timeout"))
          with
          | Ok () -> Alcotest.fail "expected publication compound"
          | Error cause when Eta.Cause.is_interrupt_only cause ->
              Alcotest.fail "publication failure must not be interrupt-only"
          | Error cause ->
              (match Eta.Cause.failures cause with
              | `Ptt_error "timeout" :: _ | _ :: `Ptt_error "timeout" :: _ ->
                  Alcotest.fail "deadline"
              | _ -> ());
              if
                (match cause with
                | Eta.Cause.Fail (`Ptt_error "timeout") -> true
                | _ -> false)
              then Alcotest.fail "deadline";
              Alcotest.(check bool)
                "provider fault invoked" true
                (Atomic.get stt_n >= 1);
              Alcotest.(check bool)
                "primary stt failure retained" true (pub014_has_stt cause);
              Alcotest.(check bool)
                "publication finalizer Fail composed" true
                (pub014_has_pub_fail cause)))

let test_owned_cancel_cleanup_defect_preserves_older () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let retry_dir = Filename.concat dir "recordings" in
      ignore (seed_text retry_dir "older-candidate");
      let started = Atomic.make false in
      let proceed, resolve_proceed = Eio.Promise.create () in
      let stt _ _ _ =
        Eta.Effect.sync (fun () ->
            Atomic.set started true;
            Eio.Promise.await proceed;
            failwith "provider-cleanup-defect")
      in
      let deliver _ _ _ = Eta.Effect.unit in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      Eio.Fiber.fork ~sw (fun () ->
          match Eta_eio.run_once ~sw ~clock workflow with
          | Ok () -> ()
          | Error _ -> ());
      Eio.Time.sleep clock 0.2;
      Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
      offer_pcm audio_q;
      Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
      wait_until ~clock (fun () -> Atomic.get started);
      let pause_sock =
        Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path)
      in
      Fun.protect
        ~finally:(fun () -> Eio.Flow.close pause_sock)
        (fun () -> Eio.Flow.copy_string "{\"method\":\"Pause\"}\n" pause_sock);
      Eio.Promise.resolve resolve_proceed ();
      wait_until ~clock (fun () ->
          try load_text_payload retry_dir = "older-candidate" with _ -> false);
      Alcotest.(check string)
        "older candidate unchanged" "older-candidate"
        (load_text_payload retry_dir);
      Alcotest.(check bool)
        "did not publish new audio" true
        (peek_kind retry_dir <> Some "audio"))

let test_admission_held_until_provider_finalizer () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let persist_path = Filename.concat dir "recording.json" in
      let coordinator = Pie.Recording.Coordinator.create ~persist_path () in
      let attempt =
        match
          Pie.Recording.Coordinator.try_start coordinator
            Pie.Recording.Coordinator.Ptt_transcribe ()
        with
        | Started a -> a
        | _ -> Alcotest.fail "expected Started"
      in
      let store =
        match Pie.Recording.Retry_store.Active.open_store ~root:dir with
        | Ok store -> store
        | Error (`Retry_store msg) -> Alcotest.fail msg
      in
      let owner =
        match
          Pie.Assistant.Service.create_attempt_owner ~store ~coordinator
            ~attempt Pie.Assistant.Service.Transcribe
        with
        | Ok owner -> owner
        | Error (`Ptt_error msg) -> Alcotest.fail msg
      in
      let held, release_hold = Eio.Promise.create () in
      let finish_returned = Atomic.make false in
      let inner : Pie.Ptt_loop.handle =
        {
          offer = (fun _ -> Eta.Effect.unit);
          finish =
            Pie.Ptt_loop.Buffered
              (fun _clip ->
                Eta.Effect.sync (fun () -> Atomic.set finish_returned true));
          cancel = Eta.Effect.unit;
        }
      in
      let wrapped = Pie.Assistant.Service.attempt_handle owner inner in
      Eio.Fiber.fork ~sw (fun () ->
          match
            Eta_eio.run_once ~sw ~clock
              (Pie.Assistant.Service.run_acquired_attempt ~attempt
                 ~on_settle:(Pie.Assistant.Service.settle_attempt owner)
                 (match wrapped.Pie.Ptt_loop.finish with
                 | Pie.Ptt_loop.Buffered f ->
                     f { Pie.Ptt_loop.duration_ms = 10; pcm_bytes = loud_pcm }
                     |> Eta.Effect.finally
                          (Eta.Effect.sync (fun () -> Eio.Promise.await held))
                 | Pie.Ptt_loop.Streaming _ ->
                     Alcotest.fail "expected Buffered finish"))
          with
          | Ok () -> ()
          | Error cause -> fail_cause cause);
      wait_until ~clock (fun () -> Atomic.get finish_returned);
      (match
         Pie.Recording.Coordinator.try_start coordinator
           Pie.Recording.Coordinator.Ptt_transcribe ()
       with
      | Pie.Recording.Coordinator.Busy _ -> ()
      | Pie.Recording.Coordinator.Started _ ->
          Alcotest.fail "admission released before provider finalizer"
      | Pie.Recording.Coordinator.Disabled ->
          Alcotest.fail "coordinator disabled");
      Eio.Promise.resolve release_hold ();
      wait_until ~clock (fun () ->
          not
            (Pie.Recording.Coordinator.snapshot coordinator)
              .Pie.Recording.Coordinator.active);
      match
        Pie.Recording.Coordinator.try_start coordinator
          Pie.Recording.Coordinator.Ptt_transcribe ()
      with
      | Pie.Recording.Coordinator.Started _ -> ()
      | _ -> Alcotest.fail "admission still held after provider finalizer")

let inject_cause cause =
  Eta.Effect.async ~register:(fun resume ->
      resume (Eta.Exit.Error cause);
      None)

let test_recover_single_intended_fail () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let recovered = Atomic.make false in
  let eff =
    Eta.Effect.fail (`Ptt_error "stt:unavailable")
    |> Pie.Ptt_loop.recover_single_failure (function
      | `Ptt_error "stt:unavailable" ->
          Atomic.set recovered true;
          Some Eta.Effect.unit
      | _ -> None)
  in
  (match Eta_eio.run_once ~sw ~clock eff with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "single Fail should recover");
  Alcotest.(check bool) "handler ran" true (Atomic.get recovered)

let test_recover_concurrent_ordinary_and_budget () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let ran = Atomic.make false in
  let cause =
    Eta.Cause.concurrent
      [
        Eta.Cause.fail (`Ptt_error "stt:unavailable");
        Eta.Cause.fail (`Ptt_error "pcm:capture_budget");
      ]
  in
  let eff =
    inject_cause cause
    |> Pie.Ptt_loop.recover_single_failure (fun _ ->
        Atomic.set ran true;
        Some Eta.Effect.unit)
  in
  match Eta_eio.run_once ~sw ~clock eff with
  | Ok () -> Alcotest.fail "concurrent Fail pair must not recover"
  | Error err ->
      Alcotest.(check bool) "handler not run" false (Atomic.get ran);
      let rendered =
        Format.asprintf "%a"
          (Eta.Cause.pp (fun fmt (`Ptt_error m) -> Format.pp_print_string fmt m))
          err
      in
      let has s =
        let rec loop i =
          i <= String.length rendered - String.length s
          && (String.sub rendered i (String.length s) = s || loop (i + 1))
        in
        loop 0
      in
      Alcotest.(check bool) "ordinary retained" true (has "stt:unavailable");
      Alcotest.(check bool) "budget retained" true (has "pcm:capture_budget")

let test_recover_concurrent_pause_and_body () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let ran = Atomic.make false in
  let cause =
    Eta.Cause.concurrent
      [
        Eta.Cause.fail `Owned_pause;
        Eta.Cause.fail (`Body_error (`Ptt_error "inject:failed"));
      ]
  in
  let eff =
    inject_cause cause
    |> Pie.Ptt_loop.recover_single_failure (function
      | `Owned_pause ->
          Atomic.set ran true;
          Some Eta.Effect.unit
      | `Body_error _ -> None)
  in
  match Eta_eio.run_once ~sw ~clock eff with
  | Ok () -> Alcotest.fail "Owned_pause+Body_error must not be quiet Ok"
  | Error _ -> Alcotest.(check bool) "handler not run" false (Atomic.get ran)

let test_recover_sequential_typed_pair () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let ran = Atomic.make false in
  let cause =
    Eta.Cause.sequential
      [
        Eta.Cause.fail (`Ptt_error "stt:unavailable");
        Eta.Cause.fail (`Ptt_error "inject:failed");
      ]
  in
  let eff =
    inject_cause cause
    |> Pie.Ptt_loop.recover_single_failure (fun _ ->
        Atomic.set ran true;
        Some Eta.Effect.unit)
  in
  match Eta_eio.run_once ~sw ~clock eff with
  | Ok () -> Alcotest.fail "sequential Fail pair must not recover"
  | Error err ->
      Alcotest.(check bool) "handler not run" false (Atomic.get ran);
      let rendered =
        Format.asprintf "%a"
          (Eta.Cause.pp (fun fmt (`Ptt_error m) -> Format.pp_print_string fmt m))
          err
      in
      let has s =
        let rec loop i =
          i <= String.length rendered - String.length s
          && (String.sub rendered i (String.length s) = s || loop (i + 1))
        in
        loop 0
      in
      Alcotest.(check bool) "first retained" true (has "stt:unavailable");
      Alcotest.(check bool) "second retained" true (has "inject:failed")

let () =
  match Sys.getenv_opt stream_child_env with
  | Some "1" -> run_stream_child ()
  | _ ->
      Alcotest.run "assistant-owner"
        [
          ( "reload",
            [
              Alcotest.test_case
                "record-to-delivery last-good then successful reload" `Quick
                test_record_to_delivery_last_good_then_reload;
            ] );
          ( "input",
            [
              Alcotest.test_case "required input failure sets lastError" `Quick
                test_required_input_failure_sets_last_error;
              Alcotest.test_case "Recording_overload cancels attempt" `Quick
                test_recording_overload_cancels_attempt;
              Alcotest.test_case "keyboard Input_lost cancels attempt" `Quick
                test_keyboard_input_lost_cancels_attempt;
            ] );
          ( "cancel",
            [
              Alcotest.test_case "cancel waits for blocked config loader" `Quick
                test_cancel_waits_for_blocked_loader;
              Alcotest.test_case "cancelled attempt releases recording fd"
                `Quick test_cancelled_attempt_releases_recording_fd;
              Alcotest.test_case
                "pause latch is one-shot; resume does not stop next attempt"
                `Quick test_pause_latch_one_shot_resume_fresh_attempt;
              Alcotest.test_case "Pause then Resume starts a fresh attempt"
                `Quick test_pause_resume_fresh_attempt_delivers;
              Alcotest.test_case "composite typed body failures stay both"
                `Quick test_composite_body_failures_preserved;
              Alcotest.test_case "owned pause plus cleanup failure is not Ok"
                `Quick test_owned_pause_plus_cleanup_is_not_ok;
              Alcotest.test_case
                "Pulse overload and cleanup failures both preserved" `Quick
                test_pulse_overload_and_cleanup_failures_preserved;
              Alcotest.test_case
                "status observer defect composes with Pulse cause" `Quick
                test_status_observer_defect_composes_with_pulse;
              Alcotest.test_case "cleanup cancel and defect both visible" `Quick
                test_cleanup_cancel_and_defect_both_visible;
            ] );
          ( "service_gates",
            [
              Alcotest.test_case "pre-handle failure then fresh press delivers"
                `Quick test_service_pre_handle_failure_then_fresh_press;
              Alcotest.test_case
                "delayed old cleanup preserves new same-mode attempt" `Quick
                test_delayed_old_cleanup_preserves_new_same_mode_attempt;
              Alcotest.test_case "busy keyboard and RPC then fresh cycle" `Quick
                test_busy_keyboard_and_rpc_then_fresh_cycle;
              Alcotest.test_case "capture budget sets lastError" `Quick
                test_capture_budget_sets_last_error;
              Alcotest.test_case "provider failure publishes audio then retries"
                `Quick
                test_provider_failure_publishes_audio_then_retries_provider;
              Alcotest.test_case
                "delivery failure publishes uncertain text without auto-retry"
                `Quick test_delivery_failure_publishes_text_retry_skips_provider;
              Alcotest.test_case "blank transcript publishes audio no inject"
                `Quick test_blank_transcript_publishes_audio_without_inject;
              Alcotest.test_case "abort does not publish" `Quick
                test_abort_does_not_publish;
              Alcotest.test_case "startup text retry skips provider" `Quick
                test_startup_text_retry_skips_provider;
              Alcotest.test_case "successive failures keep latest only" `Quick
                test_successive_failures_keep_latest_only;
              Alcotest.test_case "busy and disabled retry reject without load"
                `Quick test_busy_and_disabled_retry_does_not_load_payload;
              Alcotest.test_case "nested missing parent is private 0700" `Quick
                test_nested_missing_parent_is_private;
              Alcotest.test_case "blueprint on_press does not publish" `Quick
                test_blueprint_on_press_does_not_publish;
              Alcotest.test_case "success preserves older candidate" `Quick
                test_success_preserves_older_candidate;
              Alcotest.test_case "publication failure keeps primary" `Quick
                test_publication_failure_keeps_primary;
            ] );
          ( "abort_older",
            [
              Alcotest.test_case
                "owned cancel cleanup defect preserves older candidate" `Quick
                test_owned_cancel_cleanup_defect_preserves_older;
            ] );
          ( "admission_hold",
            [
              Alcotest.test_case "admission held until provider finalizer"
                `Quick test_admission_held_until_provider_finalizer;
            ] );
          ( "recover",
            [
              Alcotest.test_case "single intended Fail recovers" `Quick
                test_recover_single_intended_fail;
              Alcotest.test_case "concurrent ordinary and budget both retained"
                `Quick test_recover_concurrent_ordinary_and_budget;
              Alcotest.test_case "concurrent pause and body both retained"
                `Quick test_recover_concurrent_pause_and_body;
              Alcotest.test_case "sequential typed pair retained" `Quick
                test_recover_sequential_typed_pair;
            ] );
          ( "stream",
            [
              Alcotest.test_case
                "production Codex streaming via private child env" `Slow
                test_streaming_codex_private_child;
            ] );
        ]
