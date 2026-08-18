type recording_mode =
  | Ptt_transcribe
  | Ptt_translate
  | Wakeword
  | Meeting_transcribe

type start_result =
  | Started of recording_mode
  | Busy of recording_mode
  | Disabled

type snapshot = {
  enabled : bool;
  active : bool;
  mode : string;
  started_at : string option;
  updated_at : string;
  transcript_path : string option;
  last_error : string option;
}

type state = {
  enabled : bool;
  mode : recording_mode option;
  started_at : float option;
  transcript_path : string option;
  last_error : string option;
  updated_at : float;
}

type t = {
  persist_path : string;
  state : state ref;
}

let string_of_mode = function
  | Ptt_transcribe -> "ptt-transcribe"
  | Ptt_translate -> "ptt-translate"
  | Wakeword -> "wakeword"
  | Meeting_transcribe -> "meeting-transcribe"

let mode_of_string = function
  | "ptt-transcribe" -> Some Ptt_transcribe
  | "ptt-translate" -> Some Ptt_translate
  | "wakeword" -> Some Wakeword
  | "meeting-transcribe" -> Some Meeting_transcribe
  | _ -> None

let initial_state now = {
  enabled = true;
  mode = None;
  started_at = None;
  transcript_path = None;
  last_error = None;
  updated_at = now;
}

let iso_of_unix seconds =
  let tm = Unix.gmtime seconds in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let snapshot_of_state state =
  let mode =
    match state.mode with
    | Some mode -> string_of_mode mode
    | None -> "idle"
  in
  {
    enabled = state.enabled;
    active = state.mode <> None;
    mode;
    started_at = Option.map iso_of_unix state.started_at;
    updated_at = iso_of_unix state.updated_at;
    transcript_path = state.transcript_path;
    last_error = state.last_error;
  }

let json_of_snapshot (snapshot : snapshot) =
  `Assoc [
    ("enabled", `Bool snapshot.enabled);
    ("active", `Bool snapshot.active);
    ("mode", `String snapshot.mode);
    ("startedAt", (match snapshot.started_at with Some s -> `String s | None -> `Null));
    ("updatedAt", `String snapshot.updated_at);
    ("transcriptPath", (match snapshot.transcript_path with Some s -> `String s | None -> `Null));
    ("lastError", (match snapshot.last_error with Some s -> `String s | None -> `Null));
  ]

let mkdir_p = Fs.mkdir_p

let write_state t =
  let snapshot = snapshot_of_state !(t.state) in
  mkdir_p (Filename.dirname t.persist_path);
  let oc = open_out_bin t.persist_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
       output_string oc (Yojson.Safe.pretty_to_string (json_of_snapshot snapshot));
       output_char oc '\n')

(* Best-effort persistence used on every state mutation: the in-memory state
   is authoritative, so a transient write failure is logged rather than
   crashing the daemon's hot path. *)
let persist t =
  try write_state t
  with exn ->
    Process.emit_sync ~level:Error
      ~attrs:[
        ("path", t.persist_path);
        ("error", Printexc.to_string exn);
      ]
      "failed to write recording state"

let create ?(persist_path = Paths.recording_state_path) () =
  let now = Unix.gettimeofday () in
  let t = { persist_path; state = ref (initial_state now) } in
  (* Fail fast at startup: if we cannot write the initial state file, the
     caller must know rather than run with a phantom persistence path. *)
  write_state t;
  t

let snapshot t = snapshot_of_state !(t.state)

let mutate t f =
  let result, next = f !(t.state) in
  t.state := next;
  persist t;
  result

let try_start t mode ?transcript_path () =
  mutate t (fun current ->
    if not current.enabled then
      (Disabled, current)
    else
      match current.mode with
      | Some active -> (Busy active, current)
      | None ->
        let now = Unix.gettimeofday () in
        (Started mode, {
          enabled = current.enabled;
          mode = Some mode;
          started_at = Some now;
          transcript_path;
          last_error = None;
          updated_at = now;
        }))

let stop t mode =
  mutate t (fun current ->
    match current.mode with
    | Some active when active = mode ->
      let now = Unix.gettimeofday () in
      (true, {
        enabled = current.enabled;
        mode = None;
        started_at = None;
        transcript_path = None;
        last_error = current.last_error;
        updated_at = now;
      })
    | _ -> (false, current))

let set_enabled t enabled =
  mutate t (fun current ->
    ((), { current with enabled; updated_at = Unix.gettimeofday () }))

let toggle_enabled t =
  mutate t (fun current ->
    let enabled = not current.enabled in
    (enabled, { current with enabled; updated_at = Unix.gettimeofday () }))

let toggle_meeting t =
  mutate t (fun current ->
    let now = Unix.gettimeofday () in
    match current.mode with
    | Some Meeting_transcribe ->
      let next = {
        enabled = current.enabled;
        mode = None;
        started_at = None;
        transcript_path = None;
        last_error = current.last_error;
        updated_at = now;
      } in
      (snapshot_of_state next, next)
    | None when current.enabled ->
      let next = {
        enabled = current.enabled;
        mode = Some Meeting_transcribe;
        started_at = Some now;
        transcript_path = None;
        last_error = None;
        updated_at = now;
      } in
      (snapshot_of_state next, next)
    | _ -> (snapshot_of_state current, current))

let set_error t message =
  mutate t (fun current ->
    ((), { current with last_error = Some message; updated_at = Unix.gettimeofday () }))

let clear t =
  mutate t (fun current ->
    let next = {
      enabled = current.enabled;
      mode = None;
      started_at = None;
      transcript_path = None;
      last_error = None;
      updated_at = Unix.gettimeofday ();
    } in
    ((), next))
