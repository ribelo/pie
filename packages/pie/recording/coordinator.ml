type recording_mode = Ptt_transcribe | Ptt_translate

(* PIE004: admission is owned by one attempt, not by a mode. A delayed release
   from an earlier attempt must never clear admission owned by a later attempt
   that happens to run the same mode. *)
(* One-shot pause/cancel latch owned by a single admission attempt. Signalling
   is idempotent; awaiters that subscribe after the signal still observe it.
   Resume does not clear the latch. The next try_start allocates a fresh one. *)
type cancel = {
  fired : bool Atomic.t;
  wait : unit Eio.Promise.t;
  resolve : unit Eio.Promise.u;
}

type attempt = {
  id : int;
  mode : recording_mode;
  cancel : cancel;
  mutable activity : Phase.activity;
}

let attempt_counter = Atomic.make 0

let make_cancel () =
  let wait, resolve = Eio.Promise.create () in
  { fired = Atomic.make false; wait; resolve }

let make_attempt mode =
  {
    id = Atomic.fetch_and_add attempt_counter 1;
    mode;
    cancel = make_cancel ();
    activity = Phase.Starting;
  }

let attempt_mode attempt = attempt.mode
let attempt_id attempt = attempt.id

let signal_cancel attempt =
  if Atomic.compare_and_set attempt.cancel.fired false true then
    Eio.Promise.resolve attempt.cancel.resolve ()

let await_cancel attempt = Eio.Promise.await attempt.cancel.wait
let cancel_signalled attempt = Atomic.get attempt.cancel.fired

type start_result = Started of attempt | Busy of recording_mode | Disabled

type snapshot = {
  phase : Phase.t;
  enabled : bool;
  active : bool;
  processing : int;
  mode : string;
  started_at : string option;
  updated_at : string;
  transcript_path : string option;
  last_error : string option;
}

type state = {
  enabled : bool;
  (* The active slot is the admitted attempt itself. *)
  mode : attempt option;
  phase : Phase.activity option;
  started_at : float option;
  transcript_path : string option;
  last_error : string option;
  updated_at : float;
}

type t = {
  persist_path : string;
  caps :
    < Ox_caps.Cap.env
    ; Ox_caps.Cap.fs_write
    ; Ox_caps.Cap.time
    ; Ox_caps.Cap.stderr >;
  logger : Ox_observability.Logger.t option;
  state : state ref;
  mutex : Eio.Mutex.t;
  changed : Eio.Condition.t;
  mutable attempts : attempt list;
}

let locked t f = Eio.Mutex.use_rw ~protect:true t.mutex f
let current_attempt t = locked t (fun () -> !(t.state).mode)

let string_of_mode = function
  | Ptt_transcribe -> "ptt-transcribe"
  | Ptt_translate -> "ptt-translate"

let mode_of_string = function
  | "ptt-transcribe" -> Some Ptt_transcribe
  | "ptt-translate" -> Some Ptt_translate
  | _ -> None

let initial_state now =
  {
    enabled = true;
    mode = None;
    phase = None;
    started_at = None;
    transcript_path = None;
    last_error = None;
    updated_at = now;
  }

let iso_of_unix seconds =
  let tm = Unix.gmtime seconds in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let snapshot_of_state ?(processing = 0) state =
  let mode =
    match state.mode with
    | Some attempt -> string_of_mode attempt.mode
    | None -> "idle"
  in
  {
    phase =
      (match state.mode with
      | Some attempt ->
          Phase.Active
            (if cancel_signalled attempt then Phase.Cancelling
             else Option.value ~default:Phase.Starting state.phase)
      | None ->
          Phase.legacy ~active:false ~enabled:state.enabled ~mode
            ~last_error:state.last_error);
    enabled = state.enabled;
    active = state.mode <> None;
    processing;
    mode;
    started_at = Option.map iso_of_unix state.started_at;
    updated_at = iso_of_unix state.updated_at;
    transcript_path = state.transcript_path;
    last_error = state.last_error;
  }

let json_of_snapshot (snapshot : snapshot) =
  `Assoc
    [
      ("phase", `String (Phase.to_string snapshot.phase));
      ("enabled", `Bool snapshot.enabled);
      ("active", `Bool snapshot.active);
      ("processing", `Int snapshot.processing);
      ("mode", `String snapshot.mode);
      ( "startedAt",
        match snapshot.started_at with Some s -> `String s | None -> `Null );
      ("updatedAt", `String snapshot.updated_at);
      ( "transcriptPath",
        match snapshot.transcript_path with
        | Some s -> `String s
        | None -> `Null );
      ( "lastError",
        match snapshot.last_error with Some s -> `String s | None -> `Null );
    ]

let mkdir_p = Fs.mkdir_p

let write_state t =
  let caps = t.caps in
  let snapshot =
    snapshot_of_state
      ~processing:
        (List.length t.attempts - if !(t.state).mode = None then 0 else 1)
      !(t.state)
  in
  mkdir_p ~caps (Filename.dirname t.persist_path);
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps t.persist_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc
        (Yojson.Safe.pretty_to_string (json_of_snapshot snapshot));
      output_char oc '\n')

(* Best-effort persistence used on every state mutation: the in-memory state
   is authoritative, so a transient write failure is logged rather than
   crashing the daemon's hot path. *)
let persist t =
  let caps = t.caps in
  try write_state t
  with exn -> (
    match t.logger with
    | Some logger ->
        Ox_observability.Logger.error logger
          ~fields:
            [
              ("path", String t.persist_path);
              ("error", String (Printexc.to_string exn));
            ]
          "failed to write recording state"
    | None ->
        Ox_caps.Cap_stdlib.eprintf ~caps
          "failed to write recording state (%s): %s@." t.persist_path
          (Printexc.to_string exn))

let create ~caps ?logger ?persist_path () =
  let persist_path =
    Option.value persist_path ~default:(Paths.recording_state_path ~caps)
  in
  let caps =
    (caps
      :> < Ox_caps.Cap.env
         ; Ox_caps.Cap.fs_write
         ; Ox_caps.Cap.time
         ; Ox_caps.Cap.stderr >)
  in
  let now = Ox_caps.Cap_unix.gettimeofday ~caps () in
  let t =
    {
      persist_path;
      caps;
      logger;
      state = ref (initial_state now);
      mutex = Eio.Mutex.create ();
      changed = Eio.Condition.create ();
      attempts = [];
    }
  in
  (* Fail fast at startup: if we cannot write the initial state file, the
     caller must know rather than run with a phantom persistence path. *)
  write_state t;
  t

let processing_count t =
  List.length t.attempts - if !(t.state).mode = None then 0 else 1

let snapshot t =
  locked t (fun () ->
      snapshot_of_state ~processing:(processing_count t) !(t.state))

(* The admitted attempt owns its FIFO position through delivery and settlement. *)
let await_delivery t attempt =
  Eio.Mutex.use_ro t.mutex (fun () ->
      let rec wait () =
        match t.attempts with
        | first :: _ when first.id = attempt.id -> ()
        | _ when not (List.exists (fun a -> a.id = attempt.id) t.attempts) -> ()
        | _ ->
            Eio.Condition.await t.changed t.mutex;
            wait ()
      in
      wait ())

let is_active t = locked t (fun () -> !(t.state).mode <> None)

let mutate_unlocked t f =
  let result, next = f !(t.state) in
  if next != !(t.state) then (
    t.state := next;
    persist t);
  result

let mutate t f = locked t (fun () -> mutate_unlocked t f)

(* Processing releases microphone ownership, not the FIFO settlement slot.
   A stale processing owner cannot change the next capture's status. *)
let set_phase t (attempt : attempt) phase =
  let caps = t.caps in
  locked t @@ fun () ->
  let current = !(t.state) in
  match current.mode with
  | Some active when active.id = attempt.id ->
      let phase = if cancel_signalled active then Phase.Cancelling else phase in
      let before = Option.value ~default:Phase.Starting current.phase in
      if not (Phase.can_transition before phase) then false
      else if before = phase then true
      else (
        attempt.activity <- phase;
        mutate_unlocked t (fun current ->
            ( true,
              {
                current with
                mode =
                  (if phase = Phase.Processing || phase = Phase.Delivering then
                     None
                   else current.mode);
                started_at =
                  (if phase = Phase.Processing || phase = Phase.Delivering then
                     None
                   else current.started_at);
                phase =
                  (if phase = Phase.Processing || phase = Phase.Delivering then
                     None
                   else Some phase);
                updated_at = Ox_caps.Cap_unix.gettimeofday ~caps ();
              } )))
  | _ ->
      if
        List.exists (fun a -> a.id = attempt.id) t.attempts
        && Phase.can_transition attempt.activity phase
      then (
        attempt.activity <- phase;
        true)
      else false

let try_start t mode ?transcript_path () =
  let caps = t.caps in
  mutate t (fun current ->
      if not current.enabled then (Disabled, current)
      else
        match current.mode with
        | Some active -> (Busy active.mode, current)
        | None when processing_count t >= 4 -> (Busy mode, current)
        | None ->
            let now = Ox_caps.Cap_unix.gettimeofday ~caps () in
            let attempt = make_attempt mode in
            t.attempts <- t.attempts @ [ attempt ];
            ( Started attempt,
              {
                enabled = current.enabled;
                mode = Some attempt;
                phase = Some Phase.Starting;
                started_at = Some now;
                transcript_path;
                last_error = None;
                updated_at = now;
              } ))

(* Settlement removes this attempt's FIFO slot without changing a newer capture.
   Persist queue-count changes even when no capture state changes. *)
let release t attempt =
  let caps = t.caps in
  mutate t (fun current ->
      let known = List.exists (fun a -> a.id = attempt.id) t.attempts in
      t.attempts <- List.filter (fun a -> a.id <> attempt.id) t.attempts;
      Eio.Condition.broadcast t.changed;
      match current.mode with
      | Some active when active.id = attempt.id ->
          let now = Ox_caps.Cap_unix.gettimeofday ~caps () in
          ( true,
            {
              enabled = current.enabled;
              mode = None;
              phase = None;
              started_at = None;
              transcript_path = None;
              last_error = current.last_error;
              updated_at = now;
            } )
      | _ ->
          ( known,
            if known then
              {
                current with
                updated_at = Ox_caps.Cap_unix.gettimeofday ~caps ();
              }
            else current ))

(* Publish order: signal the current attempt, then write enabled, then persist.
   A persist failure is logged by [mutate] and does not unsignal. Pause of an
   in-flight attempt is therefore consistent with in-memory enabled=false even
   when the state file write fails. *)
let set_enabled t enabled =
  let caps = t.caps in
  mutate t (fun current ->
      (if not enabled then
         match current.mode with
         | Some attempt -> signal_cancel attempt
         | None -> ());
      ( (),
        {
          current with
          enabled;
          updated_at = Ox_caps.Cap_unix.gettimeofday ~caps ();
        } ))

let toggle_enabled t =
  let caps = t.caps in
  mutate t (fun current ->
      let enabled = not current.enabled in
      (if not enabled then
         match current.mode with
         | Some attempt -> signal_cancel attempt
         | None -> ());
      ( enabled,
        {
          current with
          enabled;
          updated_at = Ox_caps.Cap_unix.gettimeofday ~caps ();
        } ))

let set_error t message =
  let caps = t.caps in
  mutate t (fun current ->
      ( (),
        {
          current with
          last_error = Some message;
          updated_at = Ox_caps.Cap_unix.gettimeofday ~caps ();
        } ))

let clear t =
  let caps = t.caps in
  mutate t (fun current ->
      (match current.mode with
      | None -> ()
      | Some attempt ->
          signal_cancel attempt;
          t.attempts <- List.filter (fun a -> a.id <> attempt.id) t.attempts;
          Eio.Condition.broadcast t.changed);
      let next =
        {
          enabled = current.enabled;
          mode = None;
          phase = None;
          started_at = None;
          transcript_path = None;
          last_error = None;
          updated_at = Ox_caps.Cap_unix.gettimeofday ~caps ();
        }
      in
      ((), next))
