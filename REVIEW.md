# Eta vs Eio Usage Review — Round 3 (post-second-implementation)

## Summary

The agent fixed the round-2 catastrophes. Real wins everywhere:

- **N1, N2 fixed** — `with_client` / `with_record_stream` no longer create
  nested Eta runtimes. `acquire_release` over the parent runtime, single
  `Eio.Stream.t`, no translator fiber, recv exposed as a closure.
- **N3 fixed** — `Obj.t` is gone. `process_mgr` is threaded explicitly.
- **N4 fixed** — `Sh.run` / `Sh.run_capture` are real Eta effects with
  `[> `Subprocess_error of string]`.
- **N5 fixed** — `openrouter.ml` no longer puts `Eio.Process` inside
  `Effect.Blocking.submit`.
- **N6 fixed** — niri shell injection patched (`spawn -- wtype -- text`).
- **N7 fixed** — `--json` argument order restored.
- **N8 fixed** — `status_cmd` once again prints `"off"` for daemon-down.
- **N9 fixed** — `daemon/client.ml` rewritten over `Eio.Net` +
  `Eio.Buf_read.line`. Typed errors. No more `Effect.Blocking.submit`.
- **N10 fixed** — `read_line ()` replaced with `Eio.Buf_read.parse_exn ~max_size:4096
  Eio.Buf_read.line stdin` in all three command files.
- **N11 fixed** — `Eta.Mutable_ref` replaced with plain `ref` in command files.
- **N13 fixed** — `let*` operator everywhere, control flow reads
  top-to-bottom, helpers extracted.
- **B5 fixed** — `Camelpie.Fs.mkdir_p` extracted.

Acceptance test passes: `Obj.t`, `Eio.Switch.run` (outside `bin/pie.ml`),
`read_line`, `Sys.command`, `_blocking`, `Hashtbl.copy`, `Blocking.submit`,
`Unix.socket`, `Unix.connect` — zero in `packages/camelpie` outside the
single allowed file. `Eio.Fiber.fork_daemon` only inside
`pulse/client.ml:135`.

This is a large step forward. Most of round 2's blockers are gone.

The remaining issues are smaller in kind: a few typed-error rows are still
fictional, three command files still copy-paste an identical 30-line helper
block, `bin/pie.ml` did not actually shrink, and a couple of behavior
nicks crept in. Not blockers individually but worth one more cleanup pass.

---

## P0 — Open structural issues

### R1. Typed-error rows still fictional in 4 places (was N12, partially open)
**Severity:** Public-contract lie

The agent claimed: *"Typed error rows are real — Eta.Effect.fail used
throughout; catch at call sites"*. Mostly true. **Four exceptions remain.**

Recall the rule: `Eta.Effect.catch` catches only `Cause.Fail`, never
`Cause.Die`. Anything raised inside `Eta.Effect.sync` becomes `Cause.Die`.
So whenever an `Effect.sync` callback ends up calling `failwith` /
`raise Foo`, the typed error row in the corresponding `.mli` is unreachable.

#### R1a. `pulse/client.ml`'s `get_server_info` / `list_sources` use a self-defeating pattern
**File:** `packages/camelpie/pulse/client.ml:185-198`

```ocaml
let get_server_info client =
  Eta.Effect.sync (fun () ->
    match get_server_info_impl client with
    | Ok info -> info
    | Error (`Pulse_error msg) -> failwith msg)
  |> Eta.Effect.catch (fun (`Pulse_error msg) -> Eta.Effect.fail (`Pulse_error msg))
```

The `failwith msg` is interpreted as `Cause.Die`. `Eta.Effect.catch` only
fires on `Cause.Fail`. **The `catch` is dead code.** When the impl returns
`Error _`, the user sees a runtime defect, not a typed `Pulse_error`.

**Fix:**
```ocaml
let get_server_info client =
  Eta.Effect.bind
    (function
      | Ok info -> Eta.Effect.pure info
      | Error e -> Eta.Effect.fail e)
    (Eta.Effect.sync (fun () -> get_server_info_impl client))
```

Same shape for `list_sources`.

#### R1b. `pulse/client.ml`'s `with_client` acquire raises through `Effect.sync`
**File:** `packages/camelpie/pulse/client.ml:171-179`

```ocaml
let with_client ~sw ~net ~clock … f =
  Eta.Effect.acquire_release
    ~acquire:(Eta.Effect.sync (fun () ->
      connect ~net ~clock ~sw …))
    …
```

`connect` raises `Client_error` / `failwith` on cookie failure, auth
failure, protocol mismatch. Inside `Effect.sync` those become `Cause.Die`.
The `with_client` signature claims `[> `Pulse_error of string ] as 'err`
in its result row. That row is **fictional for connect failures**.

**Fix:** make `connect` itself return a `result`, then lift it via
`Effect.bind` like R1a. Or wrap `connect` in an `Effect.catch` that
translates `Client_error` to `Effect.fail (`Pulse_error e.message)`. The
former is cleaner — kill the `Client_error` exception entirely.

#### R1c. `text_injection.ml::detect_session` raises `failwith`
**File:** `packages/camelpie/input/text_injection.ml:21-26, 38-44`

```ocaml
let detect_session () =
  match Sys.getenv_opt "XDG_SESSION_TYPE" with
  …
  | _ -> … failwith "could not detect graphical session"

let inject ~process_mgr text =
  Eta.Effect.sync (fun () -> detect_session ())
  |> Eta.Effect.bind (fun session -> …)
```

`failwith` inside `Effect.sync` → `Cause.Die`. The `.mli`'s
`[> `Injection_error of string | …]` row never fires for the
no-graphical-session case.

**Fix:**
```ocaml
let detect_session_eff =
  match Sys.getenv_opt "XDG_SESSION_TYPE" with
  | Some "wayland" -> Eta.Effect.pure Wayland
  | Some "x11" -> Eta.Effect.pure X11
  | _ ->
    if Sys.getenv_opt "WAYLAND_DISPLAY" <> None then Eta.Effect.pure Wayland
    else if Sys.getenv_opt "DISPLAY" <> None then Eta.Effect.pure X11
    else Eta.Effect.fail (`Injection_error "could not detect graphical session")
```

…and replace the `Effect.sync (fun () -> detect_session ())` call with
`detect_session_eff`. While there, `detect_session : unit -> session_type`
in the `.mli` is the public escape hatch; either delete it or rename it
to `detect_session_unsafe`. Currently it's a public footgun.

#### R1d. `OPENROUTER_API_KEY not set` raises in three commands
**Files:** `commands/ptt.ml:46-50`, `commands/assistant.ml:43-47`,
`commands/stt_interactive.ml:42-46`

```ocaml
let api_key =
  match Sys.getenv_opt "OPENROUTER_API_KEY" with
  | Some k -> k
  | None -> failwith "OPENROUTER_API_KEY not set"
```

Same pattern, three times, all in the middle of a `let*` chain. Becomes
`Cause.Die` → never typed.

**Fix (combines with R3):** put the lookup inside `Stt.Openrouter`:

```ocaml
(* stt/openrouter.ml *)
val transcribe :
  process_mgr:_ Eio.Process.mgr ->
  ?model:string -> ?language:string ->
  bytes ->
  (string, [> `Stt_error of string ]) Eta.Effect.t
(* implementation reads OPENROUTER_API_KEY internally and returns
   `Stt_error "OPENROUTER_API_KEY not set"` if missing *)
```

The three command callsites collapse from four lines to one.

---

### R2. `bin/pie.ml` did not shrink
**File:** `packages/camelpie/bin/pie.ml` — 359 lines (was 327 before round 2,
271 originally)
**Severity:** Decomposition / round-2 N14 not actually fixed

The agent claimed: *"bin/pie.ml shrank — daemon commands are direct calls"*.
Daemon commands becoming direct calls **is** real — but the file as a
whole grew from 327 to 359 lines because every command now threads four
or five capabilities (`~sw`, `~net`, `~clock`, `~process_mgr`, `~stdin`)
through a `run_effect (fun ~env ~sw -> …)` closure.

Sample density (line 165–198, three near-identical `ptt` variants):

```ocaml
let ptt_cmd =
  let doc = "Push-to-talk capture and transcribe/translate" in
  let info = Cmd.info "ptt" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Ptt.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ~process_mgr:(Eio.Stdenv.process_mgr env)
            ~stdin:(Eio.Stdenv.stdin env)
            ~mode:`Transcribe ()))
      $ env_term)

let ptt_transcribe_cmd = (* same body, same mode *)
let ptt_translate_cmd  = (* same body, mode := `Translate *)
```

Three full `Cmd.v` blocks for one command with one differing parameter.

**Fix:** one tiny helper that resolves the capability bundle once:

```ocaml
type caps = {
  sw : Eio.Switch.t;
  net : [`Generic] Eio.Net.ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Std.r;
  process_mgr : Eio.Process.mgr_ty Eio.Std.r;
  stdin : Eio.Flow.source_ty Eio.Std.r;
}

let run_cmd mk_eff =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let caps = {
    sw;
    net = Eio.Stdenv.net env;
    clock = Eio.Stdenv.clock env;
    process_mgr = Eio.Stdenv.process_mgr env;
    stdin = Eio.Stdenv.stdin env;
  } in
  let rt = Eta.Runtime.create ~sw ~clock:caps.clock ~capture_backtrace:true () in
  match Eta.Runtime.run rt (mk_eff caps) with
  | Ok () -> ()
  | Error c when Eta.Cause.is_interrupt_only c -> exit 130
  | Error _ -> exit 1
```

Each `Cmd.v` then becomes a one-liner:
```ocaml
let ptt_cmd =
  Cmd.v (Cmd.info "ptt" ~doc) Term.(
    const (fun () -> run_cmd (Camelpie.Commands.Ptt.run ~mode:`Transcribe))
    $ env_term)
```

The three PTT subcommands collapse to:
```ocaml
let ptt_cmd, ptt_transcribe_cmd, ptt_translate_cmd =
  let info name = Cmd.info name ~doc:"PTT capture/transcribe/translate" in
  let make name mode =
    Cmd.v (info name) Term.(
      const (fun () -> run_cmd (Camelpie.Commands.Ptt.run ~mode))
      $ env_term)
  in
  make "ptt" `Transcribe, make "ptt-transcribe" `Transcribe, make "ptt-translate" `Translate
```

…or — the cleaner judo move — drop `ptt-transcribe` and `ptt-translate`
entirely and add `--mode={transcribe|translate}` to `ptt`. Three subcommands
that do the same thing today (B8 still open) is a UX bug.

After this pass `bin/pie.ml` should land near 200 lines, not 359.

---

### R3. Three-way duplication of `read_stdin_line` + `drain_until_interrupt`
**Files:** `commands/ptt.ml:3-32`, `commands/assistant.ml:3-32`,
`commands/stt_interactive.ml:3-32`
**Severity:** Decomposition — copy-paste

```ocaml
let read_stdin_line ~stdin =
  Eta.Effect.sync (fun () ->
    Eio.Buf_read.parse_exn ~max_size:4096 Eio.Buf_read.line stdin)

let drain_until_interrupt ~stdin recv =
  let chunks_ref = ref [] in
  let rec recv_loop () =
    let* chunk = recv () in
    let* () = Eta.Effect.sync (fun () -> chunks_ref := chunk :: !chunks_ref) in
    recv_loop ()
  in
  let* () = Eta.Effect.race [ Eta.Effect.map (fun _ -> ()) (read_stdin_line ~stdin); recv_loop () ] in
  Eta.Effect.sync (fun () ->
    let data = List.rev !chunks_ref in
    let data_len = List.fold_left (fun acc b -> acc + Bytes.length b) 0 data in
    Printf.printf "Captured %d bytes in %d chunks\n%!" data_len (List.length data);
    let pcm = Bytes.create data_len in
    let offset = ref 0 in
    List.iter (fun chunk ->
      Bytes.blit chunk 0 pcm !offset (Bytes.length chunk);
      offset := !offset + Bytes.length chunk
    ) data;
    let normalized = Audio.Pcm.normalize_pcm_for_stt pcm in
    normalized.normalized_bytes)
```

Identical 30 lines, three times. `assistant.ml` has a tiny tweak (returns
`Bytes.empty` instead of normalizing on `data_len = 0`); `stt_interactive.ml`
has another tweak (`exit 0` on empty). Both tweaks should be parameters or
post-processing on the shared helper.

**Fix:** lift to `commands/common.ml` (or `audio/capture.ml`):

```ocaml
val read_stdin_line :
  stdin:_ Eio.Flow.source ->
  (string, 'err) Eta.Effect.t

val capture_until_enter :
  stdin:_ Eio.Flow.source ->
  recv:(unit -> (bytes, [> `Closed ]) Eta.Effect.t) ->
  (bytes, [> `Closed ]) Eta.Effect.t
(** Concatenated raw PCM, may be empty. Caller decides what to do
    with the empty case. *)
```

Then the three command files shrink from 56/72/57 lines to ~25/40/25.
Combined with R1d (kill the `OPENROUTER_API_KEY` boilerplate), they go
even lower.

---

### R4. `pulse/client.mli` leaks implementation details
**File:** `packages/camelpie/pulse/client.mli:7-12`

```ocaml
exception Client_error of error
val error : string -> ?code:int -> unit -> error
val fail : string -> ?code:int -> unit -> 'a
```

After the typed-effect refactor, `Client_error`, `error`, and `fail` are
purely internal. No public function returns or raises them in the new
surface (`with_client`, `with_record_stream`, `get_server_info`,
`list_sources` all use `[> `Pulse_error of string ]` typed errors).

**Fix:** delete those three lines from the `.mli`. Keep `error` /
`fail` as `let`-bindings inside `client.ml` only. Once R1a/R1b are done,
even the internal `Client_error` exception can be deleted.

---

## P1 — Smaller things

### R5. `pulse/client.ml`'s `with_record_stream` release does not close the socket
**File:** `packages/camelpie/pulse/client.ml:177-184` (release branch of `with_client`)

```ocaml
~release:(fun (T t) -> Eta.Effect.sync (fun () ->
  Hashtbl.iter (fun _ resolver ->
    Eio.Promise.resolve_error resolver (error "client released" ())
  ) t.pending;
  Hashtbl.clear t.pending))
```

Resolves pending promises with an error (good — wakes up any
mid-flight `await_reply`), but does not explicitly close the socket. The
socket will be closed when `~sw` exits, which happens *after* the runtime
exits, but until then the read loop fiber may keep doing `Eio.Flow.single_read`
and `process_incoming` against a half-decommissioned hashtable
(`record_queues` is also still there).

This is borderline: `release` runs synchronously inside the outer effect
boundary, so by the time the user's effect returns, the only running
Eio fibers are the daemon read-loop which will exit on `End_of_file`
when `sw` closes the socket. In practice it works. But the asymmetry
between `connect` (opens socket via `Eio.Net.connect ~sw`, forks read
loop) and `release` (clears in-memory state but does not close socket)
is the kind of thing that bites later.

**Fix:** add `Eio.Flow.close t.socket` to release, and clear
`record_queues` so the daemon stops appending. Not strictly required for
correctness today but defensive.

---

### R6. `wayland/portal.ml::create_session` lost its exit-status check
**File:** `packages/camelpie/wayland/portal.ml:5-9, 15-30`

```ocaml
let busctl ~process_mgr args =
  Sh.run_capture ~process_mgr ~prog:"busctl" ~args:("--user" :: args)
  |> Eta.Effect.map (fun out -> (0, out))
  |> Eta.Effect.catch (fun (`Subprocess_error msg) ->
    Eta.Effect.pure (1, msg))

let create_session ~process_mgr () =
  let* (_status, out) = busctl ~process_mgr […] in
  if String.length out = 0 then
    Eta.Effect.fail (`Portal_error "CreateSession failed — is xdg-desktop-portal running?")
  else
    match String.split_on_char '"' out with …
```

Original code: `if status <> 0 || String.length out = 0 then Error …`.
New code: only checks `String.length out = 0`. **`_status` is discarded.**
A `Subprocess_error` returns `(1, msg)`, where `msg` is the *exception
message string* (non-empty), so the path falls through into
`String.split_on_char '"' msg` and produces a meaningless "unexpected
busctl output: …" error.

**Fix:**
```ocaml
let* (status, out) = busctl ~process_mgr […] in
if status <> 0 || String.length out = 0 then
  Eta.Effect.fail (`Portal_error ("CreateSession failed: " ^ out))
else …
```

Also the `(1, msg)` shape pretends the subprocess exit code is 1 even
when the real failure was, e.g., process spawn error. Cleaner: drop the
`(int * string)` tuple entirely and let the typed error propagate:

```ocaml
let busctl ~process_mgr args =
  Sh.run_capture ~process_mgr ~prog:"busctl" ~args:("--user" :: args)
  |> Eta.Effect.catch (fun (`Subprocess_error msg) ->
    Eta.Effect.fail (`Portal_error ("busctl failed: " ^ msg)))

let create_session ~process_mgr () =
  let* out = busctl ~process_mgr […] in
  if String.length out = 0 then
    Eta.Effect.fail (`Portal_error "CreateSession returned no path")
  else …
```

`bind_shortcut` has the same dead status check (line 35). Same fix.

---

### R7. `Eio.Buf_read.parse_exn` per-call allocation
**Files:** `commands/{ptt,assistant,stt_interactive}.ml:3-5`

```ocaml
let read_stdin_line ~stdin =
  Eta.Effect.sync (fun () ->
    Eio.Buf_read.parse_exn ~max_size:4096 Eio.Buf_read.line stdin)
```

`Buf_read.parse_exn` constructs a fresh `Buf_read.t` on every call,
allocating an internal buffer. For the assistant's "press Enter, record,
press Enter, repeat forever" loop that is one allocation per round.
Functionally fine — `Buf_read.line` only consumes up to the newline so
no over-read is lost — but the Eio idiom is one `Buf_read.t` per flow
lifetime. Combined with R3, the lifted helper should keep a `Buf_read.t`
around in a closure, not allocate per-call.

---

### R8. `B8` is "TODO comment added", not fixed
**File:** `commands/ptt.ml:38-41`

```ocaml
(* B8 TODO: mode (`Transcribe | `Translate) is only used in the prompt.
   Wire model / language to differ between modes, or collapse the
   ptt-transcribe / ptt-translate subcommands into one. *)
```

Three subcommands (`ptt`, `ptt-transcribe`, `ptt-translate`) still do
identical work. Either wire `model`/`language`/`prompt` to differ — the
real Whisper translate-vs-transcribe mode is `model=whisper-1` plus
`response_format=json` and either `transcriptions` or `translations` as
the endpoint — or delete the duplicate subcommands. A TODO buys nothing.

---

### R9. `niri.ml::type_text_impl` quotes via `--` but caller-supplied text could break the parse
**File:** `packages/camelpie/niri/niri.ml:13-17`

```ocaml
Sh.run ~process_mgr ~prog:niri
  ~args:["msg"; "action"; "spawn"; "--"; "wtype"; "--"; normalized]
```

The `--` separators rely on niri's `spawn` accepting `--` to stop option
parsing. That's the conventional posix shape but worth a minimal smoke
test (`niri msg action spawn -- wtype -- "hello world"`) before claiming
N6 fully closed. If niri's `spawn` parser doesn't honor `--`, the text
re-enters niri's own option parsing and a transcript starting with
`-` becomes a niri flag.

This is paranoia, not a known-bad — but the round-2 review flagged the
shell-injection regression specifically, so the replacement fix deserves
verification.

---

### R10. `pulse/client.ml` connect still raises through `with_client`
Same as R1b but worth restating: the `with_client` typed error row is
fictional for connect failures. Round 1's fix shape — `connect` returning
`(t, [> `Pulse_error …]) result` — should land here. After R1a + R1b +
R4, the entire `Client_error` exception can be deleted.

---

### R11. `Sh.find_executable` is synchronous outside any effect
**File:** `packages/camelpie/sh.mli:1`, callers in
`wtype.ml`, `xdotool.ml`, `niri.ml`

```ocaml
val find_executable : string -> string option
```

Calls `Sys.getenv_opt` and `Sys.file_exists` (a `stat()` syscall) directly,
outside any `Effect.sync`. In practice this runs before the effect chain
starts, so it just blocks the runtime fiber for one syscall. Fine for
PATH lookup. But the callers do:

```ocaml
match Sh.find_executable "wtype" with
| None -> Eta.Effect.fail (`Wtype_error "wtype not found in PATH")
| Some wtype -> …
```

…outside any Effect, then thread the result into the effect chain. That
pattern works but the `Effect.t` only starts *after* the `match`. If
`find_executable` is ever cached or moved to e.g. an XDG resolver that
talks to a background service, the synchronous shape here will become a
problem.

**Fix (low priority):** make `find_executable` an effect:
`val find_executable : string -> (string option, 'err) Eta.Effect.t`,
or — simpler — convert the existing `match` into an effect-shaped helper:

```ocaml
let resolve_exe name err_tag =
  match Sh.find_executable name with
  | Some p -> Eta.Effect.pure p
  | None -> Eta.Effect.fail (err_tag (name ^ " not found in PATH"))
```

…shared by `wtype.ml`, `xdotool.ml`, `niri.ml`.

---

## What got better since round 2 — a real list

This is genuinely clean work. Highlights worth keeping:

- **No more nested Eta runtimes anywhere.** Round 2's two worst sins
  (N1, N2) are gone. `acquire_release` is used correctly with caller-provided
  `~sw`.
- **`Sh` is a typed effect.** `Obj.t` ambient capability deleted.
- **`daemon/client.ml` is a real Eio rewrite.** No more
  `Effect.Blocking.submit` workaround. Typed errors. ~50 lines.
- **`niri/niri.ml` shell injection actually fixed** with `--` separators
  rather than `Filename.quote` (cleaner).
- **`stt/openrouter.ml` cleanup is fine.** Temp-file work in `Effect.sync`,
  curl via `Sh.run_capture`, cleanup on both success and failure paths.
- **`wtype.ml` clipboard fallback uses `let*`** and proper typed-error
  `Effect.catch`. Reads top-to-bottom.
- **All three "stream-with-Enter-to-stop" commands use `Eio.Buf_read.line`.**
  No more `read_line ()` runtime freeze.
- **`status_cmd` distinguishes `Daemon_off`, `Daemon_error`, and
  `Snapshot`** — typed-error consumption working as intended.
- **`Effect.race [stdin_line; recv_loop]`** is the right shape for
  user-driven stop.

---

## Approval bar — round 3

This is the first round where I'd consider it close to mergeable.
Remaining presumptive blockers (things the skill flags as automatic
no-merge):

- ❌ Typed-error contracts in `.mli` files are still fictional in four
  places (R1a–R1d). All four are mechanical fixes, ~10 lines each.
- ❌ Three command files duplicate a 30-line stdin/drain helper (R3).
- ⚠️ `bin/pie.ml` is 359 lines and could plausibly be ~200 (R2). Not a
  hard blocker but the original review explicitly asked for this and the
  agent's claim of "shrank" was false.

Everything else (R5–R11) is nit-grade: real findings, but each is small,
local, and would land in a single follow-up commit.

If R1, R2, R3, R4 land, this branch passes the bar.

---

## Recommended next pass — short

1. **R1a + R1b + R1c + R1d**: lift the four `failwith`-inside-`Effect.sync`
   sites into `Effect.fail` chains. Delete the dead `Effect.catch` wrappers.
   ~30 lines of changes, four files. After this `Effect.catch` calls
   are non-trivial everywhere they appear.
2. **R3**: extract `Camelpie.Commands.Common` (or `Audio.Capture`) with
   `read_stdin_line` and `capture_until_enter`. Three command files
   shrink dramatically.
3. **R4**: delete `Client_error` / `error` / `fail` from
   `pulse/client.mli`.
4. **R2**: introduce `caps` record + one-liner `run_cmd` helper in
   `bin/pie.ml`. File should land near 200 lines after.
5. **R6**: fix `portal.ml::create_session`'s lost exit-status check.
6. **R8**: pick one — either wire `mode` into the request or delete the
   duplicate subcommands.

Everything else is cleanup.
