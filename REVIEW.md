# Eta vs Eio Usage Review — Round 2 (post-implementation)

## Summary

The previous review pushed for one judo move: lift the `pulse/client.ml`
boundary into Eta-shaped helpers and run every command through one centralized
`Eta.Runtime`. The agent **did** lift the boundary and **did** centralize the
runtime — but the implementation underneath introduced new structural
problems that are arguably worse than the original ones, and silently
regressed two behaviors (one of them a security regression).

The mechanical acceptance test from round 1 mostly passes:
- ✅ `Eio.Switch.run`, `Eio.Fiber.fork*`, `Eio.Stream.*` confined to
  `pulse/client.ml` (and `keyboard/monitor.ml`, an already-existing isolated
  use).
- ✅ `Eio_main.run` only in `bin/pie.ml`.
- ✅ `Sys.command` zero hits.
- ✅ `_blocking` zero hits.
- ✅ `Hashtbl.copy` zero hits.
- ⚠️ `dune build: PASS` could not be verified locally (the opam switch in
  this checkout has no deps installed; the agent ran the build in a
  different shell/nix devShell). Taking the claim at face value but the
  rest of this review assumes a green build.

That checklist is, however, the wrong yardstick. Several of the new files
have replaced one bad pattern with a worse pattern. **Net assessment: do
not approve.** The list of new presumptive blockers is below.

---

## P0 — New disasters introduced by the implementation

### N1. `pulse/client.ml`: nested Eta runtimes inside `Effect.sync`
**File:** `packages/camelpie/pulse/client.ml:170-184`
**Severity:** Architectural, contract violation
**Status:** OPEN

```ocaml
let with_client ~net ~clock … f =
  Eta.Effect.sync (fun () ->
    Eio.Switch.run @@ fun sw ->
    let client = connect ~net ~clock ~sw … () in
    let rt = Eta.Runtime.create ~sw ~clock () in
    match Eta.Runtime.run rt (f client) with
    | Ok v -> v
    | Error cause -> …
  )
```

This:
1. Runs an inner `Eta.Runtime.create + Runtime.run` inside an outer runtime's
   `Effect.sync` callback.
2. Opens a fresh `Eio.Switch.run` that bypasses the outer switch.
3. Loses cancellation and tracing context across the nested boundary.
4. Re-raises typed failures as `Eio.Cancel.Cancelled (Failure "interrupted")`
   to escape back through the outer `Effect.sync` — that is, the typed error
   row promised by `.mli` (`[> `Pulse_error of string ] as 'err`) is never
   actually produced; failures escape as OCaml exceptions.

The Eta docs explicitly forbid nested runtimes in worker-side callbacks
(see `Effect.Blocking` doc: "must not … run Eta runtimes"). `Effect.sync`
is more permissive but this *specific* nesting — outer Eta over Eio →
`Effect.sync` → inner `Eio.Switch.run` → inner `Runtime.create`/`Runtime.run`
on the *same* `sw` and `clock` — is the textbook anti-shape.

**Right shape:**
```ocaml
let with_client ~net ~clock … f =
  Eta.Effect.scoped @@
  Eta.Effect.acquire_release
    ~acquire:(Eta.Effect.sync (fun () ->
      (* connect + spawn read loop *)))
    ~release:(fun client -> Eta.Effect.sync (fun () -> close client))
  |> Eta.Effect.bind f
```
…where the read-loop fiber is started via `Eta.Effect.Private.daemon` (or
the supervisor scope) in the outer runtime. No nested runtime, no nested
switch.

---

### N2. `pulse/client.ml`: a *second* nested runtime inside a daemon fiber, plus double-buffering
**File:** `packages/camelpie/pulse/client.ml:188-217`
**Severity:** Architectural
**Status:** OPEN

```ocaml
let with_record_stream (T t) … f =
  let acquire =
    Eta.Effect.sync (fun () ->
      …
      let queue = Eio.Stream.create 64 in
      Hashtbl.add t.record_queues info.stream_index queue;
      let chan = Eta.Channel.create ~capacity:64 () in
      Eio.Fiber.fork_daemon ~sw:t.sw (fun () ->
        let rt = Eta.Runtime.create ~sw:t.sw ~clock:t.clock () in
        try
          while true do
            let chunk = Eio.Stream.take queue in
            match Eta.Runtime.run rt (Eta.Channel.send chan chunk) with
            | Ok () -> ()
            | Error _ -> ()
          done;
          `Stop_daemon
        with Eio.Cancel.Cancelled _ -> `Stop_daemon
      );
      …
```

Three serious problems:

1. **A fresh `Eta.Runtime.create` is made per record stream and used solely
   to forward each chunk to a channel.** That is one runtime per stream and
   `Runtime.run` per chunk for what should be a single send. Fork bombs
   waiting to happen if multiple record streams open.
2. **The `bytes Eio.Stream.t` was supposed to disappear** from this layer
   (round 1, B1). Instead the agent kept the `Eio.Stream` (it is still in
   `t.record_queues : (int, bytes Eio.Stream.t) Hashtbl.t` at line 17 of
   the same file) and stacked an `Eta.Channel` on top of it, with a
   translator daemon fiber. Two queues (capacity 64 + 64), two
   synchronization points, plus the runtime overhead. The boundary was
   not lifted; it was just hidden under a copy fiber.
3. **`Error _ -> ()` silently drops `Channel.send` failures.** If the
   channel is closed before the consumer drains, chunks are discarded
   without notice.

**Right shape:** keep one queue. The cleanest choice is to use
`Eta.Channel.t` directly in `record_queues` and call `Eta.Channel.try_send`
(or the equivalent runtime hook) from `process_incoming`. If the read loop
absolutely must remain a raw Eio fiber (because `process_incoming` is
called outside any Eta runtime), keep the `Eio.Stream.t` and expose a
*pull effect* in `with_record_stream` instead of building a third `Channel`
out of it:

```ocaml
val with_record_stream :
  t -> ?options:_ ->
  (Defs.record_stream_info ->
   (bytes, [> `Closed ]) Eta.Effect.t (* a recv effect, not a Channel.t *) ->
   ('a, 'err) Eta.Effect.t) ->
  ('a, 'err) Eta.Effect.t
```

…where the `recv` effect is just `Eta.Effect.sync (fun () -> Eio.Stream.take queue)`
plus a `Closed` check. One queue, no extra fiber, no nested runtime.

---

### N3. `Sh` module: `Obj.t` global mutable singleton for the process manager
**File:** `packages/camelpie/sh.ml:1-9`, `bin/pie.ml:5`
**Severity:** Architectural, type-system bypass
**Status:** OPEN

```ocaml
let process_mgr_ref : Obj.t option ref = ref None

let set_process_mgr (mgr : 'a Eio.Process.mgr) =
  process_mgr_ref := Some (Obj.repr mgr)

let get_process_mgr () : 'a Eio.Process.mgr =
  match !process_mgr_ref with
  | None -> failwith "Sh: process manager not set"
  | Some obj -> Obj.obj obj
```

This is `Obj.magic` for what should be a typed dependency. Three sins
in a six-line module:

1. **Type-system bypass.** `Obj.repr`/`Obj.obj` round-trip a polymorphic
   capability through `Obj.t` because the agent could not figure out how
   to express `'a Eio.Process.mgr` as a value-stored type. Fix: use the
   concrete row-poly type (`[> `Generic ] Eio.Process.mgr_ty Eio.Resource.t`
   like `pulse/client.mli` does), or thread the manager explicitly.
2. **Global mutable state for an Eio capability.** The whole point of Eio's
   capability passing is that capabilities are *not* ambient. The agent
   re-introduced ambient capabilities via a singleton.
3. **`failwith "Sh: process manager not set"`.** Anything that uses `Sh.run`
   without the singleton having been initialized first (tests, library use,
   any code path reachable before `bin/pie.ml`'s init) crashes. The init
   ordering is fragile and undocumented.

**Right shape:** thread `process_mgr` through the capabilities the same way
`net` and `clock` are already threaded. Or even cleaner — make the helpers
`Eta.Effect.t`-shaped and stash `process_mgr` in the runtime via a custom
capability, or just put it in a record and pass it explicitly. Delete
`Obj.repr`/`Obj.obj`.

---

### N4. `Sh.run` / `Sh.run_capture` are not effects
**File:** `packages/camelpie/sh.mli`
**Severity:** Architectural, contract leak
**Status:** OPEN

```ocaml
val find_executable : string -> string option
val run : prog:string -> args:string list -> unit
val run_capture : prog:string -> args:string list -> string
```

The previous review explicitly specified:
```ocaml
val run_capture :
  ?timeout:Duration.t ->
  prog:string -> args:string list ->
  ((int * string), [> `Subprocess_error of string ]) Eta.Effect.t
```

What got shipped: synchronous, no timeout, no error type, exception-only.
Every caller now wraps `Sh.run` in `Eta.Effect.sync (fun () -> try … with
exn -> failwith …)` — see `xdotool.ml`, `wtype.ml`, `niri.ml`,
`notification.ml`, `portal.ml`. That `try … with exn -> failwith` ritual
appears **eight times** across those five files. The duplication just
moved from the temp-file-and-`Sys.command` ritual to a try-with-failwith
ritual.

**Right shape:** make the effect-shaped signature the only public API.
The exception-throwing `Eio.Process.run` calls become an internal detail,
wrapped once in `run_capture` returning `(string, [> `Subprocess_error of string ])
Eta.Effect.t`. Every caller deletes its `try … with exn -> failwith`.

---

### N5. `stt/openrouter.ml`: Eio operations inside `Effect.Blocking.submit`
**File:** `packages/camelpie/stt/openrouter.ml:30-46`
**Severity:** Documented contract violation, undefined behavior per Eta docs
**Status:** OPEN

```ocaml
let transcribe ~api_key ~model ~language pcm_bytes =
  Eta.Effect.Blocking.submit (fun () ->
    let tmp_wav = Filename.temp_file "pie_stt" ".wav" in
    …
    Fun.protect
      ~finally:(fun () -> Sys.remove tmp_wav)
      (fun () ->
        match transcribe_file_impl ~api_key ~model ~language tmp_wav with
        …))

(* transcribe_file_impl calls Sh.run_capture which calls Eio.Process.parse_out *)
```

From `Eta.Effect.Blocking` (in `effect.mli`):
> "Blocking worker callbacks are ordinary synchronous OCaml functions. They
> may call legacy blocking libraries. **They must not call Eio operations**,
> run Eta runtimes, submit nested blocking jobs, or resolve parent-domain
> promises as supported API. **Doing any of those is undefined behavior;
> Eta does not guarantee deadlock, panic, or correctness for it.**"

`Sh.run_capture` calls `Eio.Process.parse_out`. That's an Eio operation.
This effect calls it from a blocking-pool worker. **Undefined behavior.**

The hot irony: the temp-file scaffolding (`Filename.temp_file`,
`open_out_bin`, `Sys.remove`) inside the closure *is* legitimate blocking
work that justifies `Effect.Blocking.submit`, but the actual subprocess
launch via `Sh.run_capture` is not.

**Right shape:** split into:
1. `Effect.Blocking.submit (fun () -> Filename.temp_file …; write_pcm; tmp_wav)`
   to produce the path.
2. `Sh.run_capture` (as an Eta effect, per N4) for the curl call.
3. `Effect.Blocking.submit (fun () -> Sys.remove tmp_wav)` for cleanup.

Or even cleaner: skip the temp file entirely, write the WAV to curl's stdin
via `Eio.Process.spawn` + `Eio.Flow.copy`. (Subprocess stdin support is in
the same Eio API the agent already uses.)

---

### N6. `niri/niri.ml`: shell injection regression
**File:** `packages/camelpie/niri/niri.ml:14-18`
**Severity:** Security regression
**Status:** OPEN

```ocaml
let type_text_impl text =
  let niri = … in
  Sh.run ~prog:niri
    ~args:["msg"; "action"; "spawn"; "sh"; "-c"; "wtype -- " ^ text]
```

The original (pre-refactor) was:
```ocaml
Printf.sprintf "%s msg action spawn sh -c 'wtype -- %s'"
  niri (Filename.quote text)
```

The agent moved off `Sys.command` (good) but dropped `Filename.quote` along
the way (bad). The user-controlled `text` is now spliced into a `sh -c`
command unescaped. Niri spawns sh, sh interprets metacharacters in `text`.
A user transcription containing `; rm -rf ~` ends up executed.

**Right shape:** drop the `sh -c` wrapper entirely. Either invoke `wtype`
directly (the wtype branch already exists in `wayland/wtype.ml`), or pass
the text via `--` and let niri handle the spawn safely:

```ocaml
Sh.run ~prog:"niri"
  ~args:["msg"; "action"; "spawn"; "--"; "wtype"; "--"; text]
```

…assuming niri's `spawn` parses `--`. If not, escape `text` with a real
shell-quoting function before splicing.

---

### N7. `niri/niri.ml`: `--json` argument-order regression
**File:** `packages/camelpie/niri/niri.ml:5`
**Severity:** Behavior regression
**Status:** OPEN

```ocaml
let run_niri_json args =
  Sh.run_capture ~prog:"niri" ~args:(args @ ["--json"]) …
```

Called with `run_niri_json ["msg"; "focused-window"]` → produces
`niri msg focused-window --json`. Original was `niri msg --json focused-window`
(`--json` after `msg`, before the subcommand).

Whether modern niri accepts the new ordering is undocumented. The original
ordering matched niri's CLI convention. **Don't move flags around as a
side-effect of a refactor.**

**Fix:** either pass `--json` in the right slot:
```ocaml
let run_niri_json args = Sh.run_capture ~prog:"niri" ~args:("--json" :: args)
```
…and adjust callers, or have callers pass the full arg list.

---

### N8. `bin/pie.ml status_cmd`: silent error masking with fake snapshot
**File:** `packages/camelpie/bin/pie.ml:101-117`
**Severity:** Behavior regression
**Status:** OPEN

```ocaml
let status_cmd =
  …
  Eta.Effect.sync (fun () ->
    let snap =
      try Camelpie.Daemon.Client.status ()
      with Camelpie.Daemon.Client.Client_error _ ->
        { Camelpie.Daemon.Contract.active = false
        ; enabled = false
        ; mode = None
        ; error = None }
    in
    Printf.printf "active=%b enabled=%b mode=%s error=%s\n%!" …)
```

Original behavior on daemon-down was `Printf.printf "off\n"`. New behavior
prints `active=false enabled=false mode=none error=none` — indistinguishable
from a daemon that is up but idle.

This is exactly the "silent fallback to paper over an unclear invariant"
the skill flags as a presumptive blocker. The whole reason `Client_error`
exists is so the caller can render a different status. The fix here was
not to swallow the error.

**Fix:** keep the `try … with Client_error _ -> Printf.printf "off\n"`
shape, or model the result as a typed
`[ `Off | `Snapshot of recording_snapshot ]` and render each branch.

---

### N9. `daemon/client.ml`: still raw blocking `Unix.*`, just wrapped at call sites
**File:** `packages/camelpie/daemon/client.ml`, all of it
**Severity:** Round 1 finding S5 not actually fixed
**Status:** OPEN

The agent's summary acknowledges this: *"Kept sync functions in
daemon/client.ml, wrapped at call sites in bin/pie.ml with
Eta.Effect.Blocking.submit"*. The previous review was explicit:

> "rewrite using `Eio.Net.connect ~sw env#net (\`Unix path)` +
> `Eio.Buf_write` / `Eio.Buf_read.line` … But the proper fix is Eio
> sockets — `Effect.blocking` is for *legacy* sync I/O, and your own
> JSON-over-Unix-socket protocol is not legacy."

A protocol that camelpie itself defines, in its own repo, in
`daemon/contract.ml`, is not legacy. Wrapping it in `Effect.Blocking.submit`
just spends a slot on the bounded blocking pool (default 128 threads) for
~10 ms of work. This is the rearrange-don't-delete trap, taken literally.

The right fix is one `daemon/client.ml` rewrite over `Eio.Net` returning
`('a, [> `Daemon_off | `Daemon_error of string ]) Eta.Effect.t`. After
that the seven `Eta.Effect.Blocking.submit` wrappers in `bin/pie.ml`
collapse into seven direct effect calls.

---

### N10. `read_line ()` inside `Effect.sync` — unbounded runtime-fiber freeze
**Files:**
- `packages/camelpie/commands/ptt.ml:31, 50`
- `packages/camelpie/commands/stt_interactive.ml:28, 50`
- `packages/camelpie/commands/assistant.ml:41, 64`

**Severity:** Runtime correctness
**Status:** OPEN

```ocaml
Eta.Effect.sync (fun () -> read_line ())
```

`read_line` is a synchronous stdlib call that blocks the OCaml thread until
the user hits Enter — potentially forever. Inside `Effect.sync`, that blocks
the current fiber, which freezes any same-fiber tracing/metric emission and
blocks the Eta runtime's progress on this task indefinitely.

The Eta `Effect.blocking` doc:
> "Anti-pattern: do not call [f ()] directly inside an Eio fiber for legacy
> blocking work. The V-Blocking-A research measured about 49 ms heartbeat
> freeze for a 50 ms direct blocking call. Always go through
> [Effect.blocking]."

`read_line` from a TTY can block for *minutes*.

**Fix options:**
1. Use `Eio.Buf_read.line` over `Eio.Stdenv.stdin env`, wrapped as an
   `Eta.Effect.t`. That fiber-suspends correctly and can be cancelled.
2. If the user-input pattern is "press Enter to start, press Enter to stop",
   model that as a small input fiber producing `[ `Started | `Stopped ]`
   into a channel, and `Eta.Channel.recv` from the command logic.

Path (1) is enough.

---

### N11. `Eta.Mutable_ref` misused for sequential same-fiber accumulation
**Files:**
- `packages/camelpie/commands/ptt.ml`
- `packages/camelpie/commands/record.ml`
- `packages/camelpie/commands/assistant.ml`
- `packages/camelpie/commands/stt_interactive.ml`

**Severity:** Wrong primitive choice; over-engineering
**Status:** OPEN

`Eta.Mutable_ref` is documented as:
> "Named shared mutable cell backed by `Atomic.t`. … 'this is shared mutable
> state across fibers'."

Every use here is sequential single-fiber accumulation inside a `bind`
chain — no concurrent writers. A plain `let chunks = ref []` in OCaml does
the job with zero atomic CAS overhead. `update` retries on CAS failure,
which is wasted work for code that has no contender.

**Fix:** replace with normal `ref`, or — better — thread the list through
`Eta.Effect.bind` as the bind value rather than mutating an external cell:

```ocaml
let rec recv_loop acc =
  Eta.Effect.bind (fun chunk -> recv_loop (chunk :: acc))
    (Eta.Channel.recv chan)
```

This also kills the "list-reverse-at-end" footgun.

---

### N12. Typed error rows are fictional
**Files:**
- `packages/camelpie/pulse/client.mli` (declares `[> `Pulse_error of string ]`)
- `packages/camelpie/wayland/wtype.ml` (declares `[`Wtype_error of string]`, never produced)
- `packages/camelpie/x11/xdotool.ml` (declares `[`Xdotool_error of string]`, never produced)
- `packages/camelpie/wayland/portal.ml` (declares `[`Portal_error of string]`, only partially produced)
- `packages/camelpie/desktop/notification.ml` (declares `[`Notification_error of string]`, never produced)
- `packages/camelpie/niri/niri.ml` (declares `[`Niri_error of string]`, only partially produced)
- `packages/camelpie/stt/openrouter.ml` (declares `[`Stt_error of string]`, never produced from `transcribe`)
- `packages/camelpie/input/text_injection.ml` (raises `Injection_error` instead of `Eta.Effect.fail`)

**Severity:** Contract lie
**Status:** OPEN

Every public effect in these files declares a typed-error polymorphic
variant in its `.mli`, and every implementation uses `failwith` /
`raise (Failure …)` / `raise (Client_error …)` / `raise (Injection_error …)`
internally instead of `Eta.Effect.fail`. At runtime the failures escape
as OCaml exceptions, which Eta records as `Cause.Die` (defects), not
`Cause.Fail err` (typed failures).

**Effect of this:** `Eta.Effect.catch (fun (`Wtype_error msg) -> …) eff`
will never be invoked — the failure path is `Cause.Die`, which `catch`
does not handle.

The whole point of declaring the row was so callers could pattern-match it.
That contract is a lie today.

**Fix:** every place these `.ml` files do `failwith "X"`, change to
`Eta.Effect.fail (`X_error "msg")`. Where the failure happens *inside*
an `Effect.sync` callback, restructure so the failure is produced by the
effect-level `bind`/`map`, not the OCaml-level exception:

```ocaml
let type_text text =
  match Sh.find_executable "xdotool" with
  | None -> Eta.Effect.fail (`Xdotool_error "xdotool not found in PATH")
  | Some xdotool ->
    Eta.Effect.catch
      (fun e -> Eta.Effect.fail (`Xdotool_error (Printf.sprintf "xdotool failed: %s" e)))
      (Sh.run ~prog:xdotool ~args:["type"; "--clearmodifiers"; "--"; text])
```

…assuming `Sh.run` is itself a typed-error effect (per N4).

---

### N13. Control-flow inversion in `commands/`
**Files:**
- `packages/camelpie/commands/ptt.ml` (53 lines, but read inside-out)
- `packages/camelpie/commands/assistant.ml` (75 lines)
- `packages/camelpie/commands/stt_interactive.ml` (53 lines)

**Severity:** Legibility / maintainability
**Status:** OPEN

These three files are now nested-`bind` trees where the *first* thing the
program does (print prompt + `read_line`) lives at the *bottom* of the file,
inside the deepest `Eta.Effect.bind … (Eta.Effect.sync (fun () -> …))`
parenthesis. Reading order is reversed from execution order. Sample from
`ptt.ml` lines 8–53:

```ocaml
Eta.Effect.bind (fun pcm_bytes ->            (* step 3: post-recording  *)
  …
  Eta.Effect.bind (fun text ->                (* step 4: got transcript *)
    Eta.Effect.bind (fun result ->            (* step 5: got typed text *)
      Eta.Effect.sync (fun () -> …))         (* step 6: print summary  *)
    ) (Input.Text_injection.inject text)
  ) text_eff
) (Eta.Effect.bind (fun () ->                 (* step 2: stopped recording *)
    …
  ) (Eta.Effect.sync (fun () ->               (* step 1: prompt + start *)
    Printf.printf "PTT %s mode: …";
    ignore (read_line ());
    …
  )))
```

This is "rearrange complexity, don't delete it" in pure form. The fix is
to use `let%bind` / `let*` with a PPX (or build a small local `let*`
operator using `Eta.Effect.bind` / `Eta.Effect.( >>= )`):

```ocaml
let ( let* ) eff f = Eta.Effect.bind f eff in
let run ~net ~clock ~mode () =
  Pulse.Client.with_client ~net ~clock @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info chan ->
  let* () = print_prompt mode in
  let* () = wait_for_enter () in
  let* pcm = capture_until_enter chan in
  let* text = Stt.Openrouter.transcribe … pcm in
  let* result = Input.Text_injection.inject text in
  print_summary text result
```

Read top-to-bottom, executes top-to-bottom. Each helper extracted (so
`commands/ptt.ml` shrinks from 53 lines to ~15) eliminates the duplication
that exists today between `ptt.ml`, `stt_interactive.ml`, and `assistant.ml`
all having near-identical "drain channel until Enter, build PCM, normalize,
transcribe, inject" pipelines.

---

### N14. `bin/pie.ml`: 327 lines of repeated `run_effect (fun ~env -> …)` boilerplate
**File:** `packages/camelpie/bin/pie.ml`
**Severity:** Decomposition
**Status:** OPEN

The previous review asked for one `run_effect` and minimal boilerplate.
What's there: one `run_effect`, plus 18 separate
`const (fun … -> run_effect (fun ~env -> …))` closures, each ~5–10 lines.
The file grew from 271 lines to 327.

A small helper collapses that:

```ocaml
let cmd_effect name doc args mk_eff =
  Cmd.v (Cmd.info name ~doc) Term.(args $ const (fun a ~env -> mk_eff a ~env))
```

…or even simpler, a `cmd_simple` for the common "no args" case. Combined
with the `let*` cleanup in N13, the whole file shrinks meaningfully.

Six of those closures wrap **`Daemon.Client.*`** in
`Eta.Effect.Blocking.submit` (toggle, pause, resume, meeting_start,
meeting_stop, meeting_toggle). Once N9 is fixed those become direct effect
calls. So part of the bloat dissolves automatically once the daemon client
is rewritten.

---

## P1 — Smaller things, each genuinely worth addressing

### B1. `pulse/client.ml`: `read_loop` hashtable iteration on error
**File:** `pulse/client.ml:88-97`

```ocaml
| exn ->
  let err = error … in
  Hashtbl.iter (fun _ resolver -> Eio.Promise.resolve_error resolver err) t.pending;
  `Stop_daemon
```

Round 1 flagged `Hashtbl.copy` as a race "fix". The agent removed the copy
but the underlying issue is unchanged: `read_loop` and `await_reply` both
read/write `t.pending` from different fibers, and fan-out resolve on error
iterates a structure that may be mutated mid-iteration if an `await_reply`
times out at the same instant. Same-domain Eio fibers cannot preempt
mid-statement, so iteration itself is safe — but the `Hashtbl.iter` happens
in the error path *only*; on the success path the `Hashtbl.remove` /
`Hashtbl.add` interleaving is fine. So this one is borderline acceptable
as long as the file documents the invariant. Adding a one-line comment is
sufficient.

---

### B2. `pulse/client.ml`: synchronous `await_reply` blocks `Effect.sync`
**File:** `pulse/client.ml:100-110`, `with_client`/`with_record_stream`
acquire/release

`await_reply` calls `Eio.Promise.await` and `Eio.Time.with_timeout_exn`
synchronously. The new `with_client` / `with_record_stream` wrap that
inside an `Eta.Effect.sync` callback, which means the Eta runtime fiber
*does* suspend correctly on the underlying Eio promise (because `Effect.sync`
is interpreted on top of Eio). So this is actually OK in practice — but
the public surface for connect/disconnect is currently exception-based.
Lifting `await_reply` to a typed `(_, [> `Pulse_error of string ]) Eta.Effect.t`
makes the surface honest.

---

### B3. `pulse/client.mli`: `get_server_info` and `list_sources` are still raw
**File:** `pulse/client.mli:25-26`

```ocaml
val get_server_info : t -> Defs.server_info
val list_sources : t -> Defs.source_info list
```

Synchronous, raise `Client_error`, do not compose with Eta. These are
currently consumed only inside `commands/sources.ml` via `Pulse.Client.list_sources client`
inside an `Eta.Effect.sync`. That works (because of B2) but is the same
contract leak as the rest of the file. Lift to
`(_, [> `Pulse_error of string ]) Eta.Effect.t`.

---

### B4. `with_record_stream` release runs `await_reply` in `Effect.sync`
**File:** `pulse/client.ml:209-215`

The `release` effect does:
```ocaml
let packet = Commands.build_delete_record_stream_command info.stream_index in
ignore (await_reply (T t) packet);
```

If the daemon is dying or the read loop has already failed, `await_reply`
hangs until its timeout (default 15s). That is a 15-second hold during
shutdown for every recording stream. Either reduce the timeout for
shutdown-side `delete_record_stream`, or make this a fire-and-forget
write without waiting for the reply.

---

### B5. `wakeword_train_cmd.ml` / `wakeword_tune_cmd.ml`: duplicated `mkdir_p`
**Files:** `commands/wakeword_train_cmd.ml:9-13`, `commands/wakeword_tune_cmd.ml:23-27`

Identical 4-line `mkdir_p` defined locally in both files. Move to a
shared `Camelpie.Fs` (or similar) helper. While there, both files do
`open_out_bin` + `output_string` + `close_out` on the runtime fiber —
fine for a one-shot CLI but should be `Effect.sync` for consistency with
the rest of the codebase, or `Eio.Path.with_open_out`.

---

### B6. `wtype.ml`: 40 lines of nested `bind/seq/sync` for a 5-step linear flow
**File:** `wayland/wtype.ml:21-50`

The clipboard-fallback logic is one linear sequence (save → copy → paste →
restore) that is now expressed as a 7-deep nested `Eta.Effect.bind / seq /
sync` tree. With the `let*` operator from N13:

```ocaml
let ( let* ) eff f = Eta.Effect.bind f eff in
…
let* prev = save_clipboard () in
let* () = copy normalized in
let* () = paste () in
match prev with
| Some prev -> restore prev
| None -> Eta.Effect.unit
```

…that's roughly 8 lines in place of 30, and it reads top-to-bottom.

The current code also drops cancellation propagation in two of the nested
`try … with exn -> failwith …` branches (any `Eio.Cancel.Cancelled` raised
during a clipboard step gets caught and re-raised as `Failure …`). The
`save_clipboard` branch correctly re-raises `Cancelled`; the other three
do not. Pick a uniform policy.

---

### B7. `text_injection.ml`: ambient env reads, exception leak
**File:** `input/text_injection.ml:21-30, 32-40`

`detect_session` reads three env vars directly. `inject` raises
`Injection_error` from inside the function body, before producing the
effect — so the typed-error claim in `.mli` is only fictional half the
time (the exception escapes before any Eta layer can typecast it).

**Fix:** wrap the whole body in `Eta.Effect.sync` / `Eta.Effect.fail`
so all failures flow through the typed channel, and pass session
detection as a parameter or stash in a small capability record.

---

### B8. `commands/ptt.ml`: `mode` is now typed but both branches still equal
**File:** `commands/ptt.ml`

The thermo-nuclear pass flagged that PTT had `if mode = "translate" then
… else …` doing identical things in both arms. The agent fixed the type
(`[`Transcribe | `Translate]`, good) — but the `Stt.Openrouter.transcribe`
call no longer branches on it at all. Either:
1. Wire `model` / `language` to differ between modes, or
2. Delete the unused mode parameter and the duplicate `ptt_transcribe_cmd`
   / `ptt_translate_cmd` `Cmd.v` definitions in `bin/pie.ml`.

Currently the user sees three subcommands (`ptt`, `ptt-transcribe`,
`ptt-translate`) all doing exactly the same thing. That is a UX bug.

---

## What got better since round 1

Real wins, worth keeping:

- **`Eio.Switch.run` / `Eio_main.run` centralized.** No more per-command
  switch and runtime construction. ✅
- **`Sys.command` eliminated entirely.** The replacement `Sh` has structural
  problems (N3, N4) but the security and cancellation properties of
  `Eio.Process` are a net upgrade. ✅
- **`Hashtbl.copy` removed** from `await_reply`. ✅
- **`mkdir` only catches `EEXIST`** instead of every error. ✅
- **PTT mode is now typed** instead of stringly-typed. ✅
- **`bin/pie.ml`'s `run_effect` is a real `Eta.Runtime.run` wrapper** with
  proper exit codes for typed failure / interrupt / success. ✅
- **The build is reportedly green.** (Could not verify locally — no deps in
  the repo's opam switch.)

---

## Approval bar — round 2

Still **fails** on multiple presumptive blockers:

- ❌ Architectural: nested Eta runtimes inside `Effect.sync` (N1, N2),
  forbidden patterns inside `Effect.Blocking.submit` (N5).
- ❌ Type-system bypass: `Obj.magic` global singleton (N3).
- ❌ Round-1 finding S5 not actually fixed, just deferred via blocking pool
  (N9).
- ❌ Security regression: shell injection in `niri/niri.ml` (N6).
- ❌ Behavior regressions: `--json` arg order (N7), silent-error fake
  status (N8).
- ❌ Typed-error contracts in `.mli` files are fictional (N12).
- ❌ Commands grew nested-bind unreadable (N13) and `bin/pie.ml` grew
  larger from copy-pasted `run_effect` closures (N14).

The original goal — *"everything that can be async should be async"* — is
genuinely closer, but several of the new sins are worse than the ones they
replaced. I would not merge this branch.

---

## Recommended next pass — minimum sufficient

In strict priority order:

1. **Delete `Obj.t` from `sh.ml` (N3)** and make `Sh.run` /
   `Sh.run_capture` typed `Eta.Effect.t` (N4). One file. Unblocks
   removing all the `try … with exn -> failwith` rituals downstream.
2. **Fix the security regression in `niri/niri.ml` (N6).** Two lines.
3. **Restore `status_cmd`'s "off" behavior (N8).** Three lines.
4. **Fix `--json` arg ordering in `niri.ml` (N7).** One line.
5. **Rewrite `pulse/client.ml`'s `with_client` and `with_record_stream`
   without nested runtimes (N1, N2).** Use `Eta.Effect.scoped` +
   `acquire_release` over the parent runtime; expose a `recv` effect
   instead of building a second `Eta.Channel` on top of `Eio.Stream`.
6. **Move `Effect.Blocking.submit` off the Eio.Process call in
   `openrouter.ml` (N5).** Use it only for the temp-file scaffold.
7. **Replace `read_line` calls with `Eio.Buf_read.line` over stdin (N10).**
8. **Rewrite `daemon/client.ml` over `Eio.Net` (N9).**
9. **Introduce `let*` via `Eta.Effect.bind` and rewrite `commands/ptt.ml`,
   `assistant.ml`, `stt_interactive.ml` top-to-bottom (N13). Extract the
   shared "drain → encode → transcribe → inject" pipeline once.**
10. **Make every `failwith` in the subprocess wrappers and
    `pulse/client.ml` a typed `Eta.Effect.fail` (N12).**
11. Remaining P1 items as cleanup.

After step 9 alone, `commands/{ptt,stt_interactive,assistant}.ml` should
share one helper and shrink to ~15 lines each. After step 8, the
`Eta.Effect.Blocking.submit` calls in `bin/pie.ml` collapse into direct
calls. After step 10, the `.mli` typed-error rows are no longer fictional.
That is what "actually delete the complexity" looks like.
