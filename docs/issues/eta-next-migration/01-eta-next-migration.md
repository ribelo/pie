---
status: planned
package: pie
consumer: pie
---

# Pie on eta-next

## Problem Statement

Pie still builds against v1 Eta's effect interpreter, HTTP/WebSocket transport, provider façade and streams. Its microphone, keyboard, assistant, control socket and CLI share ownership and cancellation contracts that a mechanical symbol rename would break. The installed assistant is in active use; preparation and later validation must not replace its executable or restart its systemd service.

This is row 32, step 1 of 2: dependency preparation and a dispatchable whole-port specification, **not a source port**. The audited pin is eta-next `786876a6c08776badccd3a366c601c1c1086d12d`. Step 2 is not complete until every unit below is integrated and verified. Kernel blockers are explicit; no v1 bridge or consumer-local transport/runtime substitute is permitted.

## Solution

Use direct Eio calls, explicit capabilities and switches, and polymorphic-variant results. Keep exceptions for defects, with `Eta.Scope.run` at boundaries. Move Codex sessions to `Eta_ai_realtime.Session`, OpenRouter calls to `Eta_ai_openrouter.Responses` and `Audio`, the executable to `Eta_app`'s Door, and the control listener to `Eta_ipc.Listener`. Preserve Pie's application codecs, capture/admission policy and retry storage. Apply the [row-27 stream verdict](</home/ribelo/projects/ribelo/ocaml/eta-next/docs/issues/stream/01-stream.md>): finite data uses `Seq`; fallible live audio uses consumer-owned pulls and existing state plus Eio synchronization, not a new stream framework.

## User Stories

1. As a Pie maintainer, I want one pinned eta-next dependency graph, so that no v1 runtime survives an apparently successful migration. ^pienext-8r9p
2. As a dictation user, I want bounded audio capture, release-tail handling, resampling and settlement to retain their behavior, so that push-to-talk neither loses audio nor admits overlapping attempts. ^pienext-rgpp
3. As a Codex user, I want each realtime operation to own one connection through readiness, callbacks, terminal outcome and cleanup, so that cancellation and a stalled peer cannot leak readers or deliver incomplete text. ^pienext-jlmd
4. As an OpenRouter user, I want Responses and audio requests to preserve prompts, outcomes and budgets, so that only a completed valid transcription or translation reaches delivery. ^pienext-hd67
5. As a Pie operator, I want the whole CLI and assistant lifecycle under one Door, so that commands, configuration, logging and signal shutdown have one owner. ^pienext-ipne
6. As a control client, I want the existing one-line protocol and socket safety under eta_ipc, so that status, pause, resume, toggle and actions remain compatible. ^pienext-x9f5
7. As a maintainer, I want deterministic regression tests and disjoint port ownership, so that parallel workers cannot erase each other's behavior or files. ^pienext-b3x8
8. As the operator of the installed assistant, I want all migration checks isolated from production, so that the running service stays usable. ^pienext-fn6g

## Requirements

- The Pie build shall depend only on eta-next's consolidated Eta packages and direct Eio libraries, without `eta_eio`, `eta_http_eio`, `eta_http_tls_openssl`, `eta_otel_eio`, `eta_stream`, `eta_stream_eio` or `eta_component`. ^pienext-5l91
- When a live audio pull fails, Pie shall propagate that failure distinctly from EOF; while audio remains queued, Pie shall preserve the owner's existing delivery/discard policy and maintain frame/byte admission and accounting under the same lock. ^pienext-6nnq
- When a terminal realtime event is received, Pie shall decode and fold that event before stopping and shall issue no subsequent receive; if a text callback fails, Pie shall stop before another pull. ^pienext-w6d8
- When an operation ends by success, typed failure, defect or cancellation, Pie shall await its owned children and cleanup before releasing admission; if independent failure information remains, Pie shall not turn it into success or discard it as an expected stop. ^pienext-4ps6
- When OpenRouter returns a non-completed outcome, a tool continuation, malformed structured text or an oversized result, Pie shall reject delivery and retain a sanitized diagnostic without response bodies, credentials or user content. ^pienext-nkfe
- When the user invokes an existing Pie command or CLI override, the Door-hosted application shall preserve its meaning, output and exit behavior; while a recording attempt is active, the effective configuration shall remain stable for that attempt. ^pienext-1wgu
- When a control peer sends a malformed or overlong request, the listener shall preserve the existing malformed-request JSON reply where the peer can still receive it, without starting an action; while handlers are admitted, the listener shall bound peers and cancel disconnected work. ^pienext-sw3t
- When the assistant loads its configuration and prompt files, Pie shall retain the existing path, missing-file diagnostics, bounded regular-file reads, validation and reload/last-good policy. ^pienext-g7hc
- While optional OTLP export is disabled or failing, Pie shall retain usable stderr diagnostics; when shutdown is requested, owned exporter work shall settle within its configured bounds after final application records. ^pienext-gaq3
- While preparing or validating this migration, workers shall use private build/output directories and isolated fixture sockets and shall not modify the installed Pie executable, shared OPAM switch, live credentials or systemd service. ^pienext-xfx7
- The final port shall preserve the current PCM, queued-byte, item and result bounds, and shall test provider encoding ceilings explicitly rather than silently reducing the accepted capture domain. ^pienext-bee9

## Implementation Decisions

- `Effect.pure/fail/map/bind/sync/sync_exn/from_result/flatten_result` become ordinary values, result matching and direct functions. Deferred hooks become `unit -> ('a, 'err) result` only where the owner must decide when to invoke them; no replacement effect DSL. Update old tests about *effect construction* to assert admission at the explicit invocation boundary.
- `Eta.Syntax` is not a replacement API. Use local result operators or matches. Do not catch all exceptions as ordinary errors; cancellation propagates unchanged. Preserve failure composition at actual ownership boundaries, not the old interpreter's data representation. An unresolved loss of simultaneous cleanup/body failures is a blocker, not permission to delete assertions.
- `Eta.Concurrent.with_background ~sw background body` owns fail-fast reader/body work; `par` and `all` join siblings. `race` is **first success**, and returns a `Cause.t` error if all branches fail: use `Eta.Timeout` for deadlines, not a success race with a timeout error. Do not rebuild v1 daemon or named-background machinery.
- `Eta.Resource.acquire_release ~sw ~acquire ~release` registers a unit-returning release on the supplied switch. A typed close failure must be handled and retained by the operation's direct result/settlement policy, not silently discarded or presented as an ordinary success. Finalizer defects and interrupts belong to `Scope.run`; use child switches for operation lifetime rather than registering every operation on the process switch.
- `Eta.Promise`, `Semaphore`, `Ref` and `Spi.Expert` calls are not kernel demands: use `Eio.Promise.create/resolve/await`, `Eio.Semaphore.make/acquire/release`, direct refs/atomics where appropriate, `Eio.Mutex.use_rw`, `Eio.Condition.await/broadcast`, and direct I/O. Never suspend between checking an admission limit and recording the admitted bytes/items outside the protecting lock.
- Keep low-level control **client** request framing in Pie using direct Eio and `Eta.Timeout`. eta_ipc currently exports a listener, not a client; rewriting the thin existing client is not copying the listener. Preserve request-correlated response validation in the pure Contract codec.
- `Eta_app.SERVICE.create ~sw config deps` is an application assembly boundary, not a replacement component registry. The assistant's dependencies remain explicit. `status_query` combines status functions and `serve ~sw ()` waits; neither is a second process runner. Remove the old Process interpreter/signal owner when Door owns the executable.
- Preserve the local stderr channel, but adopt eta-next's structured JSON logger rather than claiming byte-identical v1 logfmt output. Convert diagnostic tests to parse structured records and assert the same redaction/categories. Optional exporter configuration is consumer-owned environment decoding feeding `Eta_otel.create`, not a demand for `Eta_otel_eio.with_otlp`.

## Testing Decisions

| Coverage | Stories/requirements | Verification in step 2 |
|---|---|---|
| Dependency identity | pienext-8r9p, pienext-5l91 | Static metadata and forbidden-symbol audit; clean private `@install` build after all ports |
| Audio, ownership, budgets | pienext-rgpp, pienext-6nnq, pienext-4ps6, pienext-bee9 | Capture-clock, Pulse-liveness, PTT/provider/end-to-end budgets, incremental capture, streaming-resampling, assistant-owner and lifecycle-soak suites |
| Realtime | pienext-jlmd, pienext-w6d8 | Session/realtime outcomes; fake WS peer with EOF, binary/malformed event, stalled readiness, callback failure, terminal frame followed by poison frame, and FD/fiber census |
| OpenRouter | pienext-hd67, pienext-nkfe | Local HTTP fixtures: completed/incomplete/cancelled/failed/missing status, completed+function_call, structured JSON shape, model mismatch, exact deadline and size boundaries |
| Door/config/diagnostics | pienext-ipne, pienext-1wgu, pienext-g7hc, pienext-gaq3 | Bare invocation and existing flags, every old command, config-boundary/live-config, local-diagnostics/diagnostic-content, SIGTERM and exporter drain under a temporary HOME/XDG tree |
| IPC | pienext-x9f5, pienext-sw3t | Existing control-protocol/socket-ownership fixtures: malformed/overlong JSON, response mismatch, sixteen admitted peers, disconnect, stale/replaced endpoint and lock identity |
| Safe integration | pienext-b3x8, pienext-fn6g, pienext-xfx7 | Full suites and `@fmt` in private build tree; no live provider tests unless separately authorized; installed executable checksum and live service state unchanged |

Use `Eta_test.run` only for mock-clock tests. It supplies real OS/net/fs/process capabilities alongside mock clocks, so fixtures still need private paths. Use `advance`, `fork_run`, `await` and `with_census` where applicable. Socket tests needing real elapsed time use `Eio_main.run` plus `Eta.Scope.run`; manually advance mock monotonic time separately from wall time. Do not remove tests because v1's runner is absent.

## Out of Scope

This preparation does not port OCaml sources, fix eta-next, deploy, install, restart services or commit. No domain redesign, new queue/stream library, compatibility runtime, shadow HTTP/WS transport, copied IPC server, credential rewrite, or provider-network test is authorized here. The lead commits the preparation and dispatches the following units.

## Further Notes: Prepared State and Production Safety

- [flake.nix](</home/ribelo/projects/ribelo/pie/flake.nix>) now selects `git+file:///home/ribelo/projects/ribelo/ocaml/Eta?ref=eta-next`; [flake.lock](</home/ribelo/projects/ribelo/pie/flake.lock>) locks the revision above. Only the eta node and its required new `oxcaml-lsp` / `oxcaml-merlin-src` subtree nodes may change. Every pre-existing non-eta node is retained unchanged.
- [dune-project](</home/ribelo/projects/ribelo/pie/dune-project>) and regenerated [pie.opam](</home/ribelo/projects/ribelo/pie/pie.opam>) require OCaml >= 5.4.0 and `eta`, `eta_ai`, `eta_ai_openrouter`, `eta_ai_realtime`, `eta_app`, `eta_http`, `eta_ipc`, `eta_json`, `eta_observability`, `eta_otel`, with `eta_test` test-only. Existing non-Eta dependencies are retained.
- All affected package/test Dune library lists use the target package names; duplicates from consolidation are removed. Sources deliberately still reference v1, so this preparation is not expected to compile the whole application yet.
- Pre-existing edits are preserved: removal of `eta_component` from metadata/install lists, removal of an obsolete workaround paragraph in [docs/missing-eta-api.md](</home/ribelo/projects/ribelo/pie/docs/missing-eta-api.md>), and named v1 `pulse.client.reader` background work in [pulse/client.ml](</home/ribelo/projects/ribelo/pie/packages/pie/pulse/client.ml>). The latter is superseded by direct structured ownership only in step 2. Existing scratch/output/transcription files remain untouched.
- Read-only production inspection found an active `pie.service`. Its Nix-store `ExecStart` wrapper exports the key and `PATH`, then execs `/home/ribelo/.cache/opam/5.4.0+ox/bin/pie`. Never overwrite, remove or rebuild that shared-switch executable; never stop/restart the unit during migration. Do not execute `eta-opam-install-ox`, the flake's shared-switch install helper, or [scripts/test-local-eta.sh](</home/ribelo/projects/ribelo/pie/scripts/test-local-eta.sh>): they can replace the executable used by the service. U5 updated the helper package names, but this does not make it a safe migration test instruction.

## Kernel Gaps Visible at the Preparation Pin

These are missing observable contracts, not requests to restore v1 names. Draft stories below are consumer pulls for eta-next work; API spellings marked **proposed** are not available at the pin. Kernel changes land and are verified there before the affected worker integrates them. No worker may fill a gap with a private implementation of the same infrastructure.

### G1 — Responses discards tool-continuation evidence (blocks U3 acceptance)

**Exact absence:** [lib/ai_openrouter/responses.mli](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/ai_openrouter/responses.mli>) exposes `response = { id; model; status; text }`, and [lib/ai_openrouter/responses.ml](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/ai_openrouter/responses.ml>) returns no text for non-message/non-output_text items. Thus `decode`/`create` accept a `status="completed"` reply with a `function_call` plus valid text but erase the call. [stt/openrouter.ml](</home/ribelo/projects/ribelo/pie/packages/pie/stt/openrouter.ml>) rejects tool continuation, and [test/openrouter_outcomes/test_openrouter_outcomes.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/openrouter_outcomes/test_openrouter_outcomes.ml>) has exactly that fixture. Checking `status` alone cannot preserve it.

**Draft eta-next story:** As a Pie Responses caller, I want decoded responses to retain a typed indication of tool continuation/refusal/non-text output that can prevent text delivery, so that completed status does not authorize a tool-dependent answer. ^pienext-93ym

**Proposed API:** extend `Responses.response` with an outcome/output discriminator sufficient to distinguish those cases (or return a typed non-deliverable error from `decode/create` for this text-only endpoint). Preserve the response status and forbid silently dropping disqualifying items. Kernel test: completed+function_call+valid text is distinguishable from completed text-only; Pie fixture still rejects it. Do not bypass `Responses.create` with a private HTTP/JSON provider.

### G2 — IPC peer failures cannot produce protocol replies (blocks U4 listener replacement)

**Resolved (eta-next b39988bf nipc-c871; Pie U4):** malformed/oversized lines get `{"error":"malformed-request"}` via `?error_response`. Pie's old whole-peer 1 s budget is preserved as **300 ms ingress + 400 ms handler + 300 ms egress** (sum 1 s), 16 active peers, 64 KiB line and reply caps.


**Exact absence:** [lib/ipc/listener.mli](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/ipc/listener.mli>) offers only `on_peer_error : peer_error -> unit`; `decode` errors and `Ingress_too_large` never reach `handler` or `encode`. The callback has neither response return nor flow. Pie's [assistant_service/server.ml](</home/ribelo/projects/ribelo/pie/packages/pie/assistant_service/server.ml>) sends `{"error":"malformed-request"}` plus newline for malformed JSON/codec failures and overlong lines. Mapping malformed input to a fake successful request does not address oversized ingress and is not the intended boundary.

**Draft eta-next story:** As a Pie control-listener owner, I want a bounded optional peer-error response encoder, so that malformed/overlong requests receive the existing protocol error without admitting a handler. ^pienext-pyo6

**Proposed API:** a listener-owned `?error_response : peer_error -> 'response option` (or equivalent typed ingress/decode-specific hook), encoded by the existing `encode` and subject to `max_reply_bytes`/`egress`. No reply for disconnect/EOF; no secondary error overwrites the original cause. Verify malformed and oversized line replies, no action admission, stalled writer timeout, and one response maximum. Preserve the old one-second peer budget explicitly when assigning ingress/handler/egress budgets; do not assume three independent one-second stages equal the old whole-peer bound.

### G3 — Door cannot inject capabilities into extra commands or parse service-specific terms (blocks U4 CLI)

**Exact absence:** [lib/app/eta_app.mli](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/app/eta_app.mli>) accepts prebuilt `(unit -> int) Cmdliner.Cmd.t list` and a service callback of `Env.t -> config -> sw -> result`. It exposes no typed service `Cmdliner.Term.t` argument or environment-aware command builder. [lib/app/eta_app.ml](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/app/eta_app.ml>) creates the Eio environment internally and constructs the service term from `--config` alone. Pie's [bin/pie.ml](</home/ribelo/projects/ribelo/pie/packages/pie/bin/pie.ml>) has default-service `--ptt-transcribe-keysym` and `--ptt-translate-keysym` overrides, while sources/meter/record/type/status/toggle/pause/resume/retry/msg commands need Eio capabilities and result/exit handling. `service_command` only renames the service verb. Starting the old Process runner inside an extra command would nest Eio loops and retain the old lifecycle owner.

**Draft eta-next story:** As the Pie CLI owner, I want typed service arguments and capability-bearing extra commands under the same Door, so that all existing verbs and default-service overrides run without a second runner. ^pienext-zyck

**Proposed API:** a typed service argument term supplied to the service callback and a command-builder/command callback contract that receives Door `Env.t` plus an owned switch and returns a typed outcome. Expose both through `run` and `run_for_test`; keep one signal/exit boundary. Test bare invocation with both overrides, every existing verb, errors, help, and cancellation of a long-lived extra command. Do not parse `Sys.argv` a second time or capture capabilities in global refs.

**U4 CLI diagnostic ruling:** malformed daemon JSON reports typed protocol error `malformed-response` on stderr and exits nonzero. The previous `Json_error` exception class name was an implementation detail, not a user-facing contract; successful output, command exits, and daemon protocol replies retain their contracts.

### G4 — Door lacks the existing config source/read policy (blocks U4 full config integration)

**Exact absence:** [lib/app/config.ml](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/app/config.ml>) resolves a fixed `<XDG_CONFIG_HOME>/<name>/config.toml`; [lib/app/eta_app.mli](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/app/eta_app.mli>) has no default-source override or production reader hook (only `run_for_test` has `read`). Pie's [config/stt.ml](</home/ribelo/projects/ribelo/pie/packages/pie/config/stt.ml>) loads [Paths.stt_config_path](</home/ribelo/projects/ribelo/pie/packages/pie/paths.ml>) (`stt.toml`), requires that source, reads bounded regular files, resolves prompt files relative to it, and has legacy-file diagnostics and live per-attempt reload. Simply handing Door the same type would change startup input and missing-file behavior before Pie validation runs. TOML parsing itself is already supported and is **not** a gap.

**Draft eta-next story:** As a Pie operator, I want the Door to accept an application-declared config source and bounded read/load policy, so that adopting its lifecycle does not rename or relax the validation of my existing configuration files. ^pienext-y5y2

**Proposed API:** a source/default-path and required/default-missing policy, plus a production reader/loader seam also honored by `config check` and `run_for_test`; carry the resolved source to application validation/prompt resolution. The application still owns live reload and stable attempt snapshots, not the Door. Verify stt.toml, missing-file/legacy diagnostics, explicit override, oversized/non-regular files and relative prompt paths. Do not manufacture a second empty Door config that ignores the application's real config, rewrite argv, or move the user's file as a migration shortcut.

### G5 — Audio has a smaller non-configurable local input ceiling (blocks full size-domain equivalence in U3)

**Exact absence:** [lib/ai_openrouter/audio.ml](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/ai_openrouter/audio.ml>) hard-codes an 8 MiB multipart body limit; `Audio.body` and `Audio.create` have no limit argument. [dictation_limits.ml](</home/ribelo/projects/ribelo/pie/packages/pie/dictation_limits.ml>) allows 64 MiB PCM, and [stt/openrouter.ml](</home/ribelo/projects/ribelo/pie/packages/pie/stt/openrouter.ml>) admits that bound before WAV/multipart encoding. The new local ceiling can reject an otherwise admitted capture before the provider sees it. This is an input-policy mismatch, not a claim that a remote model guarantees acceptance of 64 MiB.

**Draft eta-next story:** As a Pie audio caller, I want an explicit bounded request-size policy covering the WAV and multipart overhead, so that the kernel does not silently replace my existing capture admission bound with 8 MiB. ^pienext-osw0

**Proposed API:** an optional validated `max_request_bytes` shared by `Audio.body` and `Audio.create`, with safe default and typed oversize result before network I/O; alternatively the lead must explicitly approve a changed Pie size contract before port acceptance. Test just below/at/above the selected encoded-body ceiling and the consumer's PCM boundary. Do not split one utterance into ad-hoc requests or copy multipart transport into Pie.

**Already available, not gaps:** `Eta_http.Client.request ?response_idle` supplies the OAuth response-idle timeout; `Ws_client.connect ?establishment_timeout` and `Session.connect` bound DNS/TCP/TLS/upgrade only; `Eta_otel.create/reporter/span_ids` plus Door `telemetry` cover explicit exporter assembly. `Eta_stream`, Mailbox and Drain_counter must not be requested. Compound failure preservation needs integration tests against the new `Scope/Concurrent` semantics; do not promise identical v1 Cause trees.

# PORT PLAN

## Audit Boundary and Inventory

The source audit found **38 files with literal `Eta_` references (37 .ml and one .mli)**, not 37 total. A broader `Eta` scan yields **57 .ml/.mli files** because core `Eta.*`, interface types and provenance text also matter. Every one is assigned below. Rows containing only provenance text still get an owner. The IPC server and config boundaries have no Eta literal but are additionally included because the requested migration changes them.

Inventory paths are repository-relative labels linked to the actual files. Ownership is file-level, not symbol-level; no two workers edit the same file. Shared metadata is frozen from step 1 and belongs only to U5 during integration. Each owner may edit its existing companion interfaces and tests listed in its ownership boundary; new files must stay within that boundary or be agreed with the lead first.

## Exact Target Contracts to Freeze Before Dispatch

| Contract | Target and rule |
|---|---|
| Core direct work | `Eta.Scope.run (fun sw -> result)`; `Eta.Concurrent.with_background/par/all/race ~sw`; `Eta.Timeout` for deadlines; `Eta.Resource.acquire_release` for switch releases. Consult [lib/eta/concurrent.mli](</home/ribelo/projects/ribelo/ocaml/eta-next/lib/eta/concurrent.mli>) for first-success vs fail-fast distinction. |
| Process capabilities | Remove `Process.run` as runner. U1 provides explicit HTTP construction and telemetry assembly (proposed Pie helpers `Process.make_http ~sw ~net ~clock`, `Process.telemetry ~sw env config`). Callers receive `~clock`, `~mono_clock`, `~logger`, `~http`, `~net`, `~process_mgr` only as needed; no ambient HTTP/logger refs. |
| Telemetry | `Eta_http.Client.create ~sw ~net ~clock ~tls ()`; `Client.tls ?ca_file ()`; `Eta_otel.create ~sw ~clock ~http_client ~endpoint ~service_name`, then `reporter`, `span_ids`, `shutdown`. Exporter root follows HTTP creation, application resources follow exporter; LIFO teardown leaves HTTP usable during export drain. `Eta_observability.Logger.info/warn/error/span` replace effect logging. |
| Audio pulls | Consumer signature `unit -> (bytes option, 'err) result` for fallible live PCM. Finite fixtures/chunks use `Seq.t`. A queue that must close/fail is stdlib `Queue` plus existing owner state guarded by `Eio.Mutex`/`Condition`, not a bare `Eio.Stream`. Plain test rendezvous may use `Eio.Stream.create/add/take`. |
| HTTP and OAuth | `Eta_http.Client.request http ~method_ ~url ~headers ~body ?deadline ~response_idle ~max_body_bytes ()`; use `response.status/body`. OAuth `response_idle` is 30s, not a replacement total request deadline. Headers are string pairs, body bytes; encode forms/query via `Eta_http.Url.encode_component`. Preserve auth ownership/atomic publication and `SSL_CERT_FILE`. |
| Realtime transport | `Eta_ai_realtime.Session.connect ~sw ~net ~clock ~api_key:(Eta_observability.Redact.make token) ~headers ?ca_file ~establishment_timeout url`; `Session.send session json`, `receive session`, `close session`. `Session.event = {kind; raw}`; fold raw JSON through Pie's existing event policy, do not route it through a generic chat adapter. Avoid duplicating the Authorization header; keep originator/version/User-Agent/session ID extras. |
| WS internals/fixtures | Underlying `Eta_http.Ws_client.connect/send_text/send_binary/receive/close` only where a transport-level test needs it; pure fixture codec is `Eta_http.Ws_codec.encode/decode/accept_key/key_of_nonce`. Application sessions use eta_ai_realtime, not a second WS transport. |
| Responses | `Eta_ai_openrouter.Responses.create ~http ~clock ~logger ~api_key ?endpoint ~deadline request`; request fields `model; prompt; wav; temperature; text_format; reasoning_effort`. Preserve `Json_schema {name;schema;strict}`, `status="completed"`, bounded structured output decode and G1. |
| Audio transcription | `Eta_ai_openrouter.Audio.create ~http ~api_key ?endpoint ~deadline {model;filename;wav;language;prompt;temperature}`; returns text or `Eta_ai.Error.t`. Preserve prompt/temperature and G5; WAV comes from Pie's pure PCM code. |
| Provider policy | API key is a `Redact.t`; derive explicit endpoint from `OPENROUTER_BASE_URL`, not the deleted `provider/responses_provider` builders. Keep 60s direct transcription / 120s Responses whole-operation limits, context/prompt policy, model checks and 1 MiB result bound. Exhaustively sanitize `Eta_ai.Error.t`; its `Invalid_response` can carry user data, so do not blindly call its pretty-printer into logs. |
| IPC listener | `Eta_ipc.Listener.run ~sw ~fs ~net ~clock ~path ~policy ~decode ~encode ~budget ~handler ~on_peer_error ()`. Startup returns after admission starts. Set peers/backlog 16 and max request line 65536 to match Pie, explicitly assign reply/stage bounds and G2. Preserve pure `Contract.request_of_json/response_to_json`; delete server-owned bind/probe/lock/accept/cleanup machinery rather than leave two owners. |
| IPC client | ``Eio.Net.connect ~sw (`Unix path)``, `Eio.Flow.write`, `Eio.Buf_read.line`, request-correlated Contract decoder, and `Eta.Timeout` at one second. Child switch per request, including failed connect; cancellation is not Daemon_off. |
| Door | `Eta_app.run ~name:"pie" ~config ~validate ~telemetry ~pp_err`, service `create ~sw config deps`, `status_query`, `serve`, `request_stop`. G3/G4 extend this surface before whole-CLI/config integration; proposed names are not claimed to exist. |
| Tests | `Eta_test.run/advance/fork_run/await/with_census` or explicit `Eio_main.run` + `Eta.Scope.run`. Replace old streams with `Seq`, pulls, Eio promises or rendezvous streams; retain concrete assertions for cancellation, leak counts, deadline origin and sanitized error classes. |

## Per-file Mapping and Ownership

| Unit | File | Target modules/functions and retained contract |
|---|---|---|
| U1 | [desktop/notification.ml](</home/ribelo/projects/ribelo/pie/packages/pie/desktop/notification.ml>) | Direct Sh command results; Logger debug/warn, cancellation unchanged. |
| U1 | [desktop/notification.mli](</home/ribelo/projects/ribelo/pie/packages/pie/desktop/notification.mli>) | Direct result and explicit capability signatures matching notification implementation. |
| U1 | [input/text_injection.ml](</home/ribelo/projects/ribelo/pie/packages/pie/input/text_injection.ml>) | Direct clipboard/wtype/xdotool calls; result callbacks, Logger.span, scoped cleanup for clipboard ownership. |
| U1 | [input/text_injection.mli](</home/ribelo/projects/ribelo/pie/packages/pie/input/text_injection.mli>) | Direct result signatures; preserve prepared text bytes and cancellation boundaries. |
| U1 | [keyboard/monitor.ml](</home/ribelo/projects/ribelo/pie/packages/pie/keyboard/monitor.ml>) | Concurrent.with_background; Eio promises, mutex/condition and direct bus reads; switch-owned setup, bus identity, input-loss and exactly-once close. |
| U1 | [niri/niri.ml](</home/ribelo/projects/ribelo/pie/packages/pie/niri/niri.ml>) | Direct Sh calls; Result composition for focused-window JSON; explicit logger/process capabilities. |
| U1 | [process.ml](</home/ribelo/projects/ribelo/pie/packages/pie/process.ml>) | Process capabilities + Telemetry: delete Eta_eio interpreter, object capabilities and custom process signal owner; explicit Client/Logger/OTLP assembly for Door. |
| U1 | [sh.ml](</home/ribelo/projects/ribelo/pie/packages/pie/sh.ml>) | Direct Eio.Process.run/parse_out with ~process_mgr/~sw, explicit command result and cancellation; Logger.span instead of Effect wrappers. |
| U1 | [sh.mli](</home/ribelo/projects/ribelo/pie/packages/pie/sh.mli>) | Replace Effect.t with direct result signatures; expose only required process/switch/logger capabilities. |
| U1 | [test/clipboard_owner/test_clipboard_owner.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/clipboard_owner/test_clipboard_owner.ml>) | Resource/Scope, direct result callbacks and Eio promises; ownership cancellation and cleanup failures |
| U1 | [test/focus_rules/test_focus_rules.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/focus_rules/test_focus_rules.ml>) | direct text-injection/niri results; Logger and preserved rule selection |
| U1 | [test/keyboard_bus_boundary/test_keyboard_bus_boundary.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/keyboard_bus_boundary/test_keyboard_bus_boundary.ml>) | direct Eio bus fixtures and Scope; malformed/oversized messages and sender identity |
| U1 | [test/keyboard_ownership/test_keyboard_ownership.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/keyboard_ownership/test_keyboard_ownership.ml>) | Concurrent.with_background, Scope and Eio promises; setup/input loss/descriptor cleanup |
| U1 | [test/prepared_text/test_prepared_text.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/prepared_text/test_prepared_text.ml>) | direct injection result and Eio process fixtures; exact whitespace preservation |
| U1 | [wayland/clipboard.ml](</home/ribelo/projects/ribelo/pie/packages/pie/wayland/clipboard.ml>) | Direct process I/O and Resource.acquire_release; child-switch clipboard owner and explicit cleanup/failure handling. |
| U1 | [wayland/clipboard.mli](</home/ribelo/projects/ribelo/pie/packages/pie/wayland/clipboard.mli>) | Direct result callbacks with explicit switch/process capabilities. |
| U1 | [wayland/wtype.ml](</home/ribelo/projects/ribelo/pie/packages/pie/wayland/wtype.ml>) | Direct Sh result; preserve argument separation and text whitespace. |
| U1 | [x11/xdotool.ml](</home/ribelo/projects/ribelo/pie/packages/pie/x11/xdotool.ml>) | Direct Sh result; preserve current key/text behavior and error mapping. |
| U2 | [commands/meter.ml](</home/ribelo/projects/ribelo/pie/packages/pie/commands/meter.ml>) | Direct Pulse acquisition and fallible pull loop; Eio.Time.sleep/explicit clocks; no Stream.run_fold. |
| U2 | [commands/record.ml](</home/ribelo/projects/ribelo/pie/packages/pie/commands/record.ml>) | Direct Pulse pull and scoped WAV/file output; preserve capture duration, cleanup and errors. |
| U2 | [commands/sources.ml](</home/ribelo/projects/ribelo/pie/packages/pie/commands/sources.ml>) | Direct Pulse list_sources result and ordinary formatting; logger/clock are explicit. |
| U2 | [pulse/client.ml](</home/ribelo/projects/ribelo/pie/packages/pie/pulse/client.ml>) | Concurrent.with_background for reader; Eio promises/semaphore; consumer Queue+Mutex+Condition with close/error states; direct socket codec reads/writes; Resource.acquire_release. |
| U2 | [pulse/client.mli](</home/ribelo/projects/ribelo/pie/packages/pie/pulse/client.mli>) | Replace Effect/Stream signatures with direct results and fallible live PCM pulls; callbacks are explicit functions, not latent effects. |
| U2 | [test/capture_clock/test_capture_clock.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/capture_clock/test_capture_clock.ml>) | Eta_test.run/advance and Eio.Time.Mono; remove sync wrapper, assert separate wall/mono clocks |
| U2 | [test/capture_clock/test_mono_race.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/capture_clock/test_mono_race.ml>) | Eta_test.fork_run/await, Concurrent.race/par, Eio.Time.Mono.sleep and Eio.Stream.take; cancellation unregisters sleepers |
| U2 | [test/diagnostic_recording/test_diagnostic_recording.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/diagnostic_recording/test_diagnostic_recording.ml>) | direct Pulse pull fixtures, Eio promises and Scope; faithful PCM/partial-file cleanup |
| U2 | [test/incremental_capture/test_incremental_capture.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/incremental_capture/test_incremental_capture.ml>) | finite Seq and fallible pulls; Eio synchronization; incremental statistics, not whole-history rescans |
| U2 | [test/pulse_liveness/test_pulse_liveness.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/pulse_liveness/test_pulse_liveness.ml>) | Concurrent.with_background, Promise/Condition; bounded audio must not block protocol replies |
| U2 | [test/streaming_resampling/test_streaming_resampling.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/streaming_resampling/test_streaming_resampling.ml>) | fallible/finite chunk pulls and Session/WS fixtures; preserve cross-chunk resampler state |
| U3 | [stt/codex_realtime.ml](</home/ribelo/projects/ribelo/pie/packages/pie/stt/codex_realtime.ml>) | Realtime transport + HTTP/OAuth + Audio pulls: Session.connect/send/receive/close; Client.request ~response_idle; Url.encode_component; Concurrent.with_background and atomic bounded queue; Seq for finite chunks; inclusive terminal fold. |
| U3 | [stt/openrouter.ml](</home/ribelo/projects/ribelo/pie/packages/pie/stt/openrouter.ml>) | Responses.create and Audio.create; Redact.make; explicit http/clock/logger; structured output and status checks, sanitized Ai.Error mapping; G1/G5 gate equivalence. |
| U3 | [test/credential_ownership/test_credential_ownership.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/credential_ownership/test_credential_ownership.ml>) | direct OAuth Client.request and Scope; atomic credential publication/conflict; private files only |
| U3 | [test/explicit_prompts/test_explicit_prompts.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/explicit_prompts/test_explicit_prompts.ml>) | Responses/Audio request fixtures, direct result calls; no implicit project enrichment |
| U3 | [test/live_codex_transcribe.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/live_codex_transcribe.ml>) | Direct real-clock Eio_main/Scope and explicit provider capabilities; live-network executable remains opt-in, never run during preparation. |
| U3 | [test/live_openrouter_transcribe.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/live_openrouter_transcribe.ml>) | Direct real-clock Eio_main/Scope and explicit provider capabilities; live-network executable remains opt-in, never run during preparation. |
| U3 | [test/live_openrouter_translate.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/live_openrouter_translate.ml>) | Direct real-clock Eio_main/Scope and explicit provider capabilities; live-network executable remains opt-in, never run during preparation. |
| U3 | [test/openrouter_outcomes/test_openrouter_outcomes.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/openrouter_outcomes/test_openrouter_outcomes.ml>) | Responses.create fixtures and Scope; add G1 disqualifier checks without weakening status/structured-output cases |
| U3 | [test/provider_budgets/test_provider_budgets.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/provider_budgets/test_provider_budgets.ml>) | Client/Session plus Timeout; admission bytes/results/deadline origin; add G5 encoded-body edge cases |
| U3 | [test/realtime_outcomes/test_realtime_outcomes.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/realtime_outcomes/test_realtime_outcomes.ml>) | Session.send/receive plus Ws_codec test peer; explicit pull fold, terminal statuses and callback errors |
| U3 | [test/session_ownership/test_session_ownership.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/session_ownership/test_session_ownership.ml>) | Session lifecycle with child Scope, census and Eio promises; shutdown against a peer that stays open |
| U3 | [test/ws_settlement_probe.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/ws_settlement_probe.ml>) | Ws_client/Session under operation Scope; explicit real-clock cancellation and descriptor census; private loopback peer. |
| U4 | [assistant_service/client.ml](</home/ribelo/projects/ribelo/pie/packages/pie/assistant_service/client.ml>) | IPC client contract: direct Eio request/response and one-second Timeout; remove effect wrapping, keep protocol errors distinct. |
| U4 | [assistant_service/client.mli](</home/ribelo/projects/ribelo/pie/packages/pie/assistant_service/client.mli>) | Direct result signatures plus explicit clock capability needed by Timeout. |
| U4 | [assistant/service.ml](</home/ribelo/projects/ribelo/pie/packages/pie/assistant/service.ml>) | Direct create ~sw config deps/service callbacks; Concurrent.with_background/par, Eio promises and owner switches; Logger spans; preserve attempt config, retry publication, keyboard loss, cleanup and admission. |
| U4 | [bin/pie.ml](</home/ribelo/projects/ribelo/pie/packages/pie/bin/pie.ml>) | Whole CLI on Eta_app.run, environment-aware command assembly and service terms after G3/G4; no Process.run/nested Eio_main. |
| U4 | [ptt_loop.ml](</home/ribelo/projects/ribelo/pie/packages/pie/ptt_loop.ml>) | Fallible audio pulls, explicit Eio clocks, Promise/Mutex/Condition, Concurrent/Timeout; preserve post-roll, monotonic min-hold, byte/item bounds and settlement. |
| U4 | [recording/coordinator.ml](</home/ribelo/projects/ribelo/pie/packages/pie/recording/coordinator.ml>) | Direct synchronized admission/state and explicit result hooks; Scope/Cause only for classified operation outcome, not effect construction. |
| U4 | [test/assistant_owner/test_assistant_owner.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/assistant_owner/test_assistant_owner.ml>) | direct service result/thunks, Concurrent/Scope and Eio promises; admission through final settlement and stage-aware recovery |
| U4 | [test/config_boundary/test_config_boundary.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/config_boundary/test_config_boundary.ml>) | direct Config/Stt service calls and Scope; bounded regular reads, validation, prompt paths |
| U4 | [test/live_config/test_live_config.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/live_config/test_live_config.ml>) | direct callbacks and Eio promises; last-good config and stable attempt snapshots |
| U5 | [test/build_record/provenance.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/build_record/provenance.ml>) | No runtime API port: update Eta source/build provenance text and pin claims to eta-next; retain evidence of the actual build. |
| U5 | [test/diagnostic_content/test_diagnostic_content.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/diagnostic_content/test_diagnostic_content.ml>) | Logger.Memory/Reporter records and direct provider failures; no credentials/prompt/transcript/raw body |
| U5 | [test/end_to_end_budgets/test_end_to_end_budgets.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/end_to_end_budgets/test_end_to_end_budgets.ml>) | direct application/provider calls, Scope and Eio coordination; full-stack byte/result/deadline bounds |
| U5 | [test/lifecycle_soak/test_lifecycle_soak.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/lifecycle_soak/test_lifecycle_soak.ml>) | direct Eio/Scope and census; repeated Pulse/WS/assistant lifetimes with stable descriptors |
| U5 | [test/local_diagnostics/test_local_diagnostics.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/local_diagnostics/test_local_diagnostics.ml>) | Door telemetry + Logger.Reporter/Memory; independent stderr with absent/failing exporter |
| U5 | [test/ptt_budgets/test_ptt_budgets.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/ptt_budgets/test_ptt_budgets.ml>) | direct PTT pull and callback results; Eio promises/Condition, Timeout; queue/capture/admission/finalizer regressions |
| U5 | [test/test_pie.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/test_pie.ml>) | Entire monolithic suite belongs to U5: replace all v1 runners/effects/streams with direct results, Scope/Eta_test and pull fixtures after U1–U4 APIs stabilize; preserve protocol, ownership, terminal and cleanup cases. |

## Five Disjoint Work Units

| Unit | Exclusive file boundary beyond its inventory rows | Deliverable and dependency |
|---|---|---|
| **U1 — Direct foundations** | All production files in its rows; existing companion interfaces only. Its named test suites and their local Dune files. | Freeze explicit capability/result signatures first, then direct process/desktop/keyboard adapters and telemetry. No dependency on other Pie units. Kernel G3/G4 affect final Door use, not foundational implementation. |
| **U2 — Pulse and capture primitives** | [audio/pcm.ml](</home/ribelo/projects/ribelo/pie/packages/pie/audio/pcm.ml>), [audio/pcm.mli](</home/ribelo/projects/ribelo/pie/packages/pie/audio/pcm.mli>), [audio/resampler.ml](</home/ribelo/projects/ribelo/pie/packages/pie/audio/resampler.ml>), [audio/resampler.mli](</home/ribelo/projects/ribelo/pie/packages/pie/audio/resampler.mli>), pure files in the Pulse directory, and its named test suites/local Dune files, **only when signature/fixture integration requires edits**. | After U1 logger/capability contract: direct Pulse reader/control path and exact pull contract; preserve pure codecs/resampler, do not rewrite them gratuitously. Can run concurrently with U3. |
| **U3 — Providers** | Its two STT production files, listed tests and local test Dune files. Root test Dune stays U5. | After U1 HTTP/logger contract: realtime and OpenRouter rewrite, its own live-input queue semantics, provider fixtures. Uses the frozen pull type, not U2 internals. G1/G5 block full acceptance, not independent OAuth/WS work. |
| **U4 — Application, Door and IPC** | In addition to its rows: [assistant_service/server.ml](</home/ribelo/projects/ribelo/pie/packages/pie/assistant_service/server.ml>), [assistant_service/server.mli](</home/ribelo/projects/ribelo/pie/packages/pie/assistant_service/server.mli>), [assistant_service/contract.ml](</home/ribelo/projects/ribelo/pie/packages/pie/assistant_service/contract.ml>), [assistant_service/contract.mli](</home/ribelo/projects/ribelo/pie/packages/pie/assistant_service/contract.mli>), [config/stt.ml](</home/ribelo/projects/ribelo/pie/packages/pie/config/stt.ml>), [config/stt.mli](</home/ribelo/projects/ribelo/pie/packages/pie/config/stt.mli>), [paths.ml](</home/ribelo/projects/ribelo/pie/packages/pie/paths.ml>), [paths.mli](</home/ribelo/projects/ribelo/pie/packages/pie/paths.mli>), [dictation_limits.ml](</home/ribelo/projects/ribelo/pie/packages/pie/dictation_limits.ml>), [dictation_limits.mli](</home/ribelo/projects/ribelo/pie/packages/pie/dictation_limits.mli>); PTT pure state files; [recording/phase.ml](</home/ribelo/projects/ribelo/pie/packages/pie/recording/phase.ml>), [recording/retry_store.ml](</home/ribelo/projects/ribelo/pie/packages/pie/recording/retry_store.ml>), [recording/retry_store.mli](</home/ribelo/projects/ribelo/pie/packages/pie/recording/retry_store.mli>) when needed; the `control_protocol`, `control_socket_ownership`, `recovery_store`, `lifecycle_phase` and `cli_status` test directories, including their local Dune files. Recording-attempt/stage-aware-recovery cases inside the monolithic suite remain U5-owned. | Contract/IPC/config work may begin alongside U2/U3 after U1 freeze. Final assistant/PTT assembly needs U2+U3. G2–G4 gate server/whole-Door integration. Own all public service/CLI entry changes, never Process implementation (U1). |
| **U5 — Integration and build evidence** | [test/test_pie.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/test_pie.ml>), its listed suites; [test/dune](</home/ribelo/projects/ribelo/pie/packages/pie/test/dune>), [dune](</home/ribelo/projects/ribelo/pie/packages/pie/dune>), [bin/dune](</home/ribelo/projects/ribelo/pie/packages/pie/bin/dune>), [pie.ml](</home/ribelo/projects/ribelo/pie/packages/pie/pie.ml>), [pie.mli](</home/ribelo/projects/ribelo/pie/packages/pie/pie.mli>); [flake.nix](</home/ribelo/projects/ribelo/pie/flake.nix>), [flake.lock](</home/ribelo/projects/ribelo/pie/flake.lock>), [dune-project](</home/ribelo/projects/ribelo/pie/dune-project>), [pie.opam](</home/ribelo/projects/ribelo/pie/pie.opam>), [scripts/test-local-eta.sh](</home/ribelo/projects/ribelo/pie/scripts/test-local-eta.sh>), [README.md](</home/ribelo/projects/ribelo/pie/README.md>), [CONTEXT.md](</home/ribelo/projects/ribelo/pie/CONTEXT.md>), [docs/missing-eta-api.md](</home/ribelo/projects/ribelo/pie/docs/missing-eta-api.md>) and this ticket; remaining unassigned test/build/documentation files. | Own monolithic acceptance, final linking, pin update after kernel fixes, stale install-helper/docs correction, provenance, whole-tree format/test checks. Start test planning early; final pass follows U1–U4. No install/commit/deploy. |

Pure files listed as conditional ownership need not change. No other production files are in scope without a concrete discovered caller/signature dependency and lead-approved ownership amendment; [fs.ml](</home/ribelo/projects/ribelo/pie/packages/pie/fs.ml>) and [fs.mli](</home/ribelo/projects/ribelo/pie/packages/pie/fs.mli>) remain unchanged unless U4 requests an explicit handoff. No worker edits another worker's regression suite to force it green. A finding in a foreign file is sent to its owner/lead. Root metadata is U5-only after this preparation.

## Dependency Graph and Dispatch Order

```text
eta-next G1/G5 ----------------------> U3 acceptance
eta-next G2/G3/G4 -------------------> U4 acceptance
U1 explicit capability/result seam --> U2 Pulse/audio -------+
                                   --> U3 providers --------+--> U4 assembly
                                   --> U4 IPC/config prep   |
U1 + U2 + U3 + U4 + closed gaps ----------------------------+--> U5 final gates
```

1. Lead lands preparation only; kernel gap owners take the linked consumer draft stories. Freeze U1 helper signatures and the live pull shape before workers implement callers.
2. Dispatch U1 first; once signatures are agreed, U2 and U3 are independent. U4 may work on Contract/IPC/config without editing their files. U5 may adapt test plans, but does not guess changing production signatures.
3. Integrate U2/U3 before U4 assistant/PTT assembly. Close G2–G4 before claiming the control server and entire executable use kernel ownership. Land verified kernel fixes, then U5 alone refreshes the eta subtree pin.
4. U5 runs all relevant private tests, full install target and formatter, checks old APIs are absent, then reports completion to the lead. A metadata-only successful generation is not a successful application port.

## Specific Audio and Cancellation Acceptance Notes

- Pulse's bounded audio queue remains separate from the protocol-control reader: retaining bytes in a slow consumer must not prevent request promises from resolving. Preserve its 64-frame bound and existing error/close semantics, not just PTT's downstream 1024-item / 1 MiB queue.
- PTT capture admission is 64 MiB PCM; result text is 1 MiB. Increment/decrement counters with queue mutation under one mutex. A close/error racing admission cannot admit new data or lose accounting. Failed/cancelled enqueue must not leave bytes charged.
- `Seq` is for finite, infallible chunk transformations, not a hidden suspending/network stream. Live pulls distinguish `Ok None`, `Ok (Some bytes)`, and `Error e`. Do not map an error to `None` or expose a closed flag unrelated to the queue lock.
- On realtime terminal input, fold the inclusive terminal event and callback result before choosing return; do not translate the old `take_until` chain into an exclusive `take_while`. Readiness requires the expected session event/ack, not merely a successful TCP/WebSocket connection.
- Frame and byte ceilings, errors-vs-EOF, configured drain/discard, producer cancellation, stopped receiver, monotonic elapsed capture and cross-chunk resampling each need a regression. A process-wide uncancelled fiber is never an acceptable escape from operation cleanup.
- Preserve successful-delivery vs retained-failure recording policy, no provider rerun after text was prepared, and no new attempt before prior finalizers settle. Tests that currently inspect v1 Cause composition should verify the same independently meaningful failures using the new public classification; if the kernel cannot represent required information, stop and file that exact counterexample rather than synthesize a compatibility interpreter.

## Preparation Verification and Step-2 Gates

U5 integration acceptance is evidence-based, not a deployment. The final pass
requires a forbidden-v1-symbol census in application/test sources and package
metadata, whole-tree `dune fmt`, private `@install` build, every registered
`dune test` suite, `@fmt`, and the `.#pie` flake artifact. Record any failing
suite with its root repair; do not exclude or silence it. Remove `_build_u5`
afterward. The live microphone/key check is **prepared, not executed**:
[scripts/live-ptt-check-eta-next](</home/ribelo/projects/ribelo/pie/scripts/live-ptt-check-eta-next>)
prints the operator-only stop/check/restart sequence, requires explicit
provider-call approval, refuses an active production service/socket, and logs
results privately in `/tmp`. `bash -n` and `--dry-run` pass; active-service
refusal was observed. The real check remains for the operator after separate
approval and can never be inferred from automated fixture success.

Current U5 provenance fixture gate: `nix develop -c env DUNE_BUILD_DIR=_build_u5
dune test packages/pie/test/build_record` passes all 10 cases after archive
names move from v1 TLS transport to consolidated `eta_http`. Complete build,
test, formatter and flake results are recorded below when integration settles.

## Behaviour Changes Accepted During Step 2

- **Interrupt exit.** The Door classifies a stop it requested itself (its own SIGINT/SIGTERM identity) as a clean **exit 0**, and any other interruption as **2**. The v1 `Pie.Process.run` mapped an interrupt-only cause to **130**, so `pie` under SIGINT/SIGTERM now exits 0 instead of 130. The local-diagnostics suite asserts the Door's classification; the mapping itself belongs to the Door / [bin/pie.ml](</home/ribelo/projects/ribelo/pie/packages/pie/bin/pie.ml>) (U4), not to the test. ^pienext-ipne
- **OTLP enablement and the local channel.** [process.ml](</home/ribelo/projects/ribelo/pie/packages/pie/process.ml>) reads only `PIE_OTLP`; `OTEL_SDK_DISABLED` is no longer consulted. The local channel is eta-next's structured JSON journal record (with `Redact.t` fields rendering as `"<redacted>"`), not the v1 logfmt line, and a failing exporter leaves that channel intact while its own failure is reported on stderr. ^pienext-gaq3

