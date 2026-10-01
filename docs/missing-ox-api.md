# Missing Ox APIs used by Pie

Ox row 57 (e7054a73) and row 59 (ffe57e78, spec ox/docs/issues/pie-api-gaps/01-pie-api-gaps.md) closed most of the gaps recorded here on 2026-09-30. Pie is locked at ox ffe57e78.

## Adopted

- Responses audio format and request size: `Stt.Correction` encodes through `Ox_ai_openrouter.Responses.encode ~audio_format:Mp3 ~instructions` and `Responses.encode_text`, and passes `?max_request_bytes` sized to the capture budget. The local request encoder is gone. (`dict-dq3o`)
- Reasoning output items: `Responses.deliverable` skips `Other "reasoning"`; Pie's local filter is gone. (`dict-ldo9`)
- WebSocket establishment: `Ox_http.Ws_client.connect ?establishment_timeout` (10 s) was already in use; there is no total-effect wrapper left. (`etagap-utb3`, `etagap-g1ip`)
- Transport defects: WebSocket keepalive pings are answered for the life of the connection (long streamed dictations no longer die at 11 min 6 s, docs/issues/long-recording-cutoff) and TLS reads and writes no longer share a ciphertext buffer (docs/issues/session-ownership-wss-flake; session_ownership passes).

## Still missing

### Protected native-operation scope and latest-state wakeup

Pie uses `Ox_blocking` for native filesystem callbacks. A started callback must
finish physically before its caller releases a writer or removes a draft.
`Fs.blocking` therefore uses a cancellation-protected child Eio switch and an
explicit `Pool.Drain` shutdown. Ox exposes the pool and supervised scopes, but
not a direct-style protected child-scope combinator; `Eio.Cancel.protect` and
`Eio.Switch.run` remain the small lifecycle bridge.

Recording-state projection persistence needs one in-flight write and one pending
latest value, with non-yielding replacement, a wakeup, and a drain at switch exit.
Ox actors and channels offer bounded FIFO delivery, not latest-value replacement.
`Recording.State_writer` uses `Eio.Condition`, `Eio.Fiber.fork_daemon` on its
borrowed service switch, and a protected `Eio.Switch.on_release` drain.
`Ox.Scope.fork_daemon` exists for an Ox supervised scope; it does not attach a
daemon directly to this existing Eio switch. A latest-value mailbox or
switch-owned coalescing worker would remove this bridge without changing
retained-audio, archive or history durability.

### OpenRouter provider routing

`Responses.request` and `Responses.text_request` have no field for OpenRouter's `provider` object (`{"order": [...], "allow_fallbacks": ...}`). Pie routes one-pass and pass-2 calls to Google AI Studio first (`openrouter_provider_order`, dict-4y4r), so `Stt.Correction` still adds `provider` to the JSON Ox encodes and posts it with `Ox_ai.Transport.post_json` instead of calling `Responses.create` / `create_text`. An optional `?provider:` (order, allow fallbacks) on `encode`, `encode_text`, `create` and `create_text` would let Pie call `create` / `create_text` directly.

### Response idle timeout: available, not adopted

`?response_idle` exists on Responses and `Audio.create`, but Pie's calls are non-streaming: the provider sends nothing until the whole reply is ready, so an idle bound would have to be as long as the total one. Pie keeps its total deadlines (60 s direct transcription, `one_pass_timeout_seconds`, `correction_timeout_seconds`). (`etagap-jk25`, `etagap-3uxz`)
