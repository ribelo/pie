---
status: needs-info
package: pie
---

# WSS normal-operation flake: shared Ox TLS wire buffer

2026-09-30. Investigation ID: session-ownership-wss-flake. Related acceptance: PIE002, piesession-ml14, piesession-mp2y, piesession-y37k.

## Finding

**Ox transport bug, not a fixture-only race or a demonstrated Pie lifetime bug.** Concurrent TLS receive/send share the same mutable ciphertext buffer. The reported normal close is a secondary error that hides a TLS authentication failure. No production fix or fixture workaround was applied.

The fixture does send `response.done` before reading client audio or `response.create`. That ordering makes receive/send overlap timing unusual, but it cannot legitimately corrupt authenticated TLS records. Delaying the response would hide this transport defect rather than establish that the fixture caused the close.

## Evidence

Built and ran only through the prescribed development wrapper, with the absolute private build directory:

```sh
scripts/dev-env.sh dune build --root "$PWD" --build-dir "$PWD/_dev/b-sess" \
  --only-packages pie packages/pie/test/session_ownership/test_session_ownership.exe
scripts/dev-env.sh --private-home \
  "$PWD/_dev/b-sess/default/packages/pie/test/session_ownership/test_session_ownership.exe"
```

1. **Original public entrypoints:** build exited 0; first full run exited 1, three tests passed and WSS repeat failed: `streaming normal: normal-op: Codex realtime closed (1000)` (Alcotest run UBOCOEEH).
2. **Error-boundary probe:** temporary test-local copies of the current Pie realtime implementation and installed Ox Session/Ws_client preserved the same control flow. Logging only before `Ws_client.receive` shuts down exposed the underlying error (6ZHJWZ4M):

   ```text
   WS TLS failure before shutdown: OpenSSL error 1: error:0A000119:SSL routines::decryption failed or bad record mac
   ASSERT streaming normal: normal-op: Codex realtime closed (1000)
   ```

3. **Overlap probe:** a temporary test-local copy of Ox Tls_flow marked the interval around `Eio.Flow.single_read t.raw t.wire`. Its `drain` logged when it wrote that same buffer while the read was pending. Run TT62WV4C observed repeated overlaps, then the same bad-record-MAC error and normal-operation failure. Only the client used the copied TLS implementation; the fixture server still used installed Ox.
4. **Single-variable counterfactual:** in that client-only TLS copy, remove overlap logging and give each `drain` invocation its own ciphertext buffer, leaving fixture timing unchanged. **20/20 complete executable runs passed, all four tests each** (including 18 rounds × 12 WSS operations per run). This is diagnostic evidence, **not** a fixed production acceptance result.
5. **Reverse control:** restore the original shared buffer in the same test-local copy, retaining only error logging. Two full runs passed; the third failed with the same bad-record-MAC immediately before `buffered normal: normal-op: Codex realtime closed (1000)`. Thus both buffered and streaming operations are affected.

Successful-send/receive logging itself changed timing enough to produce ten consecutive passing WSS-only runs. Those passes were not treated as a fix. Failure-only instrumentation reproduced the fault again.

All temporary copied modules and caller substitutions were removed. The original fixture has no remaining diff. No Ox checkout, installed library, production Pie source, service, user configuration, or Git state was changed by this investigation. No packages were installed. The final original-source build succeeded. `scripts/dev-env.sh scripts/caps-gate` and `git diff --check` both exited 0.

## Mechanism

- [Tls_flow](</home/ribelo/projects/ribelo/ocaml/ox/lib/http/tls_flow.ml#L7-L15>) owns one `wire` buffer. [feed](</home/ribelo/projects/ribelo/ocaml/ox/lib/http/tls_flow.ml#L52-L61>) passes it into a potentially suspended raw read; [drain](</home/ribelo/projects/ribelo/ocaml/ox/lib/http/tls_flow.ml#L23-L28>) fills it with outgoing ciphertext and passes a slice to a potentially suspended raw write. Read and write fibers can therefore overwrite bytes still owned by the other operation.
- Pie legitimately runs receive and send concurrently through `Ox.Concurrent.par` in [buffered realtime](</home/ribelo/projects/ribelo/pie/packages/pie/stt/codex_realtime.ml#L757-L794>) and [streaming realtime](</home/ribelo/projects/ribelo/pie/packages/pie/stt/codex_realtime.ml#L850-L888>). The client TLS corruption occurs at this full-duplex boundary.
- On TLS failure, [Ws_client.receive](</home/ribelo/projects/ribelo/ocaml/ox/lib/http/ws_client.ml#L426-L430>) first sets `closed <- true`, then calls shutdown, and only afterwards returns the original error. TLS shutdown can yield while draining its alert.
- The concurrent sender then sees `closed` and [returns](</home/ribelo/projects/ribelo/ocaml/ox/lib/http/ws_client.ml#L280-L286>) `Closed (1000, "closed")`. The fail-fast parallel combinator can propagate that secondary result and cancel the receiver before it returns the original TLS error. No server WebSocket close frame is needed to produce this message.

The installed and checkout copies matched byte-for-byte during investigation:

| Source | SHA-256 |
| --- | --- |
| [Tls_flow](</home/ribelo/projects/ribelo/pie/_dev/ox-install/lib/ox_http/tls_flow.ml>) | `ae5f782825acc2286579eef4a1d0e6a15fea72872fc5f1574be20e7c29e42b68` |
| [Ws_client](</home/ribelo/projects/ribelo/pie/_dev/ox-install/lib/ox_http/ws_client.ml>) | `4247b0d6c37d59a47a161526e59bbb16161511cc02e0cbe03e614f2b272355c6` |

## Proposed upstream repair and remaining work

Give in-flight reads and writes disjoint buffers whose lifetimes extend through I/O completion. The diagnostic counterfactual was exactly this local change inside `drain` (all other `wire` uses remained unchanged):

```ocaml
let rec drain t =
  if Ssl.bio_write_pending t.ssl > 0 then (
    let wire = Cstruct.create 16384 in
    let n =
      Ssl.bio_read t.ssl (Cstruct.to_bigarray wire) 0 (Cstruct.length wire)
    in
    if n <= 0 then raise (Tls_failure "OpenSSL BIO drain failed");
    Eio.Flow.write t.raw [ Cstruct.sub wire 0 n ];
    drain t)
```

This is a causal probe, not a complete reviewed TLS concurrency design. Upstream should also audit simultaneous drain calls, write serialization, WANT_READ handling, SSL state access and cancellation. Do not hold a single mutex across a blocking receive: that would prevent the send needed to make progress. Retain the original terminal transport error so later sends do not report a misleading normal close.

Add an upstream deterministic full-duplex test with a parked read and a yielding write; verify ciphertext buffers do not alias. Then rerun this unchanged Pie acceptance executable at least 20 times against a rebuilt, identified Ox artifact. Only afterwards consider making the normal fixture protocol-realistic as separate test maintenance.

Status remains `needs-info` pending upstream repair/review and acceptance against the actual rebuilt dependency. The 20/20 counterfactual does not prove historical service crashes or establish a deployed fix.
