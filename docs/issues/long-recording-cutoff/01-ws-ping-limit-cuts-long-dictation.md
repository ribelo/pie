---
status: needs-info
package: pie
consumer: ox
---

# Long dictations are cut off after 11 min 6 s

## Problem Statement

On 2026-09-30 two push-to-talk dictations on the legacy gpt-realtime-2.1 path (started 11:12:32 and 11:24:54) failed after exactly 11 min 6 s each with `PTT capture failed: Codex realtime stream closed`. The recovery draft was aborted, so both recordings were lost.

## Evidence

- Pie's journal: capture started 11:12:32, failed 11:23:38; started 11:24:54, failed 11:36:00. Same duration, 666 s.
- A 13-minute real-time-paced stream to the same model with the same Codex login, sent from Node (which answers pings itself), completed normally and returned a transcript (docs/research/2026-09-30-dictation-models/scripts, `long.mjs` in /tmp at the time; 37 MB of 24 kHz PCM, close code 1005 after `response.done`). The server has no 11-minute limit.
- `Ox_http.Ws_client.receive` (ox `lib/http/ws_client.ml`, locked 6e113c8f, unchanged on master 76d2596d) counts control frames inside one `receive` call and fails with `Protocol "too many consecutive pings"` at 32. During a recording Pie's receiver waits in one `receive` with no data frames, and the server pings about every 20.8 s: 32 × 20.8 s ≈ 666 s. The inference that the server's ping interval is about 20.8 s comes from the matching arithmetic, not from a packet capture.

## Requested Ox change

`receive` should answer pings for as long as the connection lives, and bound only a flood (for example pings per second, or consecutive control frames without elapsed time), not the number of pings during a quiet period. Until then every Codex realtime session longer than about 11 minutes fails.

## Pie mitigation

The two-pass spec (docs/issues/two-pass-dictation, dict-oj7u and dict-u5xj) keeps capturing locally when the stream fails, transcribes the whole recording buffered after release, and retains the audio on any failure, so the cut-off no longer loses speech. The Ox fix is still needed for the streaming path to survive long recordings.
