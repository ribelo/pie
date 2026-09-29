---
kind: requirement
---
# PulseAudio client

## Intent

Provide scoped, concurrent PulseAudio command and recording access through Ox effects without leaking Eio lifecycle or timing primitives above the socket transport boundary. The capability covers one connected client scope and its command dispatcher, reader, and record streams; it does not provide reconnection.

## Requirements

- The PulseAudio client shall expose structured typed failures that distinguish protocol, authentication, server, timeout, disconnection, and malformed-packet errors. ^pulse-l1qs
- The PulseAudio client interface shall expose client operations as `Ox.Effect.t` values. ^pulse-86o0
- While a PulseAudio command awaits its response, the PulseAudio client shall represent response completion with an Ox asynchronous effect. ^pulse-gu09
- While multiple PulseAudio commands are in flight, the PulseAudio client shall correlate each response with its request tag. ^pulse-b7kt
- While one or more PulseAudio commands are pending, when a caller submits another command, the PulseAudio client shall dispatch the new command concurrently with the pending commands. ^pulse-6qr7
- While a PulseAudio client scope is active, the PulseAudio protocol reader shall remain scoped to that client scope. ^pulse-d36y
- While a PulseAudio client scope is active, the PulseAudio client shall execute the protocol reader as a scoped Ox background effect. ^pulse-w447
- While a PulseAudio client scope is active, the PulseAudio socket shall remain scoped to that client scope. ^pulse-kryi
- When a PulseAudio client scope ends or is interrupted, the PulseAudio client shall stop its protocol reader. ^pulse-zj05
- When a PulseAudio client scope ends or is interrupted, the PulseAudio client shall close its socket. ^pulse-brjg
- If the PulseAudio protocol reader terminates with a structured typed failure, then the PulseAudio client shall fail every pending command with that failure. ^pulse-if62
- If the PulseAudio protocol reader terminates with a structured typed failure, then the PulseAudio client shall close every active record stream with that failure. ^pulse-bsa1
- If the PulseAudio protocol reader terminates with a structured typed failure, then the PulseAudio client shall fail the client scope with that failure. ^pulse-rla0
- If the PulseAudio protocol reader terminates with a structured typed failure, then the PulseAudio client shall cancel remaining work in the client scope. ^pulse-blrc
- If PulseAudio rejects `SET_CLIENT_NAME`, then the PulseAudio client shall fail client acquisition with a typed server error. ^pulse-vm4c
- If PulseAudio supplies a malformed command-error payload, then the PulseAudio client shall report a typed protocol error. ^pulse-33fc
- When the PulseAudio client opens a record stream, the PulseAudio client shall expose its audio chunks as an `Ox_stream.Stream.t`. ^pulse-bc14
- While a PulseAudio record stream is active, the PulseAudio client shall limit its buffered audio to 64 chunks. ^pulse-ngc5
- While a PulseAudio record-stream buffer contains 64 chunks, the PulseAudio client shall apply producer backpressure. ^pulse-9bc5
- If a PulseAudio record stream closes with an error, then the record stream shall emit its buffered audio chunks before reporting the typed failure. ^pulse-7ny7
- The PulseAudio client shall accept a configurable request timeout as an `Ox.Duration.t`. ^pulse-2ncz
- When a caller omits the PulseAudio request timeout, the PulseAudio client shall use a duration of three seconds. ^pulse-9v2r
- If a PulseAudio command exceeds its request timeout, then the PulseAudio client shall fail the command through the typed error channel. ^pulse-h303
- If a PulseAudio command exceeds its request timeout, then the PulseAudio client shall remove the command's pending request registration. ^pulse-wipy
- If a PulseAudio reply arrives after its request timed out or its client scope ended, then the PulseAudio client shall drop the reply. ^pulse-ow5o
- If a PulseAudio reply arrives after its request timed out or its client scope ended, then the PulseAudio client shall emit a debug-level diagnostic. ^pulse-xqrb
- If `DELETE_RECORD_STREAM` fails during record-stream cleanup, then the PulseAudio client shall log the typed failure. ^pulse-tg9e
- If `DELETE_RECORD_STREAM` fails during record-stream cleanup, then the PulseAudio client shall preserve the result of the record-stream body. ^pulse-4kja
- The PulseAudio client interface shall expose the Eio network capability as its sole public Eio value. ^pulse-qoto
- While PulseAudio commands are pending, when their client scope ends, the PulseAudio client shall fail those commands with a typed disconnection error. ^pulse-o36o

## Verification seams

- Extend the existing Unix-socket mock server seam to exercise authentication, `SET_CLIENT_NAME`, concurrent tagged replies, malformed packets, timeouts, late replies, reader failure, and scope release.
- Exercise record delivery through the public `Ox_stream.Stream.t` seam to verify capacity, backpressure, clean completion, buffered draining, and typed failure.
- Compile all command-layer callers against the public PulseAudio client interface and inspect the exported interface to verify that the Eio network capability is its sole public Eio value.
