# Keep local diagnostics independent of export

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Local stderr diagnostics remain independent of export; the process logger contract is verified against reviewed Ox.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete (verify-first: no Process.ml rewrite). Integrated suite pending. Installed d06548b runtime was not changed. PIE029.

Reviewed Ox archive `.scratch/pie-audit-current-build` SHA256: ox.cmxa `8db848dfd6280b4fe5848d4879f7cc9f4a5f3d803845ba3569cf41f6102706df`; ox_http_eio.cmxa `1c48ab9b1cb428bcb25a91253f42a0567e59c6c1b730e7e8f2796e4a4926c0b2`; ox_stream_eio.cmxa `709ff20b2fb8c4fae832072c80483da243495f9a8fb6303b383c0526af751cfd`; ox_http_tls_openssl.cmxa `7e33b7de22ba08113449185241bc78e9a5d04cbf5f1c3f6698cb21cf45435dac`; dlleta_http_tls_openssl_stubs.so `52ddb67dbb0c56b973ba35e75579b5c6b38a46a169a01817f6afcb0ca6fd4d03`.

Original registered process_door 5/5 including process_door/3 logfmt on stderr. Focused local_diagnostics 6/6: disabled export stderr once and private collector 0 contacts; filtering retained; enabled export local-once with private collector metadata; exits 0/1/130.

## Requirement

- When OTEL export is disabled, PIE shall retain enabled local operational diagnostics on stderr. ^pielog-1nc0

## Evidence

The screened registered suite ran121 tests against installed Ox d06548b. One test failed: process_door/3, logfmt on stderr.

A focused Process.run probe emitted a synthetic operational message with OTEL_SDK_DISABLED=true. It returned code0 with zero stderr bytes.

Process39–44 uses a log interceptor for the stderr tee. Installed Ox_otel_eio.with_otlp86–90 creates the disabled runtime without a logger capability.

This result does not establish the behavior of incoming Ox changes. It does not show that all PIE diagnostics disappear. Process.emit_sync writes directly to stderr.

## Implementation direction

Own the local diagnostic sink independently of optional export. Use an explicit logger or a verified interceptor contract from the selected Ox artifact.

Do not enable network export to obtain local logs. Avoid duplicate local messages when export is enabled. Preserve the metadata-only policy under PIE019.

## Acceptance

Run the existing process_door tests with export disabled. Check that a synthetic enabled diagnostic reaches stderr once and no exporter sends data.

Test the enabled-export case only with a private collector. Preserve local log filtering and the normal success, failure and interrupt exit codes.

## Ox prerequisites

Repeat the consumer test against the intended incoming artifact. If that artifact already restores the required behavior, retain the test without adding redundant PIE logging code.
