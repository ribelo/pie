# Bound and identify native keyboard messages

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Binary D-Bus decoding is bounded and keyboard peers are pinned.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Integrated suite pending. No deployment. Does not claim the whole audit. PIE023. Not a general D-Bus or live-bus security/compatibility claim.

Supported application adapter: `unix:path` vs `unix:abstract` (leading NUL sockaddr, never a filesystem disguise); `guid` metadata; first address only, no AUTH fallback. AUTH line is exactly `OK ` plus 32 hex GUID (4KiB line cap). Binary messages: 64KiB header+body checked before extra allocation; little-endian only; header field code/signature/duplicates enforced; body SIGNATURE retained. KeyEvent requires sender=pinned unique owner, path/interface/member, and `Some "buuuq"` with 18-byte canonical boolean layout. NameOwnerChanged requires bus sender/path/interface/member and `Some "sss"`. GetNameOwner replies require `Some "s"`. Watch ACK is empty body and absent/empty signature. 10s acquire through Watch ACK (`Setup_timeout`, not Interrupt). Identity uses `GetNameOwner` + `NameOwnerChanged` (D-Bus spec well-known vs unique names; header SENDER=7); WatchKeyboard destination is the pinned unique a11y owner. Focused gates: keyboard_bus_boundary 32 green (`HI3D7HT3`) including private dbus-daemon Hello/GetNameOwner/Watch+KeyEvent; ownership 14 retained. main `test_pie` D-Bus KeyEvent fixture migrated to the expanded record/`parse_key_event ~owner`.

## Requirements

- When PIE resolves its keyboard bus address, PIE shall separate address options from the endpoint and preserve the distinction between filesystem and abstract Unix addresses. ^piebus-phtn
- Before allocating or waiting on keyboard setup/message input, PIE shall enforce the supported framing and setup limits defined below and report unsupported or malformed input explicitly. ^piebus-fkq5
- When PIE accepts a keyboard event, PIE shall verify the expected keyboard-service sender identity rather than accept interface/member names alone. ^piebus-1oli

## Evidence

Actual session_bus_path helper with synthetic environment values: unix:path=/synthetic/bus works; adding ,guid=synthetic includes that suffix in the returned filename; unix:abstract=synthetic returns the ordinary string synthetic without preserving endpoint kind. No socket was opened by the address control.

Monitor297–310 reads peer-controlled32-bit body/field lengths, computes aligned size and allocates that amount without an application maximum. read_crlf_line146–159 has no bound. Authentication/method setup has no finite timeout. No large-allocation or stalled-handshake attack was executed.

Parsed message fields241–248 omit sender; AddMatch360–366 selects only signal/interface/member; parse_key_event312–324 uses those same fields. This establishes missing application identity validation, not a demonstrated bypass of the live bus’s policies.

## Narrow supported contract

Keep this an application keyboard adapter. Support the Unix address forms actually declared; reject unsupported transports/options explicitly instead of inventing a filename. Parse escaped address values after separating structural delimiters. Do not silently try another endpoint after authentication failure.

Proposed limits:4KiB auth line,64KiB total binary message including aligned header/body,10 seconds total acquisition through WatchKeyboard acknowledgement. Check sums/alignment and lengths before allocation. Established event watching has no total lifetime deadline. These are conservative application limits, not a claim of full D-Bus compatibility.

Validate the supported header/value encodings and body layout. Unsupported endianness or variant signatures must yield explicit unsupported-input failure rather than incorrect skipping or apparent EOF. Follow PIE021 for physical cleanup and service input-loss outcomes.

Use the bus-assigned sender identity for the expected service, and treat owner loss/change as a capability transition. Interface strings supplied by an arbitrary peer are not identity. Verify the selected match/owner mechanism against the actual bus API during implementation; no cryptographic or live-policy assurance is claimed here.

## Acceptance

Synthetic address grammar cases; escaped delimiter values; separate filesystem/abstract endpoints; overflow/oversize/incomplete frames; truncated strings/terminators; unsupported signatures/byte order; setup timeout; unrelated sender; expected sender; service-owner change. Include positive real-encoding fixtures and local isolated bus tests where available. Never fuzz against the desktop bus.

## Ox prerequisites

Existing timeout, scoped cleanup and error contracts suffice. No new Ox D-Bus package is required by this decision. A maintained replacement client may be evaluated only with actual toolchain/integration evidence, not assumed availability.
