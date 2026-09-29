# Deliver text without inspecting previous clipboard contents

## Status

Reopened after real Chrome paste failures following deployment. The original 25 ms helper-to-key interval has been restored and redeployed; 61 targeted tests pass, including two new timing/cancellation regressions. This is a compatibility mitigation, not a publication acknowledgement. Live Chrome confirmation remains pending; no historical crash causality is claimed.

Owned clipboard offers with honest best-effort paste-key outcomes; no inspection or restoration of prior contents.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete. Integration pending. BEST-EFFORT limitation: no compositor readiness or receipt; observed copy/key errors fail; no preservation. CLI is Type-only. `result.outcome` is `Typed` or `Clipboard_key_attempted` (not delivered).

## Requirements

- When clipboard text delivery is requested, PIE shall offer the requested text without requiring a representation of the previous clipboard contents. ^piepaste-tgyo
- If copying the requested text fails, then PIE shall report the delivery failure without reporting successful paste. ^piepaste-arw2

## Decision and tradeoff

Treat explicit clipboard delivery as replacement, not a clipboard-preservation transaction. Previous image, text or other contents are not restored automatically. This intentionally removes a convenience policy that blocks the user’s primary workflow. Keep the wtype typing path independent.

Delete the wl-paste lookup, MIME inspection, snapshot model and restore/retry machinery from delivery, together with helpers/tests made unused by that deletion. Do not add image decoding, arbitrary MIME serialization, preservation flags or a silent fallback to another injection method. A valid clipboard offer is not permission to silently discard a genuine copy or key-injection error.

Keep helper ownership bounded. A clipboard offer may outlive the command while it remains the active selection; this is not permission to retain one helper per historical transcript. Use the host clipboard lifetime and one explicit owner where application ownership is required. Do not kill a clipboard owner after an arbitrary250ms and claim the target consumed it. Ctrl-V dispatch is not an application receipt acknowledgement. Final helper selection/readiness/termination must be checked with the actual wl-copy interface during implementation; no new generic subprocess framework.

## Evidence

packages/pie/wayland/clipboard.ml47–58 rejects non-text offers; paste110–136 inspects, snapshots and restores. Text_injection.inject delegates only its Wayland Paste branch to this path. Existing test text_injection/4 explicitly expects no text MIME for image-only offers.

The actual workspace-library control `.scratch/research/core-audit/check-clipboard-image.sh` passes on OxCaml5.4.0+ox: image_only=Clipboard_error; copy_calls=0; paste_calls=0. It uses synthetic wl-paste/wl-copy/wtype only, no desktop interaction. This reproduces current unwanted behavior, not a fix.

Historical (rejected): a 50 ms helper-alive window was treated as compositor readiness. Installed wl-copy 2.3.0 has no copy-side readiness or selection-loss stdout protocol. Stdin EOF and helper liveness are not compositor proof; Ctrl-V is best-effort dispatch, not a receipt. Public-path regressions live in `text_injection` paste-ignore/failed-offer cases and `test/clipboard_owner`.

## Acceptance

Promote a regression through the public paste/injection path: start with image-only, text, empty and arbitrary MIME offers; all admit the same requested text without calling wl-paste. Assert one copy and one intended key dispatch when applicable. Inject copy failure, helper early exit, key-dispatch failure, cancellation and slow consumers; distinguish request submission from confirmed consumption. Repeated operations must leave bounded owned processes/descriptors after settlement. Remove obsolete preservation assertions and register the new behavior with named tests.

## Ox prerequisites

No image-format capability is needed from Ox. Use the settled deferred-execution and cancellation/finalizer contracts for helper ownership; validate against the actual deployed Ox version. Incoming fixes do not replace the PIE consumer migration. Do not wait for a new clipboard abstraction in Ox.
