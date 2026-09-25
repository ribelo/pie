# Report unresolved formatting rules

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Configured focus rules fail closed when required focus data is unavailable.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete (PIE027). Integrated suite pending.

## Session contract (accepted)

Focus-dependent newline or text-entry rules require a compatible Niri session before lookup or emission: Wayland, an `XDG_CURRENT_DESKTOP` colon-token equal to `niri` (case-insensitive), and a nonempty `NIRI_SOCKET`. Any other session (X11, GNOME, empty socket) fails with a content-free `Injection_error` and does not run Niri. Delivery with both rule lists empty never invokes Niri and remains valid on X11 and Wayland. Unconditional rules are not a supported feature.

Focus lookup only selects rules. It does not lock the target window and does not acknowledge delivery of the text.

## Requirement

- If configured delivery rules require unavailable focus information, then PIE shall report a selection error before it emits text. ^pierouting-2xvv

## Evidence

The public injection probe uses fake niri and wtype executables. It tests both a failed lookup and a null focused-window response.

For each case, a newline-only rule returns success and emits collapsed text. An equivalent text-entry rule returns an error without emission.

Text_injection165–179 suppresses lookup failures only when text_entry_rules is empty. The resulting None selects Collapse_to_space at75–77.

## Decision

Apply one failure policy to focus-dependent entry and newline rules. Preserve the default behavior when no rules apply to an identified window.

Keep delivery without rules independent of Niri. Restrict Niri-dependent rules to a compatible session instead of silently using another session’s focus.

Do not add unconditional rules. Config.Stt228–233 and253–258 require an app_id or title_regex predicate. Unconditional public record values do not establish a supported configuration feature.

Keep recognized text available for delivery recovery under PIE008. Do not repeat recognition because rule selection failed.

Focus lookup selects rules. It does not lock the target window or acknowledge receipt of the text.

## Acceptance

Use fake helpers to test lookup failure, null focus, matching rules and known nonmatching rules. Assert no emission on unresolved selection.

Check that delivery without rules performs no focus lookup. Check that incompatible sessions fail before emission. Preserve first-match ordering and explicit newline actions.

## Eta prerequisites

Existing typed failure handling is sufficient. No new desktop framework or fallback backend is needed.
