# Repository Guidelines

## Project Structure & Module Organization
Runtime logic lives under `src/` with `main.rs` coordinating modules such as `ai_router.rs`, `command_executor.rs`, `daemon.rs`, `notification.rs`, and `wake_word.rs`. High-level requirements are tracked in the root `SPEC`, while design notes sit in `docs/architecture.md` and voice/test scenarios live in `features/*.feature`. Integration tests are centralized in `tests/integration_tests.rs`, and sample WAV assets in the repo root help exercise audio workflows.

## Build, Test, and Development Commands
All agents must verify builds locally before reporting success. For Rust code, run `nix develop -c cargo check` (with feature flags as required) after substantial changes and prior to marking a task complete. When touching keyboard, audio capture, or other feature-gated paths, also run `nix develop -c cargo check --features=wayland,audio` before claiming the work is finished.

Activate the reproducible toolchain with `nix develop` (or `nix-shell`) before building. Use `cargo build` for iterative builds and `cargo build --release` when validating optimized binaries. Run `cargo run -- list` to enumerate audio devices, `cargo run -- record --system-only` during manual checks, and `cargo run -- status` to inspect daemon capabilities and runtime state. Keep quality gates green with `cargo fmt`, `cargo clippy -- -D warnings`, and `nix develop -c cargo test`. Update the root `SPEC` and `TODO` files as part of planning and review.
Leverage the new `-v/--verbose` flag (repeat up to `-vv`) on any `pie` subcommand when you need richer tracing; the server logs every control command, capture, STT call, and debug-audio copy.

For wake word testing without Picovoice libraries, build without the `porcupine` feature (default). For production with wake word support, use `--features porcupine,audio` and set `PICOVOICE_ACCESS_KEY` environment variable. Model and keyword paths can be configured via CLI flags (`--wake-model`, `--wake-keyword`) or environment variables (`PICOVOICE_MODEL_PATH`, `PICOVOICE_KEYWORD_PATH`), with CLI taking precedence.

The `pie status` command provides runtime diagnostics showing whether audio and wake word features are enabled, current daemon state, active configuration, and any active recordings. In stub mode (default build), it shows "Audio: Disabled" and "Wake Word: Stub mode (no detection)". With `--features audio,porcupine`, it shows "Audio: Enabled" and "Wake Word: Porcupine (real detection)" along with the active model/keyword paths and sensitivity setting.

When real wake word detection is enabled, PIE continuously processes audio frames for low-latency detection (<200ms) of the "Ok PIE" wake word, transitioning immediately to command capture upon detection. After wake word detection, PIE captures microphone audio for a configurable duration (default 3 seconds) and passes a transcription placeholder to the AI router for command processing.

## Coding Style & Naming Conventions
Follow standard Rust formatting (4-space indentation, trailing commas, module-level `snake_case`) and rely on `cargo fmt` to normalize layout. Prefer `CamelCase` for types, `snake_case` for functions and files, and `SCREAMING_SNAKE_CASE` for constants. Favor small modules aligned with the existing file-per-component pattern and keep user-facing strings localized inside command modules.

## Testing Guidelines
Write integration coverage alongside feature work in `tests/integration_tests.rs`, mirroring scenario titles from `features/`. Each new capability should include a Gherkin scenario plus the Rust harness that exercises it. Run `nix develop -c cargo test` locally before pushing, and revise `SPEC`/`TODO` so requirements and backlog stay synchronized. When adding audio fixtures, store them under a descriptive filename in the repo root and note their purpose in the related spec.
For keyboard output, set `PIE_KEYBOARD_CAPTURE_PATH` during tests to route typed text into a capture file; the daemon automatically switches backends when this variable is present, enabling deterministic assertions without a Wayland compositor.
Pipeline audio captures are mirrored into `/tmp/pie_pipeline_audio/` for debugging (`PIE_DEBUG_AUDIO_DIR` overrides the location, set it to an empty string to disable retention).

## Commit & Pull Request Guidelines
Use imperative, scope-aware commit subjects (e.g., `Add wake word debounce`) and group related spec, feature, and code edits together. Reference updated specs or features in the body, highlighting any new commands or flags. Pull requests should summarize functional impact, link to relevant specs or issues, and include manual verification notes (command output, audio samples, screenshots) when behavior changes. Confirm formatting (`cargo fmt`), linting (`cargo clippy -- -D warnings`), tests (`nix develop -c cargo test`), and SPEC/TODO updates ran before requesting review.
