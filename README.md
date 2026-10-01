# Pie

OCaml/OxCaml voice assistant. The Dune/opam/flake package name is `pie`.

## Toolchain (checked)

- Compiler: OCaml/OxCaml `5.4.0+ox`
- Dune: `3.22.2`

There is no `.#oxcaml` development shell and no `camelpie` package.

## Mutable development shell vs locked package

`nix develop` is the flake **default** shell. It loads the **mutable shared**
opam switch `5.4.0+ox` under `$HOME/.cache/opam`. It does **not** install the
flake’s locked Ox source. Do not treat this shell as a hermetic package build.

The locked flake package is `pie`:

```bash
nix build --impure .#pie
```

That package uses flake inputs. It is a different artifact path from the
shared switch.

Reviewed Ox stages used in audit builds are passed as a directory parameter
(`OX_STAGE`). A historical archive hash in old notes is not a guarantee of
the current linker input.

## Build (does not install)

```bash
eval "$(opam env --switch=5.4.0+ox --set-switch)"
export OCAMLPATH="$OX_STAGE${OCAMLPATH:+:$OCAMLPATH}"
export CAML_LD_LIBRARY_PATH="$OX_STAGE/stublibs${CAML_LD_LIBRARY_PATH:+:$CAML_LD_LIBRARY_PATH}"
export DUNE_CACHE=disabled
dune build --build-dir .scratch/example-build/dune --only-packages pie
```

This does not install or restart anything.

## Glossary

Pie reads one global glossary for every application. Its default is
`~/.config/pie/glossary.md`, or `$XDG_CONFIG_HOME/pie/glossary.md` when that
configuration directory is set. `PIE_GLOSSARY` names a replacement file.
Symlinks are followed, the whole file is read at each key press, and missing or
unreadable files warn without stopping dictation. There is no project lookup
or size cap. Project terms go in sections of this same file.

```bash
pie glossary             # path, existence, physical line count
pie glossary --help      # format and a coding-agent maintenance recipe
```

Use one line per term, with comma-separated mishearing variants and `##`
section headings:

```markdown
## OCaml
- Eio — ejo, e i o
- Dune — dun, diun
```

Ask a coding agent to collect a project's names, identifiers, tools and
commands, plus how recognition mishears each. Have it extend the global
file, keep existing sections, and merge duplicate terms' variants on one
line. Edits apply at the next key press; do not restart Pie. The bundled
skill and `pie skill install` have been removed.

## Test against current local Ox master

```bash
scripts/test-local-ox.sh
```

The script enters `nix develop ../ocaml/ox` for native libraries, mirrors the
clean checked-out Ox `master` into ignored `_dev/ox-source`, builds and
installs only the needed Ox packages into ignored `_dev/ox-install`, then
puts that local prefix ahead of the shared switch via `OCAMLPATH` and
`CAML_LD_LIBRARY_PATH`. It builds Pie and runs **every registered test**
(including long end-to-end cases) with a private HOME and fail-closed desktop
helpers. It never installs into the shared opam switch. Inspect
`_dev/test-run/summary.tsv` for per-suite pass counts and
`_dev/test-run/logs/` for test diagnostics. Set `OX_PATH` to use a different
local Ox checkout. The script refuses a dirty or non-master Ox checkout.

## Build identity

Dune embeds a short Git commit, with a `-dirty` suffix when tracked or untracked
source changes exist, and the actual UTC build time. Outside a checkout (including
source archives and Nix sources without `.git`), the commit is `unknown`; builds still
work. `SOURCE_DATE_EPOCH` is intentionally ignored: the timestamp describes this
build, not a reproducible epoch.

`pie status` shows the **running daemon** build. JSON uses `buildCommit` and
`buildTime`; a differing client appears separately as `clientBuildCommit` and
`clientBuildTime`. An older daemon has no build identity. The startup config JSON
includes `build_commit` and `build_time`. `pie --version` shows the client build
without loading configuration or contacting the daemon.

Ignored captures are listed as `ignored`, with held duration, total capture duration,
peak and RMS. JSON includes optional `ignoredCapture` measurements; older history
lines remain readable. Ignored holds longer than 3000 ms notify once after release;
quiet-input warnings, accidental taps and successful correction fallbacks only log.

## Provenance record

Compile the helper **first**, in its own build directory, then invoke that
binary **directly**. Do not `dune exec` the helper against the same
`--build-dir` as `pie.exe`: that rewrites Dune’s log and is not the
`pie.exe` link line.

The helper then:

1. Snapshots `packages/pie` plus `scripts/build-identity.sh`, `dune-project` / `dune-workspace` /
   package dune files (relative-path manifest) and Ox `*.cmxa` plus the
   TLS stub.
2. Runs `DUNE_CACHE=disabled dune build --build-dir <fresh> packages/pie/bin/pie.exe`.
3. Parses **only** the `ocamlopt -o pie.exe` line (rejects unsupported
   quoting rather than guessing).
4. Re-snapshots source, linked archives, and TLS stub; refuses the record if
   any changed.
5. Atomically writes JSON beside the artifact (exclusive 0600 candidate,
   rename), including the embedded commit/time in `binary.build_identity`. Never installs.

Expected TLS stublibs are recorded as **runtime** paths, not as DLLs embedded
in the native archive.

```bash
# 1) bootstrap helper (separate build dir)
dune build --build-dir .scratch/provenance-helper/dune \
  packages/pie/test/build_record/pie_build_record.exe

# 2) invoke the helper binary directly (fresh pie.exe build dir)
./.scratch/provenance-helper/dune/default/packages/pie/test/build_record/pie_build_record.exe \
  --root "$PWD" \
  --build-dir .scratch/pie-record-build/dune \
  --output .scratch/pie-record-build/pie.provenance.json
```

## Test

Screened provenance fixtures:

```bash
dune exec --build-dir .scratch/provenance-build/dune -- \
  packages/pie/test/build_record/test_build_record.exe
```

A green helper test is not full-suite green and not deployment green.

## Prepared live PTT check (operator only)

`scripts/live-ptt-check-eta-next --dry-run` prints the complete operator
sequence without building, touching the microphone/keyboard or calling a provider.
There is no automated live PTT gate in the regular suites. After the lead
authorizes a real provider call, the operator stops `pie.service` themselves,
confirms it is inactive, then runs
`scripts/live-ptt-check-eta-next --run --provider-calls-approved`. The script
refuses an active service/socket, builds Pie into a private `/tmp` prefix,
copies the configuration into a private directory and uses a separate control
socket. The operator holds/releases their PTT key, observes one transcription,
ends the check, then restarts and verifies `pie.service` themselves even if the
check fails. Results and a restricted diagnostic log remain in `/tmp` for
inspection. Never run this against the active production service.

## Authentication

Pie accepts `OPENAI_API_KEY` and reads shared `CODEX_HOME/auth.json`
**read-only**. It never refreshes, rewrites, or copies refresh tokens. A
known-expired Codex access token requires the credential owner to run
`codex login`. No cooperative owner protocol is verified.

## Install and activation

Build, test, install, and service activation are separate. Install and
activation need explicit authorization. This audit does not run them.

The shared-switch helper `pie-opam-install` pins committed `master` and
does not deploy uncommitted audit work.

The running `pie` command is the shared-switch binary
(`$HOME/.cache/opam/5.4.0+ox/bin/pie`), not a repository copy.
