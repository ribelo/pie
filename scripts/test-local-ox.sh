#!/usr/bin/env bash
# Build and run every registered Pie test against local Ox master, without
# installing into or changing the shared opam switch. Fixture roots use writable
# /run/user/$(id -u) (runtime tmpfs), falling back to /tmp, and print the selection.
set -euo pipefail

pie_root="$(cd "$(dirname "$0")/.." && pwd -P)"
ox_root="$(cd "${OX_PATH:-$pie_root/../ocaml/ox}" && pwd -P)"
if [[ "${1:-}" != --in-shell ]]; then
  shell_root="$ox_root"
  if [[ "${1:-}" == --shared-ox ]]; then shell_root="$pie_root"; fi
  exec nix develop "$shell_root" -c bash "$0" --in-shell "$@"
fi
mode="${2:-}"
if [[ "$#" -gt 2 || ( -n "$mode" && "$mode" != --shared-ox ) ]]; then
  echo "Usage: $0 [--shared-ox]" >&2
  exit 2
fi

export OPAMROOT="$HOME/.cache/opam"
eval "$(opam env --switch 5.4.0+ox --set-switch)"
export DUNE_CACHE=disabled
pkg-config --exists openssl sqlite3 || {
  echo 'The Nix shell must provide OpenSSL and SQLite native libraries.' >&2
  exit 1
}
if [[ "$mode" != --shared-ox ]] && {
   [[ "$(git -C "$ox_root" rev-parse HEAD)" != "$(git -C "$ox_root" rev-parse master)" ]] ||
   [[ -n "$(git -C "$ox_root" status --porcelain --untracked-files=no -- lib dune-project)" ]]; }; then
  echo 'Ox must be checked out at its clean local master branch for this test.' >&2
  exit 1
fi

mirror="$pie_root/_dev/ox-source"
prefix="$pie_root/_dev/ox-install"
build="$pie_root/_dev/pie-build"
run="$pie_root/_dev/test-run"
mkdir -p "$pie_root/_dev"
rm -rf "$run"
mkdir -p "$run/logs" "$run/alcotest"
if [[ "$mode" != --shared-ox ]]; then
  rm -rf "$mirror" "$prefix"
  mkdir -p "$mirror"
  # Dune does not traverse directory symlinks. Mirror real directories and
  # symlink each source file; Ox's build products stay in this ignored tree.
  cp -rs "$ox_root/lib" "$mirror/lib"
  ln -s "$ox_root/dune-project" "$mirror/dune-project"
  # ^pcapslint-t4bn: ox_lint installs its rule pages and fixtures beside the launcher.
  mkdir -p "$mirror/docs" "$mirror/test/caps"
  cp -rs "$ox_root/docs/rules" "$mirror/docs/rules"
  ln -s "$ox_root/docs/caps.md" "$mirror/docs/caps.md"
  cp -rs "$ox_root/test/lint" "$mirror/test/lint"
  cp -rs "$ox_root/test/caps/rules" "$mirror/test/caps/rules"

  packages=ox,ox_ai,ox_ai_openrouter,ox_ai_realtime,ox_app,ox_blocking,ox_caps,ox_http,ox_ipc,ox_json,ox_lint,ox_observability,ox_otel,ox_process,ox_rpc,ox_test
  dune build --root "$mirror" --only-packages "$packages" @install
  dune install --root "$mirror" --only-packages "$packages" --prefix "$prefix"
  export OCAMLPATH="$prefix/lib${OCAMLPATH:+:$OCAMLPATH}"
  export CAML_LD_LIBRARY_PATH="$prefix/lib/stublibs${CAML_LD_LIBRARY_PATH:+:$CAML_LD_LIBRARY_PATH}"
  if [[ "$(ocamlfind query ox_http)" != "$prefix/lib/ox_http" ]]; then
    echo 'Local Ox installation did not take precedence over the shared switch.' >&2
    exit 1
  fi
  printf 'Ox master: %s\n' "$(git -C "$ox_root" rev-parse --short HEAD)"
else
  printf 'Ox shared switch: %s\n' "$OPAMSWITCH"
fi

run_gate() {
  local name="$1" ec
  shift
  set +e
  "$@"
  ec=$?
  set -e
  printf '%s rc=%s\n' "$name" "$ec"
  return "$ec"
}
run_gate build dune build --root "$pie_root" --build-dir "$build" --only-packages pie @all
# The wrapper builds @install and production bin @check in _build for typed evidence.
run_gate caps-gate "$pie_root/scripts/caps-gate"
run_gate fmt dune build --root "$pie_root" --build-dir "$build" --only-packages pie @fmt

# Fail closed rather than accidentally using a real desktop/audio/session. Do
# not read or copy the user's auth; fixtures run with an empty private HOME.
# Keep strict wall-clock fixture assertions independent of durable-storage
# latency. Use the user's private runtime tmpfs when available.
private_root="/run/user/$(id -u)"
if [[ ! -d "$private_root" || ! -w "$private_root" ]]; then
  private_root=/tmp
fi
tmp="$(mktemp -d "$private_root/pie-local-ox.XXXXXXXX")"
printf 'Pie private test root: %s\n' "$tmp" >&2
trap 'chmod -R u+rwx -- "${tmp:?}"; rm -rf -- "${tmp:?}"' EXIT
chmod 0700 "$tmp"
export HOME="$tmp/home" XDG_CONFIG_HOME="$tmp/config" XDG_CACHE_HOME="$tmp/cache"
export XDG_DATA_HOME="$tmp/data" XDG_RUNTIME_DIR="$tmp/runtime" TMPDIR="$tmp/tmp"
export CODEX_HOME="$tmp/codex" OTEL_SDK_DISABLED=true
mkdir -p "$HOME" "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" \
  "$XDG_RUNTIME_DIR" "$TMPDIR" "$CODEX_HOME" "$tmp/bin"
for name in wl-copy wl-paste xclip xsel pactl pulseaudio parec curl wget \
  busctl niri wtype ydotool xdotool notify-send; do
  printf '#!/bin/sh\nexit 127\n' > "$tmp/bin/$name"
  chmod 0755 "$tmp/bin/$name"
done
export PATH="$tmp/bin:$PATH"
unset OPENAI_API_KEY OPENROUTER_API_KEY CAMELPIE_OPENROUTER_DEBUG || true
unset DBUS_SESSION_BUS_ADDRESS PULSE_SERVER NIRI_SOCKET WAYLAND_DISPLAY DISPLAY || true

# Discover all Dune (test) stanzas, not only those in the historical census.
# Live helper executables are deliberately not registered as tests.
python3 - "$pie_root/packages/pie/test" > "$run/suites.tsv" <<'PY'
import pathlib, re, sys
root = pathlib.Path(sys.argv[1])
for dune in sorted(root.rglob('dune')):
    for name in re.findall(r'\(test\s+\(name\s+([A-Za-z0-9_]+)\)', dune.read_text()):
        print(f'{dune.parent.relative_to(root)}/{name}.exe\t{name}')
PY
if [[ ! -s "$run/suites.tsv" ]]; then
  echo 'No Dune test executables found.' >&2
  exit 1
fi
status=0
printf 'suite\texit\tpassed\n' > "$run/summary.tsv"
while IFS=$'\t' read -r rel name; do
  exe="$build/default/packages/pie/test/$rel"
  log="$run/logs/$name.log"
  ec=0
  if [[ ! -x "$exe" ]]; then
    printf 'Missing test executable: %s\n' "$exe" > "$log"
    ec=127
  else
    timeout --kill-after=10s 600s "$exe" -o "$run/alcotest" > "$log" 2>&1 || ec=$?
  fi
  count="$(python3 - "$log" <<'PY'
import pathlib, re, sys
text = pathlib.Path(sys.argv[1]).read_text(errors='replace')
hits = re.findall(r'Test Successful in [^\n]*?\. (\d+) tests run\.', text)
print(hits[-1] if hits else 0)
PY
)"
  if [[ "$ec" -eq 0 && "$count" -eq 0 ]]; then ec=1; fi
  printf '%s\t%s\t%s\n' "$name" "$ec" "$count" | tee -a "$run/summary.tsv"
  if [[ "$ec" -ne 0 ]]; then status=1; fi
done < "$run/suites.tsv"
python3 - "$run/summary.tsv" <<'PY'
import csv, sys
with open(sys.argv[1]) as f:
    rows = list(csv.DictReader(f, delimiter='\t'))
print(f'TOTAL: {sum(int(r["passed"]) for r in rows)} passed in {len(rows)} suites; '
      f'{sum(int(r["exit"]) != 0 for r in rows)} failed suites')
PY
exit "$status"
