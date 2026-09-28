#!/usr/bin/env bash
# Build and run every registered Pie test against local Eta master, without
# installing into or changing the shared opam switch.
set -euo pipefail

pie_root="$(cd "$(dirname "$0")/.." && pwd -P)"
eta_root="$(cd "${ETA_PATH:-$pie_root/../ocaml/Eta}" && pwd -P)"
if [[ "${1:-}" != --in-shell ]]; then
  exec nix develop "$eta_root" -c bash "$0" --in-shell
fi

export OPAMROOT="$HOME/.cache/opam"
eval "$(opam env --switch 5.4.0+ox --set-switch)"
export DUNE_CACHE=disabled
pkg-config --exists openssl sqlite3 || {
  echo 'The Eta Nix shell must provide OpenSSL and SQLite native libraries.' >&2
  exit 1
}
if [[ "$(git -C "$eta_root" rev-parse HEAD)" != "$(git -C "$eta_root" rev-parse master)" ]] ||
   [[ -n "$(git -C "$eta_root" status --porcelain --untracked-files=no -- lib dune-project)" ]]; then
  echo 'Eta must be checked out at its clean local master for this test.' >&2
  exit 1
fi

mirror="$pie_root/_dev/eta-source"
prefix="$pie_root/_dev/eta-install"
build="$pie_root/_dev/pie-build"
run="$pie_root/_dev/test-run"
mkdir -p "$pie_root/_dev"
rm -rf "$mirror" "$prefix" "$run"
mkdir -p "$mirror" "$run/logs" "$run/alcotest"
# Dune does not traverse directory symlinks. Mirror real directories and
# symlink each source file; Eta's build products stay in this ignored tree.
cp -rs "$eta_root/lib" "$mirror/lib"
ln -s "$eta_root/dune-project" "$mirror/dune-project"

packages=eta,eta_ai,eta_ai_openai_codec,eta_ai_openrouter,eta_blocking,eta_eio,eta_http,eta_http_h1,eta_http_h2,eta_http_tls_openssl,eta_http_ws,eta_http_eio,eta_observability,eta_otel,eta_otel_eio,eta_redacted,eta_stream,eta_stream_eio
dune build --root "$mirror" --only-packages "$packages" @install
dune install --root "$mirror" --only-packages "$packages" --prefix "$prefix"
export OCAMLPATH="$prefix/lib${OCAMLPATH:+:$OCAMLPATH}"
export CAML_LD_LIBRARY_PATH="$prefix/lib/stublibs${CAML_LD_LIBRARY_PATH:+:$CAML_LD_LIBRARY_PATH}"
if [[ "$(ocamlfind query eta_http_eio)" != "$prefix/lib/eta_http_eio" ]]; then
  echo 'Local Eta installation did not take precedence over the shared switch.' >&2
  exit 1
fi
printf 'Eta master: %s\n' "$(git -C "$eta_root" rev-parse --short HEAD)"
dune build --root "$pie_root" --build-dir "$build" --only-packages pie @all

# Fail closed rather than accidentally using a real desktop/audio/session. Do
# not read or copy the user's auth; fixtures run with an empty private HOME.
tmp="$(mktemp -d /tmp/pie-local-eta.XXXXXXXX)"
trap 'chmod -R u+rwx -- "$tmp"; rm -rf -- "$tmp"' EXIT
chmod 0700 "$tmp"
export HOME="$tmp/home" XDG_CONFIG_HOME="$tmp/config" XDG_CACHE_HOME="$tmp/cache"
export XDG_DATA_HOME="$tmp/data" XDG_RUNTIME_DIR="$tmp/runtime" TMPDIR="$tmp/tmp"
export CODEX_HOME="$tmp/codex" OTEL_SDK_DISABLED=true
mkdir -p "$HOME" "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" \
  "$XDG_RUNTIME_DIR" "$TMPDIR" "$CODEX_HOME" "$tmp/bin"
for name in wl-copy wl-paste xclip xsel pactl pulseaudio parec curl wget \
  busctl niri wtype ydotool xdotool; do
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
