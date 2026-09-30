#!/usr/bin/env bash
# Run a command with the toolchain and the local Ox installation that
# scripts/test-local-ox.sh last built into _dev/ox-install, without rebuilding
# or reinstalling Ox. Several of these can run at once when each uses its own
# --build-dir. Never installs anything.
#
#   scripts/dev-env.sh dune build --root "$PWD" --build-dir "$PWD/_dev/b-history" --only-packages pie @all
#   scripts/dev-env.sh --private-home "$PWD/_dev/b-history/default/packages/pie/test/history/test_history.exe"
#
# Dune needs absolute --root and --build-dir here.
#
# --private-home runs the command like the full suite does: temporary HOME and
# XDG directories, no provider credentials, and fail-closed desktop helpers.
set -euo pipefail

pie_root="$(cd "$(dirname "$0")/.." && pwd -P)"
ox_root="$(cd "${OX_PATH:-$pie_root/../ocaml/ox}" && pwd -P)"
if [[ "${1:-}" != --in-shell ]]; then
  exec nix develop "$ox_root" -c bash "$0" --in-shell "$@"
fi
shift

prefix="$pie_root/_dev/ox-install"
if [[ ! -d "$prefix/lib" ]]; then
  echo "No local Ox installation at $prefix; run scripts/test-local-ox.sh once." >&2
  exit 1
fi
export OPAMROOT="$HOME/.cache/opam"
eval "$(opam env --switch 5.4.0+ox --set-switch)"
export DUNE_CACHE=disabled
export OCAMLPATH="$prefix/lib${OCAMLPATH:+:$OCAMLPATH}"
export CAML_LD_LIBRARY_PATH="$prefix/lib/stublibs${CAML_LD_LIBRARY_PATH:+:$CAML_LD_LIBRARY_PATH}"

if [[ "${1:-}" == --private-home ]]; then
  shift
  tmp="$(mktemp -d /tmp/pie-dev-env.XXXXXXXX)"
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
  unset OPENAI_API_KEY OPENROUTER_API_KEY PIE_GLOSSARY || true
  unset DBUS_SESSION_BUS_ADDRESS PULSE_SERVER NIRI_SOCKET WAYLAND_DISPLAY DISPLAY || true
  "$@"
else
  exec "$@"
fi
