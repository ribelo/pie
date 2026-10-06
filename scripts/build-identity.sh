#!/usr/bin/env bash
# Dune supplies the source root even when its build directory is elsewhere.
# An explicit root is useful for package/source-export regression checks.
set -eu
root="${1:-${DUNE_SOURCEROOT:-.}}"
commit=unknown
if [ -e "$root/.git" ] && command -v git >/dev/null 2>&1; then
  commit=$(git -C "$root" rev-parse --short=12 HEAD 2>/dev/null || printf unknown)
  if [ "$commit" != unknown ]; then
    status=$(git -C "$root" status --porcelain --untracked-files=normal 2>/dev/null) || commit=unknown
    if [ -n "$status" ] && [ "$commit" != unknown ]; then commit="$commit-dirty"; fi
  fi
fi
# Ordinary builds sample rule execution time, even in Nix development shells.
# The capability gate samples once for its inventory and compiler rebuilds.
built_at=${PIE_BUILD_IDENTITY_TIME:-$(date -u +%Y-%m-%dT%H:%M:%SZ)}
printf '(* Generated at build time. No runtime git or clock access. *)\nlet commit = "%s"\nlet time = "%s"\n' "$commit" "$built_at"
