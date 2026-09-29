#!/bin/sh
# PIE020: external deadlines also catch a stalled Eio scheduler.
set -eu
case "$1" in
  /*) exe=$1 ;;
  *) exe=./$1 ;;
esac
runs=${2:-50}
i=1
while [ "$i" -le "$runs" ]; do
  if timeout --kill-after=1 5 env EIO_BACKEND=posix "$exe" >stress.log 2>&1; then
    i=$((i + 1))
  else
    status=$?
    printf "prepared-text stress: run %s/%s failed (status %s); see stress.log\n" "$i" "$runs" "$status" >&2
    exit "$status"
  fi
done
printf "prepared-text stress: %s/%s passed\n" "$runs" "$runs"
