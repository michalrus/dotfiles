#!/usr/bin/env bash
set -euo pipefail

if [ -n "${SKIP_COG+x}" ]; then
  exit 0
fi

if cog verify --file "$1"; then
  exit 0
else
  status=$?
  echo >&2 'hint: set SKIP_COG=1 to skip Conventional Commits verification in commit-msg'
  exit "$status"
fi
