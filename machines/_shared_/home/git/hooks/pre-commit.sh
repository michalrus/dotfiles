#!/usr/bin/env bash
set -euo pipefail

if [ -n "${SKIP_TREEFMT+x}" ]; then
  exit 0
fi

if treefmt --fail-on-change; then
  exit 0
else
  status=$?
  echo >&2 'error: treefmt detected (and corrected) unformatted code'
  echo >&2 'hint: set SKIP_TREEFMT=1 to skip treefmt in pre-commit'
  exit "$status"
fi
