#!/usr/bin/env bash
set -euo pipefail

if [ -n "${SKIP_TREEFMT+x}" ]; then
  exit 0
fi

if treefmt --ci; then
  exit 0
else
  status=$?
  # shellcheck disable=SC2016
  echo >&2 'hint: set `SKIP_TREEFMT=1` to skip `treefmt` in pre-commit.'
  exit "$status"
fi
