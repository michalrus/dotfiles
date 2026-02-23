#!/usr/bin/env bash
set -euo pipefail

while read -r local_ref _local_sha _remote_ref _remote_sha; do
  case "$local_ref" in
  refs/heads/private | refs/heads/private/*)
    echo >&2 "error: push rejected for private branch: ${local_ref#refs/heads/}"
    echo >&2 "hint: \`private' and \`private/*' branches are configured as local-only (not pushable)"
    exit 1
    ;;
  esac
done

exit 0
