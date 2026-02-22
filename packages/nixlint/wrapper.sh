#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: nixlint [--fix] <file-or-dir>..." >&2
}

fix=0

if ! parsed=$(getopt -o "h" -l fix,help -- "$@"); then
  usage
  exit 2
fi

eval set -- "$parsed"

while true; do
  case "$1" in
  --fix)
    fix=1
    shift
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  --)
    shift
    break
    ;;
  esac
done

targets=("$@")

if [ "${#targets[@]}" -eq 0 ]; then
  usage
  exit 2
fi

declare -A seen
files=()

add_file() {
  local file="$1"
  if [ -n "${seen[$file]+set}" ]; then
    return
  fi
  seen["$file"]=1
  files+=("$file")
}

add_path() {
  local target="$1"
  if [ -d "$target" ]; then
    while IFS= read -r -d "" file; do
      add_file "$file"
    done < <(find "$target" -type f -iname "*.nix" -print0)
    return
  fi

  if [ -f "$target" ]; then
    add_file "$target"
    return
  fi

  echo "nixlint: $target: No such file or directory" >&2
  exit 2
}

for target in "${targets[@]}"; do
  add_path "$target"
done

if [ "${#files[@]}" -eq 0 ]; then
  echo "nixlint: no .nix files found" >&2
  exit 0
fi

ec=0

if [ "$fix" -eq 1 ]; then
  for file in "${files[@]}"; do
    statix fix "$file" || ec=1
  done

  printf '%s\0' "${files[@]}" | xargs -0 deadnix --edit || ec=1
  exit $ec
fi

for file in "${files[@]}"; do
  statix check "$file" || ec=1
done

printf '%s\0' "${files[@]}" | xargs -0 deadnix --fail || ec=1

for file in "${files[@]}"; do
  nil diagnostics "$file" || ec=1
done

for file in "${files[@]}"; do
  if ! errors=$(nixf-tidy --variable-lookup --pretty-print <"$file" | jq -c ".[]" | sed -r "s#^#$file: #"); then
    ec=1
    continue
  fi
  if [ -n "$errors" ]; then
    printf "%s\n\n" "$errors"
    ec=1
  fi
done

exit $ec
