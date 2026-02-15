{
  lib,
  writeShellApplication,
  findutils,
  statix,
  deadnix,
  nil,
  nixf,
  jq,
  gnused,
  coreutils,
}:
writeShellApplication {
  name = "nixlint";
  runtimeInputs = [
    findutils
    statix
    deadnix
    nil
    nixf
    jq
    gnused
    coreutils
  ];
  text = ''
    set -euo pipefail

    if [ "$#" -eq 0 ]; then
      echo "Usage: nixlint <file-or-dir>..." >&2
      exit 2
    fi

    declare -A seen
    files=()

    add_file() {
      local file="$1"
      if [ -n "''${seen[$file]+set}" ]; then
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

    for target in "$@"; do
      add_path "$target"
    done

    if [ "''${#files[@]}" -eq 0 ]; then
      echo "nixlint: no .nix files found" >&2
      exit 0
    fi

    ec=0

    for file in "''${files[@]}"; do
      statix check "$file" || ec=1
    done

    printf '%s\0' "''${files[@]}" | xargs -0 deadnix --fail || ec=1

    for file in "''${files[@]}"; do
      nil diagnostics "$file" || ec=1
    done

    for file in "''${files[@]}"; do
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
  '';
  derivationArgs.meta.description = "Run Nix linters on files or directories";
  derivationArgs.meta.platforms = lib.platforms.linux;
}
