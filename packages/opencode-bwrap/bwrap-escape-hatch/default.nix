{
  lib,
  pkgs,
  plugins,
  ...
}: let
  package = pkgs.rustPlatform.buildRustPackage {
    pname = "bwrap-escape-hatch";
    version = "0.1.0";
    src = lib.cleanSource ./.;
    cargoHash = "sha256-VKjZZG0cUzyi7HssYJ4Rd4QVS3CsNul0fv4lg0ly924=";

    meta = {
      description = "Controlled sandbox escape hatch via UNIX socket — allows sandboxed processes to request specific commands on the host";
      platforms = lib.platforms.linux;
      mainProgram = "bwrap-escape-hatch";
    };
  };

  # Generate per-command shim scripts for use inside the sandbox.
  # Each shim connects to the escape-hatch socket and proxies argv as JSON.
  mkGuestWrappers = cmds:
    pkgs.symlinkJoin {
      name = "bwrap-escape-hatch-shims";
      paths = map (cmd:
        pkgs.writeShellApplication {
          name = cmd;
          runtimeInputs = [pkgs.socat pkgs.jq];
          text = ''
            resp=$(socat - \
              UNIX-CONNECT:"$XDG_RUNTIME_DIR/bwrap-escape-hatch.sock" \
              <<< "$(jq -cn '$ARGS.positional' --args -- ${lib.escapeShellArg cmd} "$@")")
            last_line=$(printf '%s\n' "$resp" | tail -1)
            error=$(printf '%s' "$last_line" | jq -r '.error // empty')
            if [[ -n "$error" ]]; then
              echo "${cmd}: $error" >&2
            fi
            exit_code=$(printf '%s' "$last_line" | jq -r '.exit_code // 1')
            exit "$exit_code"
          '';
        })
      cmds;
    };

  hmModule = import ./hm-module.nix {inherit package plugins;};
in {
  inherit package mkGuestWrappers hmModule;
}
