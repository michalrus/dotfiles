{
  lib,
  pkgs,
  plugins,
  ...
}: let
  package = pkgs.rustPlatform.buildRustPackage {
    pname = "bwrap-escape-hatch";
    version = "0.1.0";
    src = lib.sources.sourceFilesBySuffices (lib.cleanSource ./.) [".rs" "Cargo.toml" "Cargo.lock"];
    cargoHash = "sha256-VKjZZG0cUzyi7HssYJ4Rd4QVS3CsNul0fv4lg0ly924=";

    meta = {
      description = "Controlled sandbox escape hatch via UNIX socket — allows sandboxed processes to request specific commands on the host";
      platforms = lib.platforms.linux;
      mainProgram = "bwrap-escape-hatch";
    };
  };

  # Generate per-command shim scripts for use inside the sandbox.
  # Each shim connects to the escape-hatch socket and proxies argv as JSON.
  # `specs` is a list of { name, hostBin } where `name` is the shim command
  # visible inside the guest, and `hostBin` is the full /nix/store path that
  # the escape-hatch daemon will execute on the host.
  mkGuestWrappers = specs:
    pkgs.symlinkJoin {
      name = "bwrap-escape-hatch-shims";
      paths = map ({
        name,
        hostBin,
      }:
        pkgs.writeShellApplication {
          inherit name;
          runtimeInputs = [pkgs.socat pkgs.jq];
          text = ''
            resp=$(socat - \
              UNIX-CONNECT:"$XDG_RUNTIME_DIR/bwrap-escape-hatch.sock" \
              <<< "$(jq -cn '$ARGS.positional' --args -- ${lib.escapeShellArg hostBin} "$@")")
            last_line=$(printf '%s\n' "$resp" | tail -1)
            error=$(printf '%s' "$last_line" | jq -r '.error // empty')
            if [[ -n "$error" ]]; then
              echo "${name}: $error" >&2
            fi
            exit_code=$(printf '%s' "$last_line" | jq -r '.exit_code // 1')
            exit "$exit_code"
          '';
        })
      specs;
    };

  hmModule = import ./hm-module.nix {inherit package plugins;};
in {
  inherit package mkGuestWrappers hmModule;
}
