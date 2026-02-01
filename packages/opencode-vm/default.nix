{ nixpkgs, nixpkgs-unstable, home-manager, pkgs, lib }:

# See <https://opencode.ai/docs>.

# Note: to mount `.git` directories read-only, this requires `{ programs.fuse.userAllowOther = true; }`.

assert pkgs.stdenv.hostPlatform.isLinux;

let

  workDirPlaceholder = "QEMU_WORK_DIR_PLACEHOLDER";
  sshPortPlaceholder = 22222;

  vmConfig =
    { config, modulesPath, pkgs, lib, ... }:
    {
      imports = [
        (modulesPath + "/virtualisation/qemu-vm.nix")
        ../../machines/_shared_/features/immutable-users
        ../../machines/_shared_/features/locale-en-iso
        ../../machines/_shared_/features/nix.conf
        ../../machines/_shared_/features/zsh
        ../../modules/gnu-screen
        home-manager.nixosModules.home-manager
      ];

      virtualisation = {
        mountHostNixStore = true;
        graphics = false; # don’t open a separate QEMU window, treat the host terminal as serial console
        cores = 4; # vCPUs
        memorySize = 8 * 1024; # RAM in MiB
        forwardPorts = [
          { from = "host"; host.port = sshPortPlaceholder; guest.port = 22; }
        ];
        sharedDirectories = {
          workDir = {
            source = workDirPlaceholder;
            target = "/mnt/Work";
          };
        };

        # We have to bind-mount it to set UID:GID to 0:0. This is not sensitive,
        # and doesn’t give any more privileges than are already granted. Just to
        # make `nix-direnv` work better.
        fileSystems."/root/Work" = {
          device = "/mnt/Work";
          fsType = "fuse.bindfs";
          options = [
            "force-user=root"
            "force-group=root"
          ];
          depends = [ "/mnt/Work" ];
        };
      };

      networking.hostName = "opencode-vm";
      time.timeZone = "Europe/Warsaw";
      system.stateVersion = "25.11";

      services.getty.autologinUser = "root";
      users.users.root = {
        hashedPassword = "!";
        openssh.authorizedKeys.keyFiles = [ ./id_ed25519-opencode-vm.pub ];
      };

      services.openssh.enable = true;
      services.openssh.settings.PasswordAuthentication = true;
      services.openssh.settings.PermitRootLogin = "yes";

      nix.gc.automatic = lib.mkForce false;
      nix.settings.auto-optimise-store = lib.mkForce false;

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          { home.stateVersion = "25.11"; }
          ../../machines/_shared_/home/shells
          ../../machines/_shared_/home/git
          ../../machines/_shared_/home/gnu-screen
          { home.sessionVariables.STARSHIP_NO_GIT_INTEGRATION = 1; }
        ];
        users.root.imports = [ ];
      };

      environment.systemPackages = with pkgs; [
        (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf ]))
        bindfs
        cargo
        cargo-nextest
        curl
        delta
        fd
        file
        fzf
        gh
        git
        gnupg
        httpie # HTTP requests
        inetutils
        jq
        lsof
        moreutils
        netcat-openbsd
        nixd
        nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.opencode
        openssl
        p7zip
        ripgrep
        rust-analyzer
        rustc
        rustfmt
        shellcheck
        shfmt
        tree
        unzip
        wget
        xh # HTTP requests
        yq # jq for YAML
        zip
      ];
    };

  vm = nixpkgs.lib.nixosSystem {
    inherit (pkgs.stdenv.hostPlatform) system;
    modules = [ vmConfig ];
  };

in

# Creates a temp root directory, bind-mounts each repo into it via bindfs, then
# bind-mounts each mounted repo's .git read-only (if present), runs the OpenCode
# VM, and cleans everything up on exit.
pkgs.writeShellApplication {
  name = "opencode-vm";
  runtimeInputs = [];
  text = ''
    if (( $# < 2 )); then
      echo >&2 "error: no SSH port and/or directories provided"
      echo >&2 "usage: $0 <ssh-port> /path/to/repo1 [/path/to/repo2 ...]"
      exit 2
    fi

    ssh_port="$1"
    shift

    if [[ ! "$ssh_port" =~ ^[0-9]+$ ]] || (( ssh_port < 1 || ssh_port > 65535 )); then
      echo >&2 "error: invalid SSH port: $ssh_port"
      exit 2
    fi

    for repo in "$@"; do
      if [[ ! -d "$repo" ]]; then
        echo "error: not a directory: $repo" >&2
        exit 2
      fi
    done

    tmp_root="$(mktemp -d)"

    # Track mountpoints for deterministic cleanup (unmount in reverse order).
    declare -a mounts=()

    cleanup() {
      local rc=$?
      set +e

      # Unmount in reverse order (inner mounts first).
      for (( i=''${#mounts[@]}-1; i>=0; i-- )); do
        mp="''${mounts[i]}"
        if command -v mountpoint >/dev/null 2>&1 && mountpoint -q "$mp"; then
          fusermount -u "$mp" 2>/dev/null || fusermount3 -u "$mp" 2>/dev/null || true
        else
          # If mountpoint isn't available or doesn't think it's mounted, still try.
          fusermount -u "$mp" 2>/dev/null || fusermount3 -u "$mp" 2>/dev/null || true
        fi
      done

      rm -rf "$tmp_root" 2>/dev/null || true

      exit "$rc"
    }

    trap cleanup EXIT INT TERM

    for repo in "$@"; do
      name="$(basename "$repo")"
      dest="$tmp_root/$name"
      mkdir -p "$dest"

      # Bind-mount the whole repo.
      bindfs -o no-allow-other,allow_root,default_permissions "$repo" "$dest"
      mounts+=("$dest")

      # Hide .git by mounting an empty directory over it (only if it exists).
      if [[ -d "$repo/.git" ]]; then
        mkdir -p "$dest/.git"
        bindfs -o no-allow-other,allow_root,default_permissions,ro "$repo/.git" "$dest/.git"
        mounts+=("$dest/.git")
      fi
    done

    echo "Work dir on host is: $tmp_root"

    export QEMU_WORK_DIR=$tmp_root
    export QEMU_SSH_PORT=$ssh_port
    export NIX_DISK_IMAGE="$HOME"/.cache/opencode-vm.qcow2
    ${
      pkgs.writeShellScript "run-vm" (lib.replaceStrings
        [ workDirPlaceholder (toString sshPortPlaceholder) ]
        [ "$QEMU_WORK_DIR" "$QEMU_SSH_PORT" ]
        (builtins.readFile (lib.getExe vm.config.system.build.vm)))
    }
  '';
  derivationArgs.meta.platforms = lib.platforms.linux;
}
