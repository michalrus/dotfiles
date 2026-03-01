{
  pkgs,
  lib,
  nixpkgs-unstable,
  bun2nix,
  serena,
}: let
  unsafe = nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.opencode;

  escapeHatch = pkgs.callPackage ./bwrap-escape-hatch {inherit plugins;};
  escapeHatchShims = escapeHatch.mkGuestWrappers ["notify-send" "aplay"];

  plugins = import ./plugins.nix {inherit pkgs lib bun2nix;};
  inherit (plugins) opencode-plugins;

  config = {
    "$schema" = "https://opencode.ai/config.json";
    share = "disabled";
    theme = "solarized";
    lsp = false;
    formatter = {
      biome.disabled = true;
      cargofmt.disabled = true;
      oxfmt.disabled = true;
      ruff.disabled = true;
      rubocop.disabled = true;
      rustfmt.disabled = true;
      shfmt.disabled = true;
      standardrb.disabled = true;
      uv.disabled = true;
      nixfmt.disabled = true;
      prettier.disabled = true;
      treefmt = {
        command = ["treefmt" "$FILE"];
        extensions = [
          ".bash"
          ".cjs"
          ".css"
          ".envrc"
          ".envrc.*"
          ".html"
          ".js"
          ".json"
          ".json5"
          ".jsonc"
          ".jsx"
          ".md"
          ".mdx"
          ".mjs"
          ".nix"
          ".py"
          ".pyi"
          ".rb"
          ".rs"
          ".scss"
          ".sh"
          ".toml"
          ".ts"
          ".tsx"
          ".vue"
          ".yaml"
          ".yml"
        ];
      };
    };
    tui = {
      diff_style = "stacked";
    };
    mcp = {
      serena = {
        type = "local";
        command = ["serena" "start-mcp-server"];
        enabled = true;
      };
    };
    autoupdate = false;
    experimental = {
      disable_paste_summary = true;
    };
    instructions = [
      ./preamble.md
    ];
    agent = {
      build = {
        # We’re running in a strict sandbox, so let’s relax the default permissions:
        permission = {
          read = "allow"; # reading a file
          edit = "allow"; # all file modifications
          glob = "allow"; # file globbing – like `fd` but builtin
          grep = "allow"; # content search – like `rg` but builtin
          list = "allow"; # listing files in a directory
          bash = "allow"; # running shell commands
          task = "allow"; # launching subagents
          skill = "allow"; # loading a skill (from the current repository)
          lsp = "deny"; # running LSP queries (currently non-granular) – we have a better `serena` for this
          todoread = "allow"; # reading the todo list
          todowrite = "allow"; # updating the todo list
          webfetch = "allow"; # fetching a URL
          websearch = "allow"; # web/code search
          codesearch = "allow"; # web/code search
          external_directory = "allow"; # triggered when a tool touches paths outside the project working directory
          doom_loop = "deny"; # triggered when the same tool call repeats 3 times with identical input
        };
      };
    };
  };

  bashrc = pkgs.writeText "opencode-bashrc" ''
    ${builtins.readFile ./bashrc}
    eval "$(${lib.getExe pkgs.direnv} hook bash)"
  '';

  zshrc = pkgs.writeText "opencode-zshrc" ''
    ${builtins.readFile ./zshrc}
    eval "$(${lib.getExe pkgs.direnv} hook zsh)"
  '';

  # With `--new-session` we don’t have a controlling TTY for the Bash inside the
  # sandbox, so everything works a little weird. But with it, keystrokes could
  # be injected into the controlling terminal from within the sandbox using the
  # TIOCSTI ioctl.
  #
  # The following program emits a seccomp BPF program that blocks ioctl(...,
  # TIOCSTI, ...).
  bwrapTiocstiFilter = pkgs.stdenv.mkDerivation rec {
    name = "bwrap-tiocsti-seccomp-filter";
    dontUnpack = true;
    nativeBuildInputs = [pkgs.pkg-config];
    buildInputs = [pkgs.libseccomp];
    src = ./seccomp-tiocsti-filter.c;
    buildPhase = ''
      cc -O2 -Wall -Wextra -o gen "$src" -lseccomp
    '';
    installPhase = ''
      mkdir -p "$out/bin"
      install -m755 gen "$out/bin/${name}"
    '';
    meta.mainProgram = name;
  };

  safe = pkgs.writeShellApplication {
    name = "opencode-bwrap";
    runtimeInputs = with pkgs; [coreutils findutils];
    text = ''
      data_dir="$HOME"/.local/share/opencode-bwrap

      sandbox_home="$data_dir"/home
      mkdir -p "$sandbox_home"

      # Only these will persist in the sandbox $HOME:
      persist_dirs=(
        .bin
        .cache .config .local
        .cargo
        .bun .npm .yarn
      )
      persist_files=(
        .bash_history .zsh_history .python_history
      )

      shell_exe=${lib.getExe pkgs.zsh}

      GID=$(id -g)

      etc_passwd="$data_dir/etc-passwd"
      echo "$USER:x:$UID:$GID::$HOME:$shell_exe" >"$etc_passwd"

      etc_group="$data_dir/etc-group"
      printf "users:x:%s:\nnogroup:x:65534:" "$GID" >"$etc_group"

      bwrap_opts=(
        --unshare-all
        --die-with-parent
        --clearenv
        --proc /proc
        --dev /dev
        --share-net
        --tmpfs /tmp
        --tmpfs /run/user/"$UID"
        --ro-bind /run/user/"$UID"/bwrap-escape-hatch.sock /run/user/"$UID"/bwrap-escape-hatch.sock
        --setenv XDG_RUNTIME_DIR /run/user/"$UID"
        --tmpfs "$HOME"
        --ro-bind /nix /nix
        --ro-bind /etc/resolv.conf /etc/resolv.conf
        --ro-bind "$etc_passwd" /etc/passwd
        --ro-bind "$etc_group" /etc/group
        --ro-bind /run/current-system/sw /run/current-system/sw/
        --ro-bind /etc/profiles/per-user/"$USER" /etc/profiles/per-user/"$USER"
        --ro-bind /etc/static/ssl /etc/static/ssl
        --ro-bind /etc/ssl /etc/ssl
        --ro-bind /etc/static/nix /etc/static/nix
        --ro-bind /etc/nix /etc/nix
        --ro-bind /etc/static/terminfo /etc/static/terminfo
        --ro-bind /etc/terminfo /etc/terminfo
        --ro-bind /bin/sh /bin/sh
        --ro-bind /usr/bin/env /usr/bin/env
        --ro-bind ${bashrc} /etc/bashrc
        --ro-bind ${zshrc} /etc/zshrc
        --ro-bind ${pkgs.emptyFile} "$HOME"/.zshrc
        --ro-bind "${pkgs.nix-direnv}/share/nix-direnv/direnvrc" "$HOME"/.config/direnv/lib/nix-direnv.sh
        --setenv SHELL "$shell_exe"
        --setenv HOME "$HOME"
        --setenv PATH ${lib.makeBinPath [
        unsafe
        serena
        escapeHatchShims
      ]}:/etc/profiles/per-user/"$USER"/bin:/run/current-system/sw/bin:"$HOME"/.bin
        --setenv USER "$USER"
        --setenv TERM "$TERM"
        --setenv TERMINFO_DIRS /etc/profiles/per-user/"$USER"/share/terminfo:/run/current-system/sw/share/terminfo
        --setenv LANG "$LANG"
        --setenv LOCALE_ARCHIVE "$LOCALE_ARCHIVE"
        --setenv LOCALE_ARCHIVE_2_27 "$LOCALE_ARCHIVE_2_27"
        --setenv NIX_PATH ${lib.escapeShellArg "nixpkgs=${pkgs.path}"}
        --setenv OPENCODE_DISABLE_LSP_DOWNLOAD "true"
        --setenv OPENCODE_CONFIG ${pkgs.writeText "config.json" (builtins.toJSON config)}
      )

      for d in "''${persist_dirs[@]}" ; do
        mkdir -p "$sandbox_home"/"$d"
        bwrap_opts+=( --bind "$sandbox_home"/"$d" "$HOME"/"$d" )
      done

      for f in "''${persist_files[@]}" ; do
        touch "$sandbox_home"/"$f"
        bwrap_opts+=( --bind "$sandbox_home"/"$f" "$HOME"/"$f" )
      done

      if [ -f "$HOME"/.config/git/ignore ] ; then
        bwrap_opts+=( --ro-bind "$HOME"/.config/git/ignore "$HOME"/.config/git/ignore )
      fi

      # OpenCode plugins (pinned via fetchFromGitHub, mounted read-only)
      bwrap_opts+=( --ro-bind ${opencode-plugins} "$HOME"/.config/opencode/plugins )

      rw_opts=()
      ro_git_opts=()

      # Make argv absolute without resolving symlinks (pwd -L)
      abspath() {
        local p="$1"
        if [[ "$p" == /* ]]; then
          printf '%s\n' "$p"
        else
          ( cd "$(dirname -- "$p")" && printf '%s/%s\n' "$(pwd -L)" "$(basename -- "$p")" )
        fi
      }

      for d in "$@"; do
        [ -d "$d" ] || {
          echo >&2 "$0: cannot access '$d': No such file or directory"
          exit 1
        }

        d="$(abspath "$d")"

        # Mount project dir at same path (read-write)
        rw_opts+=( --bind "$d" "$d" )

        # Then over-mount any `.git` entries inside it as read-only (dir, file, or symlink)
        if [ -z "''${OPENCODE_UNSAFE_RW_GIT-}" ]; then
          while IFS= read -r -d "" gitpath; do
            ro_git_opts+=( --ro-bind "$gitpath" "$gitpath" )
          done < <(
            find "$d" \
              -name .git \
              \( -type d -o -type f -o -type l \) \
              -print0 2>/dev/null || true
          )
        fi
      done

      exec bwrap \
        "''${bwrap_opts[@]}" \
        "''${rw_opts[@]}" \
        "''${ro_git_opts[@]}" \
        --seccomp 3 3< <(${lib.getExe bwrapTiocstiFilter}) \
        -- "$shell_exe"
    '';
    derivationArgs = {
      meta.description = "Enters a (multi-)project sandbox to run `opencode` inside; `.git` entries are mounted read-only unless OPENCODE_UNSAFE_RW_GIT is set.";
      meta.platforms = lib.platforms.linux;
      passthru.bwrap-escape-hatch = escapeHatch // {inherit escapeHatchShims;};
      passthru.plugins = plugins;
    };
  };
in
  safe
