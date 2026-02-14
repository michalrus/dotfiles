{
  lib,
  pkgs,
}: let
  wine = pkgs.wineWowPackages.stable;
  wineMonoVersion = let
    file = pkgs.runCommand "wine-mono-version.txt" {} ''
      tar --wildcards -xOf ${wine.src} '*/dlls/mscoree/mscoree_private.h' \
          | grep -aE 'WINE_MONO_VERSION' \
            | sed -E 's/.*"([^"]+)".*/\1/' \
            > $out
    '';
    version = lib.fileContents file;
  in
    if version == ""
    then throw "wine-bwrap: unable to detect WINE_MONO_VERSION from Wine source"
    else version;
  wineMonoMsi = pkgs.fetchurl rec {
    name = "wine-mono-${wineMonoVersion}-x86.msi";
    url = "https://dl.winehq.org/wine/wine-mono/${wineMonoVersion}/${name}";
    hash = "sha256-z2FzrpS3np3hPZp0zbJWCohvw9Jx+Uiayxz9vZYcrLI=";
  };
in
  pkgs.writeShellApplication {
    name = "wine-bwrap";
    text = ''
      if [ -z "''${WINEPREFIX:-}" ]; then
        echo >&2 "wine-bwrap: WINEPREFIX must be set"
        exit 1
      fi

      if [ "$#" -lt 1 ]; then
        echo >&2 "usage: wine-bwrap <exe> [args...]"
        exit 2
      fi

      abspath() {
        local p="$1"
        if [[ "$p" == /* ]]; then
          printf '%s\n' "$p"
        else
          ( cd "$(dirname -- "$p")" && printf '%s/%s\n' "$(pwd -L)" "$(basename -- "$p")" )
        fi
      }

      wine_prefix="$(abspath "$WINEPREFIX")"
      wine_home="$wine_prefix/sandbox-home"
      mkdir -p "$wine_prefix" "$wine_home"

      bwrap_opts=(
        --unshare-all
        --new-session
        --die-with-parent
        --clearenv
        --proc /proc
        --dev /dev
        --ro-bind /nix/store /nix/store
        --ro-bind /etc/fonts /etc/fonts
        --ro-bind /sys/devices/system/cpu /sys/devices/system/cpu
        --tmpfs /tmp
        --bind "$wine_home" "$HOME"
        --ro-bind ${wineMonoMsi} /opt/wine/mono/${wineMonoMsi.name}
        --bind "$wine_prefix" "$wine_prefix"
        --ro-bind /etc/hosts /etc/hosts
        --ro-bind /etc/nsswitch.conf /etc/nsswitch.conf
        --ro-bind /etc/resolv.conf /etc/resolv.conf
        --ro-bind /etc/hostname /etc/hostname
        --ro-bind /sys/block /sys/block
        --ro-bind /sys/class/block /sys/class/block
        --ro-bind /sys/dev/block /sys/dev/block
        --ro-bind /run/udev /run/udev
        --setenv HOME "$HOME"
        --setenv USER "$USER"
        --setenv PATH ${lib.makeBinPath [wine pkgs.coreutils]}
        --setenv WINEPREFIX "$wine_prefix"
      )

      if [ -n "''${WINEARCH:-}" ]; then
        bwrap_opts+=( --setenv WINEARCH "$WINEARCH" )
      fi

      if [ -n "''${DISPLAY:-}" ]; then
        bwrap_opts+=(
          --ro-bind /tmp/.X11-unix /tmp/.X11-unix
          --setenv DISPLAY "$DISPLAY"
          --ro-bind "''${XAUTHORITY:-$HOME/.Xauthority}" /tmp/.Xauthority
          --setenv XAUTHORITY /tmp/.Xauthority
        )
      fi

      # shellcheck disable=SC2016
      exec bwrap \
        "''${bwrap_opts[@]}" \
        -- ${lib.getExe pkgs.bash} -c '
          wine "$@"
          status=$?
          wineserver -w
          exit "$status"
        ' bash "$@"
    '';
    derivationArgs.meta.description = "Runs Wine inside Bubblewrap using a provided WINEPREFIX.";
    derivationArgs.meta.platforms = lib.platforms.linux;
  }
