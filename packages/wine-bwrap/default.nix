{
  lib,
  pkgs,
  shareNet ? true,
}: let
  #
  # TODO: 002c:err:winediag:getaddrinfo Failed to resolve your host name IP
  #
  # TODO: 0170:err:winediag:ntlm_check_version ntlm_auth was not found. Make sure that ntlm_auth >= 3.0.25 is in your path. Usually, you can find it in the winbind package of your distribution.
  #
  # TODO: 00b0:err:ntoskrnl:ZwLoadDriver failed to create driver L"\\Registry\\Machine\\System\\CurrentControlSet\\Services\\winebth": c00000e5
  #
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
  wineGeckoVersion = let
    file = pkgs.runCommand "wine-gecko-version.txt" {} ''
      tar --wildcards -xOf ${wine.src} '*/dlls/appwiz.cpl/addons.c' \
          | sed -nE 's/^#define GECKO_VERSION "([^"]+)".*/\1/p' \
            > $out
    '';
    version = lib.fileContents file;
  in
    if version == ""
    then throw "wine-bwrap: unable to detect GECKO_VERSION from Wine source"
    else version;
  wineMonoDir = pkgs.fetchzip {
    name = "wine-mono-${wineMonoVersion}";
    url = "https://dl.winehq.org/wine/wine-mono/${wineMonoVersion}/wine-mono-${wineMonoVersion}-x86.tar.xz";
    hash = "sha256-0TFqmaFbSU0dXUpUhIzWUqhr0DPxh321marRKKM8nws=";
  };
  wineGeckoDir32 = pkgs.fetchzip {
    name = "wine-gecko-${wineGeckoVersion}-x86";
    url = "https://dl.winehq.org/wine/wine-gecko/${wineGeckoVersion}/wine-gecko-${wineGeckoVersion}-x86.tar.xz";
    hash = "sha256-UBqWt6dMNm/kBT2ny7paA4JZ/f+cfya9uuuFshFkBR4=";
  };
  wineGeckoDir64 = pkgs.fetchzip {
    name = "wine-gecko-${wineGeckoVersion}-x86_64";
    url = "https://dl.winehq.org/wine/wine-gecko/${wineGeckoVersion}/wine-gecko-${wineGeckoVersion}-x86_64.tar.xz";
    hash = "sha256-VyoNB/R0bTOmEdX4351NJ5kWhCz4a0WqGpRBhp4rT14=";
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
        ${
        # We’d need `--cap-add CAP_NET_RAW` for `ping 127.0.0.1`, but that can only be added by root.
        if shareNet
        then "--share-net"
        else ""
      }
        --proc /proc
        --dev /dev
        --ro-bind /nix/store /nix/store
        --ro-bind /etc/fonts /etc/fonts
        --ro-bind /sys/devices/system/cpu /sys/devices/system/cpu
        --tmpfs /tmp
        --bind "$wine_home" "$HOME"
        --ro-bind ${wineMonoDir} /opt/wine/mono/wine-mono-${wineMonoVersion}
        --ro-bind ${wineGeckoDir32} /opt/wine/gecko/wine-gecko-${wineGeckoVersion}-x86
        --ro-bind ${wineGeckoDir64} /opt/wine/gecko/wine-gecko-${wineGeckoVersion}-x86_64
        --bind "$wine_prefix" "$wine_prefix"
        --ro-bind /etc/resolv.conf /etc/resolv.conf
        --ro-bind /etc/static/ssl /etc/static/ssl
        --ro-bind /etc/ssl /etc/ssl
        --ro-bind /sys/block /sys/block
        --ro-bind /sys/class/block /sys/class/block
        --ro-bind /sys/dev/block /sys/dev/block
        --ro-bind /run/udev /run/udev
        --setenv HOME "$HOME"
        --setenv USER "$USER"
        --setenv PATH ${lib.makeBinPath [wine pkgs.coreutils pkgs.iproute2]}
        --setenv WINEPREFIX "$wine_prefix"
        --setenv LANG "$LANG"
        --setenv LOCALE_ARCHIVE "$LOCALE_ARCHIVE"
        --setenv LOCALE_ARCHIVE_2_27 "$LOCALE_ARCHIVE_2_27"
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
        -- ${lib.getExe pkgs.bash} -c ${lib.escapeShellArg ((lib.optionalString (!shareNet) ''
          # Set-up the loopback interface, because some apps will not accept missing network:
          ip link set lo up                 >/dev/null 2>&1 || true
          ip addr add 127.0.0.1/8 dev lo    >/dev/null 2>&1 || true
          ip -6 addr add ::1/128 dev lo     >/dev/null 2>&1 || true
          ip route add unreachable default  >/dev/null 2>&1 || true
          ip -6 route add unreachable ::/0  >/dev/null 2>&1 || true
        '')
        + ''
          wine "$@"
          status=$?
          wineserver -w
          exit "$status"
        '')} bash "$@"
    '';
    derivationArgs.meta.description = "Runs Wine inside Bubblewrap using a provided WINEPREFIX.";
    derivationArgs.meta.platforms = lib.platforms.linux;
  }
