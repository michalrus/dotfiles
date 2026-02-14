{
  lib,
  pkgs,
}: let
  wine = pkgs.wineWowPackages.stable;
  winetricks = pkgs.winetricks;
  ntlm_auth = pkgs.samba;
  getWineAddonVersion = defineName: fileName: let
    file = pkgs.runCommand "wine-${lib.toLower defineName}-version.txt" {} ''
      tar --wildcards -xOf ${wine.src} '*/${fileName}' \
          | sed -nE 's/.*${defineName}[[:space:]]+"([^"]+)".*/\1/p' \
          | head -n 1 \
            > $out
    '';
    version = lib.fileContents file;
  in
    if version == ""
    then throw "wine-bwrap: unable to detect ${defineName} from Wine source"
    else version;
  wineMonoVersion = getWineAddonVersion "WINE_MONO_VERSION" "dlls/mscoree/mscoree_private.h";
  wineGeckoVersion = getWineAddonVersion "GECKO_VERSION" "dlls/appwiz.cpl/addons.c";
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
  mkWrapper = {
    name,
    shareNet,
    entrypoint,
    extraPath ? [],
    requireArgs ? true,
  }:
    pkgs.writeShellApplication {
      inherit name;
      text = ''
        if [ -z "''${WINEPREFIX:-}" ]; then
          echo >&2 "${name}: WINEPREFIX must be set"
          exit 1
        fi

        ${lib.optionalString requireArgs ''
          if [ "$#" -lt 1 ]; then
            echo >&2 "usage: ${name} <exe> [args...]"
            exit 2
          fi
        ''}

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
          if shareNet
          then "--share-net"
          else "# We’d need `--cap-add CAP_NET_RAW` for `ping 127.0.0.1`, but that can only be added by root."
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
          --hostname localhost
          --ro-bind /etc/resolv.conf /etc/resolv.conf
          --ro-bind ${pkgs.writeText "etc-hosts" "127.0.0.1 localhost"} /etc/hosts
          --ro-bind /etc/static/ssl /etc/static/ssl
          --ro-bind /etc/ssl /etc/ssl
          --ro-bind /sys/block /sys/block
          --ro-bind /sys/class/block /sys/class/block
          --ro-bind /sys/dev/block /sys/dev/block
          --ro-bind /run/udev /run/udev
          --ro-bind /run/dbus/system_bus_socket /run/dbus/system_bus_socket
          --setenv HOME "$HOME"
          --setenv USER "$USER"
          --setenv PATH ${lib.makeBinPath ([wine pkgs.coreutils pkgs.iproute2 ntlm_auth] ++ extraPath)}
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
            ${entrypoint} "$@"
            status=$?
            wineserver -w
            exit "$status"
          '')} bash "$@"
      '';
      derivationArgs.meta.description = "Runs Wine inside Bubblewrap using a provided WINEPREFIX.";
      derivationArgs.meta.platforms = lib.platforms.linux;
    };

  wine-bwrap = mkWrapper {
    name = "wine-bwrap";
    shareNet = false;
    entrypoint = "wine";
  };

  wine-bwrap-networked = mkWrapper {
    name = "wine-bwrap-networked";
    shareNet = true;
    entrypoint = "wine";
  };

  wine-bwrap-winetricks = mkWrapper {
    name = "wine-bwrap-winetricks";
    shareNet = true;
    entrypoint = "winetricks";
    requireArgs = false;
    extraPath = [
      winetricks
      pkgs.cabextract
      pkgs.curl
      pkgs.gawk
      pkgs.gnused
      pkgs.p7zip
      pkgs.unzip
      pkgs.wget
    ];
  };
in
  pkgs.symlinkJoin {
    name = "wine-bwrap";
    paths = [
      wine-bwrap
      wine-bwrap-networked
      wine-bwrap-winetricks
    ];
    meta.description = "Wine Bubblewrap wrappers with optional networking and winetricks.";
    meta.platforms = lib.platforms.linux;
  }
