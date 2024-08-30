{ path, system, lib, runCommand, xorg, writeShellScript, xterm, dbus, systemd, buildEnv

# Arguments you probably want to customize:

, xserverConfig ? {}                             # same format as ‘services.xserver’ in ‘configuration.nix’
, pureXauthority ? true                          # unfortunately, ‘pureXauthority’ doesn’t work with ‘emacs-daemon’
, windowManager ? "${xterm}/bin/xterm"           # entrypoint to you window manager, e.g. i3
, extraPackages ? []                             # extra packages to add to your transient profile
, extraFonts ? []                                # extra fonts for FontPath, give it your ‘config.fonts.fonts’
, loadXresources ? null                          # optional path to load with xrdb
, extraXserverArgs ? []                          # if ‘xserverConfig’ is not enough
, journaldIdentifier ? baseNameOf windowManager  # we log to journald instead of scattered log files
, profileRelativeEnvVars ? null                  # give it your ‘config.environment.profileRelativeEnvVars’
}:

let

  mkStandardSystem = module: import "${path}/nixos/lib/eval-config.nix" {
    inherit system;
    modules = [({ pkgs, lib, ... }: {
      system.stateVersion = lib.version;
      boot.loader.systemd-boot.enable = true;
      fileSystems."/" = { device = "/dev/disk/by-uuid/00000000-0000-0000-0000-000000000000"; fsType = "ext4"; };
    }) module];
  };

  standardSystem = mkStandardSystem {
    services.xserver = xserverConfig // {
      enable = lib.mkForce true;
      exportConfiguration = lib.mkForce true;
    };
    fonts.packages = extraFonts;
  };

  # Take the ‘xorg.conf’ generated by the standard NixOS:
  xorgConf = let
    etc = standardSystem.config.system.build.etc;
    sw = standardSystem.config.system.path;
  in runCommand "xorg.conf" {} ''
    cat \
      ${sw}/share/X11/xorg.conf.d/*.conf \
      ${etc}/etc/X11/xorg.conf.d/*.conf \
      ${etc}/etc/X11/xorg.conf \
      >$out
  '';

  essentialPackages =
    lib.subtractLists
    (mkStandardSystem {}).config.environment.systemPackages
    standardSystem.config.environment.systemPackages;

  xkbDir = standardSystem.config.services.xserver.xkb.dir;

  # A purer ‘${xorg.xinit}/bin/startx’.
  pureStartx = runCommand "pure-startx" {} ''
    cp ${xorg.xinit}/bin/startx $out
    chmod 755 $out

    # Don’t leave a hanging `bin/sh`, instead become `xinit`, and don’t require `xinit` on `PATH`:
    sed -r 's#^xinit #exec ${xorg.xinit}/bin/xinit #g' -i $out

    # Don’t use config files in HOME:
    sed -r 's/^(userclientrc)=.*/unset \1/g' -i $out
    sed -r 's/^(userserverrc)=.*/unset \1/g' -i $out

    # Log to stdout/stderr:
    sed -r 's#\$HOME/\.xorg\.log#/dev/null#g' -i $out

    # Keep serverauth in `/run/user/`, separate for each xserver:
    sed -r 's#HOME/\.serverauth#XDG_RUNTIME_DIR/.Xserverauth#g' -i $out

    # Don’t unset `DBUS_SESSION_BUS_ADDRESS`:
    sed -r '/^unset DBUS_/d' -i $out

    ${if !pureXauthority then "" else ''
      # Keep `.Xauthority` in `/run/user/`, separate for each xserver:
      sed -r '/^enable_xauth=/a export XAUTHORITY=$XDG_RUNTIME_DIR/.Xauthority.$$' -i $out
    ''}
  '';

  xserverArgs = let
    cfg = standardSystem.config.services.xserver;
  in [
    "-config" xorgConf
    "-xkbdir" xkbDir
    "-logfile" "/dev/null"
    "-logverbose" cfg.verbose
    "-nolisten" "tcp"
    "-novtswitch"
  ]
  ++ (if cfg.dpi != null then [ "-dpi" cfg.dpi ] else [])
  ++ extraXserverArgs;

  # We have to change not only PATH, but also other profile variables:
  transientProfile = buildEnv {
    name = "x11-rootless-profile";
    paths = essentialPackages ++ extraPackages;
    inherit (standardSystem.config.environment) pathsToLink extraOutputsToInstall;
    inherit (standardSystem.config.system.path) ignoreCollisions;
  };

  # Based on <https://github.com/NixOS/nixpkgs/blob/0c3a28f08f2/nixos/modules/config/shells-environment.nix#L12-L28>,
  # but with added checking for duplicates:
  exportProfile = profile: let
    profileRelativeEnvVars' =
      if profileRelativeEnvVars != null
      then profileRelativeEnvVars
      else standardSystem.config.environment.profileRelativeEnvVars;
    suffixedVariables = lib.flip lib.mapAttrs profileRelativeEnvVars' (_: listSuffixes:
      map (suffix: "${profile}${suffix}") listSuffixes
    );
    exportVariables = lib.concatStringsSep "\n" (lib.mapAttrsToList (n: vs:
      lib.concatMapStringsSep "\n" (v: ''
        if [[ ":''$${n}:" != *:${lib.escapeShellArg v}:* ]]; then
          export ${n}="''$${n}":${lib.escapeShellArg v}
        fi
      '') vs) suffixedVariables);
  in writeShellScript "export-${profile.name}" ''
    ${exportVariables}
    if [[ " $NIX_PROFILES " != *${lib.escapeShellArg (" " + profile + " ")}* ]]; then
      export NIX_PROFILES=${lib.escapeShellArg profile}" $NIX_PROFILES"
    fi
  '';

  runStartx = writeShellScript "run-startx-in-dbus" ''
    set -euo pipefail
    . ${exportProfile transientProfile}
    exec ${dbus}/bin/dbus-launch --exit-with-session \
      ${systemd}/bin/systemd-cat -t ${lib.escapeShellArg journaldIdentifier} \
      ${writeShellScript "run-startx" ''
        set -euo pipefail
        exec ${pureStartx} \
          ${if loadXresources == null
            then lib.escapeShellArg windowManager
            else writeShellScript "window-manager-with-Xresources" ''
              set -euo pipefail
              ${xorg.xrdb}/bin/xrdb <${lib.escapeShellArg loadXresources}
              exec ${lib.escapeShellArg windowManager}
            ''} \
          -- \
          ${lib.escapeShellArgs xserverArgs}
        ''}
  '';

in runStartx // { meta.platforms = lib.platforms.linux; }
