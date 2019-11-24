{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.logkeys';
in

{
  options = {
    services.logkeys' = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      keymap = mkOption {
        type = types.nullOr types.str;
        example = "pt_BR";
        default = null;
        description = "By default will use the built-in en_US keymap.";
      };
    };
  };

  config = mkIf cfg.enable {

    nixpkgs.overlays = [ (self: super: {
      logkeys = super.logkeys.overrideDerivation (oldAttrs: {
        postInstall = ''
          mkdir -p "$out"/share/logkeys/
          cp -r keymaps/. "$out"/share/logkeys/
          cp "${ ./pl.map }" "$out"/share/logkeys/pl.map
          '';
      });
    }) ];

    systemd.services.logkeys_ = {
      description = "Log all keys pressed on all keyboards";
      serviceConfig.Type = "forking";
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [ logkeys gawk ];
      script = ''
        # At boot, udev sometimes seems to run this too soon, short-circuit below.
        [ -e /dev/input/by-path ] || exit 0
        # For some reason /dev/input/by-path stopped having symlinks to keyboard. This is better.
        cat /proc/bus/input/devices | \
            awk 'tolower($0) ~ /name=.*keyboard/ { while(getline > 0) { if ($0 ~ /Handlers=/) { print; break; }}}' | \
            grep -o 'event[0-9]*' | \
            while IFS= read -r inp ; do
          logkeys --start --device="/dev/input/$inp" --output=/var/log/logkeys.log ${optionalString (cfg.keymap != null) "--keymap=\"${pkgs.logkeys}/share/logkeys/${cfg.keymap}.map\""}
          # why is the following not configurable?!
          rm /var/run/logkeys.pid || true
        done
        '';
    };

    services.udev.extraRules = ''
      # reload logkeys when a new USB keyboard is connected
      ACTION=="add", SUBSYSTEM=="hid", RUN+="${config.systemd.package}/bin/systemctl restart logkeys.service"
    '';

  };
}
