{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.logkeys;
in

{
  options = {
    services.logkeys = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      keymap = mkOption {
        type = types.string;
        example = "pt_BR";
        default = "";
        description = "By default will use the built-in en_US keymap.";
      };
    };
  };

  config = mkIf cfg.enable {

    nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
      logkeys = super.logkeys.overrideDerivation (oldAttrs: {
        postInstall = ''
          mkdir -p "$out"/share/logkeys/
          cp -r keymaps/. "$out"/share/logkeys/
          cp "${ ./pl.map }" "$out"/share/logkeys/pl.map
          '';
      });
    };

    systemd.services.logkeys = {
      description = "Log all keys pressed on all keyboards";
      serviceConfig.Type = "forking";
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [ logkeys ];
      script = ''
        ls /dev/input/by-path | grep kbd | while IFS= read -r inp ; do
          rinp="$(readlink -f "/dev/input/by-path/$inp")"
          logkeys --start --device="$rinp" --output=/var/log/logkeys.log ${optionalString (cfg.keymap != "") "--keymap=\"${pkgs.logkeys}/share/logkeys/${cfg.keymap}.map\""}
          # why is the following not configurable?!
          rm /var/run/logkeys.pid
        done
        '';
    };

    services.udev.extraRules = ''
      # reload logkeys when a new USB keyboard is connected
      ACTION=="add", SUBSYSTEM=="input", SUBSYSTEMS=="usb", ATTRS{authorized}=="1", RUN+="${config.systemd.package}/bin/systemctl restart logkeys.service"
      '';

  };
}
