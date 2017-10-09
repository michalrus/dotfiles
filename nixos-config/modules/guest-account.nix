{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.users.guestAccount;
  emptyDir = pkgs.runCommand "empty-directory" {} "mkdir -p $out";
in

{
  options = {
    users.guestAccount = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      skeleton = mkOption {
        type = types.path;
        example = "/home/guest.skel";
        default = emptyDir;
        description = ''
          On each login `~guest/` will be deleted and recreated from this directory.
          By default, will result in an empty `~guest/`.
          '';
      };
      home = mkOption {
        type = types.path;
        default = "/home/guest";
      };
      uid = mkOption {
        type = types.int;
        default = 31338;
      };
      groups = mkOption {
        type = types.listOf types.string;
        default = [];
      };
    };
  };

  config = mkIf cfg.enable (let

    cleanUp = pkgs.writeScript "guest-account-cleanup" ''
      #!${pkgs.stdenv.shell}

      exec ${pkgs.rsync}/bin/rsync \
        -a -H -A -X --delete --force \
        --usermap='*':guest --groupmap='*':users \
        --exclude=/.Xauthority \
        "${cfg.skeleton}/" "${cfg.home}/"
      '';

  in {

    users.extraUsers.guest = {
      hashedPassword = "";
      isNormalUser = true;
      home = cfg.home;
      uid = cfg.uid;
      description = "Guest";
      extraGroups = cfg.groups;
    };

    services.xserver.displayManager.sessionCommands = ''
      if [ "$UID" = "${toString cfg.uid}" ] ; then
        ${config.security.wrapperDir}/sudo -n ${cleanUp} || exit 1
      fi
      '';

    security.sudo = {
      enable = true;
      extraConfig = ''
        guest ALL = (root) NOPASSWD: ${cleanUp}
        '';
    };

  });
}
