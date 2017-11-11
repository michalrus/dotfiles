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

  config = mkIf cfg.enable {

    users.extraUsers.guest = {
      hashedPassword = "";
      isNormalUser = true;
      home = cfg.home;
      uid = cfg.uid;
      description = "Guest";
      extraGroups = cfg.groups;
    };

    systemd.services."guest-account-cleanup" = let

      cleanup = pkgs.writeScript "guest-account-cleanup" ''
        #!${pkgs.stdenv.shell}

        exec ${pkgs.rsync}/bin/rsync \
          -a -H -A -X --delete --force \
          --usermap='*':guest --groupmap='*':users \
          --exclude=/.Xauthority \
          "${cfg.skeleton}/" "${cfg.home}/"
      '';

      slice = [ "user-${toString cfg.uid}.slice" ];

    in {
      wantedBy = slice;
      partOf = slice; # so that it’s killed after user logs out
      before = slice; # so that this service finishes before /etc/profile setup is run (prevents races)
      script = "exec sleep $((2 ** 31))";
      preStart = "exec ${cleanup}";
      postStop = "exec ${cleanup}";
    };

  };
}
