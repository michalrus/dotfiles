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
        type = types.listOf types.str;
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

      #dep = [ "user-${toString cfg.uid}.slice" ];
      # After systemd-243 the user-.slice is not stopped after logout, so let’s depend on the user@.service.
      #   freenode/#systemd:
      #   11:34 <boucman> I'm not sure about the internal systemd logic, but it's possible the slice was kept alive because of the dependency
      dep = [ "user@${toString cfg.uid}.service" ];

    in {
      wantedBy = dep;
      partOf = dep; # so that it’s killed after user logs out
      before = dep; # so that this service finishes startup before /etc/profile setup is run (prevents races)
      script = "exec sleep $((2 ** 31))";
      preStart = "exec ${cleanup}";
      postStop = "exec ${cleanup}";
    };

  };
}
