{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.users.guestAccount;
  emptyDir = pkgs.runCommand "empty-directory" {preferLocalBuild = true;} "mkdir -p $out";
in {
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
      inherit (cfg) home uid;
      description = "Guest";
      extraGroups = cfg.groups;
    };

    systemd.services."guest-account-cleanup" = let
      cleanup = pkgs.writeScript "guest-account-cleanup" ''
        #!${pkgs.stdenv.shell}

        ${pkgs.rsync}/bin/rsync \
          -a -H -A -X --delete --force \
          --usermap='*':guest --groupmap='*':users \
          --exclude=/.Xauthority \
          "${cfg.skeleton}/" "${cfg.home}/"

        chmod 700 "${cfg.home}/"

        # Set up ‘home-manager’, if available:
        systemctl restart home-manager-guest.service || true
      '';

      #dep = [ "user-${toString cfg.uid}.slice" ];
      # After systemd-243 the user-.slice is not stopped after logout, so let’s depend on the user@.service.
      #   freenode/#systemd:
      #   11:34 <boucman> I'm not sure about the internal systemd logic, but it's possible the slice was kept alive because of the dependency
      dep = ["user@${toString cfg.uid}.service"];
    in {
      wantedBy = dep;
      partOf = dep; # so that it’s killed after user logs out
      before = dep; # so that this service finishes startup before /etc/profile setup is run (prevents races)
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${cleanup}";
        ExecStop = "${cleanup}";
      };
    };
  };
}
