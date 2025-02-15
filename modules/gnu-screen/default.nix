{ config, lib, pkgs, ... }:

let

  cfg = config.services.gnu-screen;

  session_name = "7260c3b2-2e3d-4b22-8f8b-ab87de790446";

in

{
  options = {
    services.gnu-screen = {
      usersAlways = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "root" ];
        description = ''
          List of usernames that have a system service with GNU Screen
          running at all times, independent of whether the particular
          user is logged in.
        '';
      };
    };
  };

  config = {
    # Add screen to PATH.
    environment.systemPackages = [ pkgs.screen ];

    # System services (started automatically only for configured users).
    systemd.services = lib.mkMerge (map (user: {
      "gnu-screen-${user}" = {
        description = "GNU Screen (${user})";
        serviceConfig = {
          Type = "forking";
          Restart = "always";
        };
        script = ''
          # For some reason, SHELL is not being set for root. *.*
          export SHELL=${config.users.defaultUserShell}

          source ${config.system.build.setEnvironment}
          cd "$HOME"
          export XDG_RUNTIME_DIR=/run/user/$UID
          exec "$SHELL" --login -c "exec ${pkgs.screen}/bin/screen -S ${session_name} -d -m"
        '';
        serviceConfig.User = user;
        wantedBy = [ "multi-user.target" ];
      };
    }) cfg.usersAlways);
  };

}
