{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.screen;

  baseDef = isUserService: {
    description = "GNU Screen";
    serviceConfig = {
      Type = "forking";
      Restart = "always";
    };
    script = ''
      ${if isUserService then ''
        # Check if there isn’t already a system service running for this user.
        echo ${concatStringsSep " " cfg.usersAlways} | grep -qFvw $USER || {
          echo >&2 "Aborting. A system service exists for this user. Use the ‘screen-$USER.service’."
          exit 1
        }
      '' else ""}

      # For some reason, SHELL is not being set for root. *.*
      export SHELL=${config.users.defaultUserShell}

      source ${config.system.build.setEnvironment}
      cd "$HOME"
      exec "$SHELL" --login -c "exec ${pkgs.screen}/bin/screen -d -m"
    '';
  };

in

{
  options = {
    services.screen = {
      usersAlways = mkOption {
        type = types.listOf types.string;
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

    # User service (not started automatically).
    systemd.user.services.screen = baseDef true;

    # System services (started automatically only for configured users).
    systemd.services = mkMerge (map (user: {
      "screen-${user}" = recursiveUpdate (baseDef false) {
        serviceConfig.User = user;
        wantedBy = [ "multi-user.target" ];
      };
    }) cfg.usersAlways);
  };

}
