{
  config,
  lib,
  utils,
  pkgs,
  ...
}:
with lib; {
  options.services.sqlite-dump = mkOption {
    type = types.listOf (types.submodule {
      options = {
        source = mkOption {type = types.path;};
        destination = mkOption {type = types.path;};
        runAt = mkOption {type = types.str;};
        user = mkOption {
          type = types.str;
          default = "root";
        };
        group = mkOption {
          type = types.str;
          default = "root";
        };
      };
    });
    default = [];
  };

  config = let
    sname = cfg: "sqlite-dump-${utils.escapeSystemdPath cfg.source}";

    mkService = cfg:
      nameValuePair (sname cfg) {
        path = [pkgs.sqlite];
        script = ''
          dest=$(mktemp)
          chmod 600 $dest
          sqlite3 ${cfg.source} .dump >$dest

          sum1="$(cat ${cfg.destination} | md5sum)"
          sum2="$(cat $dest              | md5sum)"

          if [ "$sum1" != "$sum2" ]; then
            cat $dest >"${cfg.destination}"
            chown ${cfg.user}:${cfg.group} ${cfg.destination}
          fi
        '';
      };

    mkTimer = cfg:
      nameValuePair (sname cfg) {
        partOf = ["${sname cfg}.service"];
        wantedBy = ["timers.target"];
        timerConfig.OnCalendar = cfg.runAt;
      };
  in {
    systemd.services = listToAttrs (map mkService config.services.sqlite-dump);
    systemd.timers = listToAttrs (map mkTimer config.services.sqlite-dump);
  };
}
