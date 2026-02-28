{package}: {
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.bwrap-escape-hatch;

  rulesFormat = lib.types.submodule {
    options = {
      note = lib.mkOption {
        type = lib.types.str;
        description = "Human-readable description of the rule.";
      };
      argv = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "Positional fnmatch patterns for the argv array.";
      };
    };
  };

  rulesFile = (pkgs.formats.json {}).generate "bwrap-escape-hatch-rules.json" cfg.rules;
in {
  options.services.bwrap-escape-hatch = {
    enable = lib.mkEnableOption "bwrap-escape-hatch sandbox escape service";

    rules = lib.mkOption {
      type = lib.types.listOf rulesFormat;
      default = [];
      example = lib.literalExpression ''
        [
          { note = "Desktop notifications (any title + body)"; argv = ["notify-send" "--" "*" "*"]; }
        ]
      '';
      description = "Allow-list rules for the escape hatch. Each rule matches an entire argv positionally using fnmatch patterns.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.sockets.bwrap-escape-hatch = {
      Unit.Description = "bwrap-escape-hatch sandbox escape socket";
      Socket = {
        ListenStream = "%t/bwrap-escape-hatch.sock";
        Accept = true;
        SocketMode = "0600";
      };
      Install.WantedBy = ["sockets.target"];
    };

    systemd.user.services."bwrap-escape-hatch@" = {
      Unit.Description = "bwrap-escape-hatch request handler";
      Service = {
        Type = "oneshot";
        StandardInput = "socket";
        StandardOutput = "socket";
        StandardError = "journal";
        ExecStart = "${lib.getExe package} --rules ${rulesFile}";
        TimeoutStartSec = 10;
        MemoryMax = "64M";
      };
    };
  };
}
