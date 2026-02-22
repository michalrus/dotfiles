{
  config,
  lib,
  ...
}:
with lib; let
  groupName = "nonet";
  cfg = config.networking.firewall.nonetGroup;
  rule = "OUTPUT -m owner --gid-owner ${groupName} -j REJECT --reject-with icmp-port-unreachable";
  rule6 = "OUTPUT -m owner --gid-owner ${groupName} -j REJECT --reject-with icmp6-port-unreachable";

  flushRules = ''
    # "${groupName}" group: flush the old rules.
    iptables  -w -D ${rule}  2>/dev/null || true
    ip6tables -w -D ${rule6} 2>/dev/null || true
  '';

  setupRules = ''
    # "${groupName}" group: the rules.
    iptables  -w -A ${rule}
    ip6tables -w -A ${rule6}
  '';
in {
  options = {
    networking.firewall.nonetGroup = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkMerge [
    {networking.firewall.extraCommands = mkBefore flushRules;}

    (mkIf cfg.enable {
      networking.firewall = {
        enable = true;
        extraCommands = setupRules;
        extraStopCommands = flushRules;
      };

      users.extraGroups."${groupName}" = {};
    })
  ];
}
