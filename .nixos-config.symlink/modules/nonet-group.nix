{ config, lib, pkgs, ... }:

with lib;

let
  groupName = "nonet";
  cfg = config.networking.firewall.nonetGroup;
  rule  = "OUTPUT -m owner --gid-owner ${groupName} -j REJECT --reject-with icmp-port-unreachable";
  rule6 = "OUTPUT -m owner --gid-owner ${groupName} -j REJECT --reject-with icmp6-port-unreachable";
in

{
  options = {
    networking.firewall.nonetGroup = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkMerge [

    {
      # Clean-up in case someone turns this off. Also, without this,
      # when reloading the `firewall.service`, the exact same rules
      # would be added every time.
      networking.firewall.extraCommands = ''
        # "${groupName}" group: flush the old rules.
        iptables  -D ${rule}  2>/dev/null || true
        ip6tables -D ${rule6} 2>/dev/null || true
      '';
    }

    (mkIf cfg.enable {
      networking.firewall = {
        enable = true;
        extraCommands = ''
          # "${groupName}" group: the rules.
          iptables  -A ${rule}
          ip6tables -A ${rule6}
          '';
      };

      users.extraGroups."${groupName}" = {};
    })

  ];
}
