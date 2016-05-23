{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.networking.firewall.nonetGroup;
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

  config = mkIf cfg.enable {

    networking.firewall = {
      enable = true;
      extraCommands = ''
        iptables  -A OUTPUT -m owner --gid-owner nonet -j REJECT --reject-with icmp-port-unreachable
        ip6tables -A OUTPUT -m owner --gid-owner nonet -j REJECT --reject-with icmp6-port-unreachable
        '';
    };

    users.extraGroups.nonet = {};

  };
}
