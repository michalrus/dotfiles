{ config, lib, pkgs, ... }:

let

  # Allows outgoing connections, ping, and incoming SSH, nothing else. So that
  # you won’t get locked out, but also won’t leak anything.
  reasonableEmptyRuleset = { ipv6 ? false }: pkgs.writeText "empty-ruleset" ''
    *filter
    :INPUT DROP
    :FORWARD DROP
    :OUTPUT ACCEPT
    -A INPUT -i lo -j ACCEPT
    -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    -A INPUT -p tcp --dport 22 -j ACCEPT
    ${if !ipv6 then ''
      -A INPUT -p icmp -m icmp --icmp-type 8 -j ACCEPT
    '' else ''
      -A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 137 -j DROP
      -A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 139 -j DROP
      -A INPUT -p ipv6-icmp -j ACCEPT
      -A INPUT -d fe80::/64 -p udp -m udp --dport 546 -j ACCEPT
    ''}
    COMMIT

    *mangle
    COMMIT

    *nat
    COMMIT

    *raw
    COMMIT

    *security
    COMMIT
  '';

  actualRulesetV4 = pkgs.writeText "actual-ruleset-v4" ''
    *filter
    :INPUT DROP
    :FORWARD DROP
    :OUTPUT ACCEPT
    -A INPUT -i lo -j ACCEPT
    -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    ${lib.concatMapStringsSep "\n" (port: ''-A INPUT -p tcp --dport ${toString port} -j ACCEPT'') config.networking.firewall.allowedTCPPorts}
    ${lib.concatMapStringsSep "\n" (port: ''-A INPUT -p tcp --dport ${toString port.from}:${toString port.to} -j ACCEPT'') config.networking.firewall.allowedTCPPortRanges}
    ${lib.concatMapStringsSep "\n" (port: ''-A INPUT -p udp --dport ${toString port} -j ACCEPT'') config.networking.firewall.allowedUDPPorts}
    ${lib.concatMapStringsSep "\n" (port: ''-A INPUT -p udp --dport ${toString port.from}:${toString port.to} -j ACCEPT'') config.networking.firewall.allowedUDPPortRanges}
    -A INPUT -s 10.77.2.0/24 -p tcp --dport 1080 -j ACCEPT
    # torrents:
    -A INPUT -i wg-airvpn -p tcp --dport 26090 -j ACCEPT
    -A INPUT -i wg-airvpn -p udp --dport 26090 -j ACCEPT
    # cardano-node:
    -A INPUT -i wg-airvpn -p tcp --dport 55709 -j ACCEPT
    -A INPUT -i wg-airvpn -p udp --dport 55709 -j ACCEPT
    # blockfrost-platform:
    -A INPUT -i wg-airvpn -p tcp --dport 18077 -j ACCEPT
    -A INPUT -p icmp -m icmp --icmp-type 8 -j ACCEPT
    -A INPUT -p tcp -m tcp --tcp-flags FIN,SYN,RST,ACK SYN -j LOG --log-prefix "refused connection: " --log-level 6
    -A INPUT -p tcp -m tcp ! --tcp-flags FIN,SYN,RST,ACK SYN -j REJECT --reject-with tcp-reset
    -A INPUT -j REJECT --reject-with icmp-port-unreachable
    -A FORWARD -s 10.77.2.0/24 -m conntrack --ctstate NEW,ESTABLISHED,RELATED -j ACCEPT
    -A FORWARD -d 10.77.2.0/24 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
    # Always allow all Wireguard packets:
    -A OUTPUT -m mark --mark 51820 -j ACCEPT
    # Prevent torrents from leaking our non-VPN IP — allow only AirVPN:
    -A OUTPUT -o wg-airvpn -m owner --uid-owner ${toString config.users.users.qbittorrent.uid} -j ACCEPT
    -A OUTPUT -o lo        -m owner --uid-owner ${toString config.users.users.qbittorrent.uid} -j ACCEPT
    -A OUTPUT              -m owner --uid-owner ${toString config.users.users.qbittorrent.uid} -j REJECT --reject-with icmp-port-unreachable
    # Prevent Cardano from leaking our non-VPN IP — allow only AirVPN:
    -A OUTPUT -o wg-airvpn -m owner --uid-owner ${toString config.users.users.cardano.uid} -j ACCEPT
    -A OUTPUT -o lo        -m owner --uid-owner ${toString config.users.users.cardano.uid} -j ACCEPT
    -A OUTPUT              -m owner --uid-owner ${toString config.users.users.cardano.uid} -j REJECT --reject-with icmp-port-unreachable
    # Prevent the `nonet` group from accessing anything:
    -A OUTPUT -m owner --gid-owner ${toString config.users.groups.nonet.gid} -j REJECT --reject-with icmp-port-unreachable
    COMMIT

    *mangle
    COMMIT

    *nat
    :PREROUTING ACCEPT
    :INPUT ACCEPT
    :OUTPUT ACCEPT
    :POSTROUTING ACCEPT
    -A PREROUTING -s 10.77.2.0/24 -p udp --dport 53 -j REDIRECT --to-port 53
    -A PREROUTING -s 10.77.2.0/24 -p tcp --dport 53 -j REDIRECT --to-port 53
    -A POSTROUTING -s 10.77.2.0/24 -j MASQUERADE
    COMMIT

    *raw
    COMMIT

    *security
    COMMIT
  '';

  actualRulesetV6 = pkgs.writeText "actual-ruleset-v6" ''
    *filter
    :INPUT DROP
    :FORWARD DROP
    :OUTPUT ACCEPT
    -A INPUT -i lo -j ACCEPT
    -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    -A INPUT -p tcp --dport 22 -j ACCEPT
    -A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 137 -j DROP
    -A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 139 -j DROP
    -A INPUT -p ipv6-icmp -j ACCEPT
    -A INPUT -d fe80::/64 -p udp -m udp --dport 546 -j ACCEPT
    # Prevent the `nonet` group from accessing anything:
    -A OUTPUT -m owner --gid-owner ${toString config.users.groups.nonet.gid} -j REJECT --reject-with icmp6-port-unreachable
    COMMIT

    *mangle
    COMMIT

    *nat
    COMMIT

    *raw
    COMMIT

    *security
    COMMIT
  '';

in {
  networking.firewall.enable = lib.mkForce false;
  networking.nftables.enable = lib.mkForce false;

  environment.systemPackages = with pkgs; [ iptables ];

  # For NAT:
  boot = {
    kernelModules = [ "nf_nat_ftp" ];
    kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = true;
      "net.ipv4.conf.default.forwarding" = true;
    };
  };

  # XXX: we still need to use iptables instead of nftables because of Docker and
  # libvirt, but let’s at least make them atomic.
  systemd.services.firewall-atomic = {
    description = "Firewall (atomic, using iptables-restore)";
    wantedBy = [ "sysinit.target" ];
    wants = [ "network-pre.target" ];
    after = [ "systemd-modules-load.service" ];
    before = [
      "network-pre.target"
      "shutdown.target"
    ];
    conflicts = [ "shutdown.target" ];

    path = with pkgs; [ iptables ];

    unitConfig.ConditionCapability = "CAP_NET_ADMIN";
    unitConfig.DefaultDependencies = false;

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      set -euo pipefail

      # Keep IPv6 on defaults:
      ip6tables-restore <${actualRulesetV6}
      iptables-restore <${actualRulesetV4}
    '';

    postStop = ''
      set -euo pipefail
      iptables-restore <${reasonableEmptyRuleset { ipv6 = false; }}
      ip6tables-restore <${reasonableEmptyRuleset { ipv6 = true; }}
    '';
  };

  users.groups.nonet.gid = 2122;
}
