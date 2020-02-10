{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.tor.torifiedUsers;

  transPort = 9040;
  dnsPort = 5353;
  # For torified users’ apps, that insist on using SOCKS5…
  allowedSocksPort = lib.toInt (lib.last (lib.splitString ":" config.services.tor.client.socksListenAddress));
  chain = "nixos-fw-torified-users";

  flushRules = ''
    for tbl in nat filter ; do
      ip46tables -w -t $tbl -D OUTPUT -j ${chain} 2> /dev/null || true
      ip46tables -w -t $tbl -F ${chain} 2> /dev/null || true
      ip46tables -w -t $tbl -X ${chain} 2> /dev/null || true
    done
  '';

  setupRules = ''

    ip46tables -w -t nat    -N ${chain}
    ip46tables -w -t filter -N ${chain}

  '' + (concatMapStringsSep "\n" (u: let user = u.username; localPorts = u.allowedLocalPorts ++ [allowedSocksPort]; in ''

    ${concatMapStringsSep "\n" (port: ''
      iptables -w -t nat -A ${chain} -d 127.0.0.0/8 -p tcp -m owner --uid-owner ${user} -m tcp --dport ${toString port} -j ACCEPT
    '') localPorts}

    # Redirect all of IPv4 TCP to TransProxy.
    iptables -w -t nat -A ${chain} -p tcp -m owner --uid-owner ${user} -m tcp -j REDIRECT --to-ports ${toString transPort}

    # Redirect all DNS queries to TransProxy.
    iptables -w -t nat -A ${chain} -p udp -m owner --uid-owner ${user} -m udp --dport 53 -j REDIRECT --to-ports ${toString dnsPort}

    # Unblock those redirection targets.
    ${concatMapStringsSep "\n" (port: ''
      iptables -w -t filter -A ${chain} -d 127.0.0.0/8 -p tcp -m owner --uid-owner ${user} -m tcp --dport ${toString port} -j ACCEPT
      iptables -w -t filter -A ${chain} -d 127.0.0.0/8 -p tcp -m owner --uid-owner ${user} -m tcp --sport ${toString port} -j ACCEPT
    '') localPorts}
    iptables -w -t filter -A ${chain} -d 127.0.0.0/8 -p tcp -m owner --uid-owner ${user} -m tcp --dport ${toString transPort} -j ACCEPT
    iptables -w -t filter -A ${chain} -d 127.0.0.0/8 -p udp -m owner --uid-owner ${user} -m udp --dport ${toString dnsPort} -j ACCEPT

    # Drop everything else.
    ip46tables -w -t filter -A ${chain} -m owner --uid-owner ${user} -j DROP

  '') cfg) + ''

    ip46tables -w -t nat    -A OUTPUT -j ${chain}
    ip46tables -w -t filter -A OUTPUT -j ${chain}

  '';

in

{

  options.services.tor.torifiedUsers = mkOption {
    type = types.listOf (types.submodule {
      options = {
        username = mkOption { type = types.str; };
        allowedLocalPorts = mkOption {
          type = types.listOf types.int;
          default = [];
          description = ''
            List of 127.0.0.1 TCP ports the user is allowed to
            access. A whitelist is needed, because those local
            services might process user input in clearnet.
          '';
        };
      };
    });
    default = [];
  };

  config = mkMerge [
    { networking.firewall.extraCommands = mkBefore flushRules; }

    (mkIf (cfg != []) {

      services.tor = {
        enable = true;
        client.enable = true;
        extraConfig = ''
          VirtualAddrNetworkIPv4 10.192.0.0/10
          AutomapHostsOnResolve 1
          TransPort ${toString transPort}
          DNSPort ${toString dnsPort}
        '';
      };

      # It’s important that the users issue NS queries themselves. In
      # other cases, this will leak deanonymizing DNS packets.
      services.resolved.enable = false;
      services.nscd.enable = false;

      networking.firewall = {
        enable = true;
        extraCommands = setupRules;
        extraStopCommands = flushRules;
      };

    })
  ];

}
