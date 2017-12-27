{ config, pkgs, lib, ... }:

with lib;

let

  inherit (import ../michalrus.com/openvpn/common.nix { inherit lib; })
    subnet-chwalecice;

  flushRules = ''

    iptables -w -D FORWARD -j nixos-fw-forward 2> /dev/null || true
    iptables -w -P FORWARD ACCEPT
    iptables -w -F nixos-fw-forward 2> /dev/null || true
    iptables -w -X nixos-fw-forward 2> /dev/null || true

    iptables -w -t nat -D POSTROUTING -j nixos-fw-nat-post 2> /dev/null || true
    iptables -w -t nat -F nixos-fw-nat-post 2> /dev/null || true
    iptables -w -t nat -X nixos-fw-nat-post 2> /dev/null || true

  '';

  setupRules = ''

    iptables -w -N nixos-fw-forward
    iptables -w -A nixos-fw-forward -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    iptables -w -A nixos-fw-forward -i wlan0 -j ACCEPT
    iptables -w -A nixos-fw-forward -i tun0 -o wlan0 -j ACCEPT
    iptables -w -A nixos-fw-forward -j REJECT --reject-with icmp-host-unreachable
    iptables -w -A FORWARD -j nixos-fw-forward
    iptables -w -P FORWARD DROP

    iptables -w -t nat -N nixos-fw-nat-post
    iptables -w -t nat -A nixos-fw-nat-post -s ${subnet-chwalecice}.0/24 -o eth0 -j MASQUERADE
    iptables -w -t nat -A POSTROUTING -j nixos-fw-nat-post

  '';

in

mkMerge [

  { networking.firewall.extraCommands = mkBefore flushRules; }

  {
    networking.firewall = {
      enable = true;
      extraCommands = setupRules;
      extraStopCommands = flushRules;
    };

    boot.kernel.sysctl = {
      "net.ipv4.conf.all.forwarding"     = mkOverride 99 true;
      "net.ipv4.conf.default.forwarding" = mkOverride 99 true;
    };

    networking.interfaces.wlan0.ip4 = [{
      address = "${subnet-chwalecice}.1";
      prefixLength = 24;
    }];

    services.hostapd = {
      enable = true;
      interface = "wlan0";
      ssid = "dome";
      wpaPassphrase = import ./router-chwalecice--wifi-password.nix;
    };

    services.dhcpd4 = {
      enable = true;
      interfaces = ["wlan0"];
      extraConfig = ''
        option subnet-mask 255.255.255.0;
        option broadcast-address ${subnet-chwalecice}.255;
        option routers ${subnet-chwalecice}.1;
        option domain-name-servers 8.8.8.8, 8.8.4.4;
        subnet ${subnet-chwalecice}.0 netmask 255.255.255.0 {
          range ${subnet-chwalecice}.100 ${subnet-chwalecice}.250;
        }
      '';
      machines = [
        { hostName = "printer";
          ethernetAddress = "f4:81:39:86:73:cb";
          ipAddress = "${subnet-chwalecice}.5";
        }
      ];
    };
  }

]
