{ config, pkgs, lib, ... }:

with lib;

let

  inherit (import ../../michalrus.com/openvpn/common.nix { inherit lib; })
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

    # https://github.com/NixOS/nixpkgs/issues/34026 o_O
    networking.localCommands = ''
      # why doesn’t this work → `ip addr add broadcast ${subnet-chwalecice}.255 dev wlan0` ?
      ${pkgs.inetutils}/bin/ifconfig wlan0 broadcast ${subnet-chwalecice}.255
    '';

    services.hostapd = {
      enable = true;
      interface = "wlan0";
      ssid = "dome";

      wpa = false; # We want to enable WPA2-PSK only, and this does WPA1. :/
      extraConfig = ''
        wpa=2
        wpa_passphrase=${import ./router-chwalecice--wifi-password.nix}
        wpa_key_mgmt=WPA-PSK
        # Cipher for WPA2: allow only AES, no TKIP:
        rsn_pairwise=CCMP
      '';
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
        { hostName = "printer";          ethernetAddress = "f4:81:39:86:73:cb"; ipAddress = "${subnet-chwalecice}.5";  }
        { hostName = "camera-kuchnia";   ethernetAddress = "48:02:2a:40:d6:99"; ipAddress = "${subnet-chwalecice}.12"; }
        { hostName = "camera-sypialnia"; ethernetAddress = "0e:f2:b3:dc:52:a8"; ipAddress = "${subnet-chwalecice}.13"; }
        { hostName = "camera-salon";     ethernetAddress = "e8:ab:fa:87:9a:89"; ipAddress = "${subnet-chwalecice}.14"; }
      ];
    };
  }

]
