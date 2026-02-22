{
  pkgs,
  lib,
  ...
}:
with import ./common.nix; let
  flushRules = ''

    iptables -w -D FORWARD -j nixos-fw-forward 2> /dev/null || true
    iptables -w -P FORWARD ACCEPT
    iptables -w -F nixos-fw-forward 2> /dev/null || true
    iptables -w -X nixos-fw-forward 2> /dev/null || true

    iptables -w -t nat -D POSTROUTING -j nixos-fw-nat-post 2> /dev/null || true
    iptables -w -t nat -F nixos-fw-nat-post 2> /dev/null || true
    iptables -w -t nat -X nixos-fw-nat-post 2> /dev/null || true

    iptables -w -t nat -D PREROUTING -j nixos-fw-nat-prerouting 2> /dev/null || true
    iptables -w -t nat -F nixos-fw-nat-prerouting 2> /dev/null || true
    iptables -w -t nat -X nixos-fw-nat-prerouting 2> /dev/null || true

  '';

  setupRules = ''

    iptables -w -N nixos-fw-forward
    iptables -w -A nixos-fw-forward -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    iptables -w -A nixos-fw-forward -i eth0 -j ACCEPT
    iptables -w -A nixos-fw-forward -j REJECT --reject-with icmp-host-unreachable
    iptables -w -A FORWARD -j nixos-fw-forward
    iptables -w -P FORWARD DROP

    iptables -w -t nat -N nixos-fw-nat-post
    iptables -w -t nat -A nixos-fw-nat-post -s ${addressing.subnet} -o tun0  -j MASQUERADE
    iptables -w -t nat -A nixos-fw-nat-post -s ${addressing.subnet} -o wlan0 -j MASQUERADE
    iptables -w -t nat -A nixos-fw-nat-post -s ${addressing.subnet} -o wwan0 -j MASQUERADE
    iptables -w -t nat -A POSTROUTING -j nixos-fw-nat-post

    iptables -w -t nat -N nixos-fw-nat-prerouting
    iptables -w -t nat -A nixos-fw-nat-prerouting -s ${addressing.subnet} -p tcp ! -d ${addressing.router} --dport 53 -j DNAT --to-destination ${addressing.router}:53
    iptables -w -t nat -A nixos-fw-nat-prerouting -s ${addressing.subnet} -p udp ! -d ${addressing.router} --dport 53 -j DNAT --to-destination ${addressing.router}:53
    iptables -w -t nat -A PREROUTING -j nixos-fw-nat-prerouting

  '';
in
  lib.mkMerge [
    {networking.firewall.extraCommands = lib.mkBefore flushRules;}

    {
      networking = {
        networkmanager = {
          enable = true;
          unmanaged = ["eth0"];
        };

        interfaces.eth0.ipv4.addresses = [
          {
            address = addressing.router;
            inherit (addressing) prefixLength;
          }
        ];

        firewall = {
          enable = true;
          extraCommands = setupRules;
          extraStopCommands = flushRules;
        };
      };

      hardware.usbWwan.enable = true;

      systemd.services.ModemManager = {
        wantedBy = ["NetworkManager.service"];
        partOf = ["NetworkManager.service"];
        after = ["NetworkManager.service"];
      };

      environment.systemPackages = with pkgs; [
        modemmanager # for `mmcli`
      ];

      boot.kernel.sysctl = {
        "net.ipv4.conf.all.forwarding" = lib.mkOverride 99 true;
        "net.ipv4.conf.default.forwarding" = lib.mkOverride 99 true;
      };

      services.dhcpd4 = {
        enable = true;
        interfaces = ["eth0"];
        extraConfig = ''
          option subnet-mask ${addressing.netmask};
          option broadcast-address ${addressing.broadcast};
          option routers ${addressing.router};
          option domain-name-servers ${addressing.router};
          subnet ${addressing.prefix} netmask ${addressing.netmask} {
            range ${addressing.dhcpRangeLow} ${addressing.dhcpRangeHigh};
          }
        '';
        machines = [
          {
            hostName = "rpi-krzysiek";
            ethernetAddress = "dc:a6:32:78:ae:d8";
            ipAddress = addressing.rpiKrzysiek;
          }
          {
            hostName = "printer";
            ethernetAddress = "00:1a:4b:23:ba:6e";
            ipAddress = addressing.printer;
          }
          {
            hostName = "dell-home-server";
            ethernetAddress = "e4:70:b8:f7:4b:8f";
            ipAddress = addressing.dell-home-server;
          }
        ];
      };
    }
  ]
