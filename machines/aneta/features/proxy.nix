{
  config,
  pkgs,
  lib,
  ...
}:
# A HTTP and SOCKS5 proxy to bypass VPN.
with import ./common.nix; let
  portHttp = 8888;
  portSocks5 = 1080;
  userHttp = "proxyhttp";
  userSocks5 = "proxysocks5";

  microsocks = pkgs.callPackage (
    {
      stdenv,
      fetchFromGitHub,
    }:
      stdenv.mkDerivation rec {
        pname = "microsocks";
        version = "1.0.2";
        #name = "${pname}-${pver}";
        src = fetchFromGitHub {
          repo = "microsocks";
          owner = "rofl0r";
          rev = "v${version}";
          sha256 = "1di11wx9ihwa0g9qzsqrb3ka2xxjb10fyc7hwjhn58mxdfwlavl0";
        };
        makeFlags = ["prefix=$(out)"];
      }
  ) {};

  # We need 1.11.0-rc1 for ProtectSystem=strict <https://github.com/tinyproxy/tinyproxy/issues/353>
  tinyproxy = pkgs.tinyproxy.overrideAttrs (old: rec {
    version = "1.11.0-rc1";
    src = pkgs.fetchFromGitHub {
      sha256 = "0q1lrr6pbvxc5bfrvs8663kz7pfmnyhy7cmrh11wzllsakvlj4ly";
      rev = version;
      repo = "tinyproxy";
      owner = "tinyproxy";
    };
    configureFlags = (old.configureFlags or []) ++ ["--disable-manpage-support"];
  });

  tinyproxyConf = pkgs.writeText "tinyproxy.conf" ''
    User ${userHttp}
    Group ${userHttp}
    Port ${toString portHttp}
    #Bind 100.118.180.230
    Timeout 600
    Syslog On
    LogLevel Warning
    XTinyproxy No
    MaxClients 100
    MinSpareServers 5
    MaxSpareServers 20
    StartServers 10
    MaxRequestsPerChild 0
    Allow ${addressing.subnet}
    DisableViaHeader Yes
  '';
in {
  #networking.firewall.enable = lib.mkForce false;

  networking.firewall.logRefusedConnections = true;
  networking.firewall.logRefusedPackets = true;

  # Z ==false działa! Przynajmniej curl --interface wwan0
  networking.firewall.checkReversePath = false;
  # natomiast 2nd routing table nadal nie działa z poniższymi odkomentowanymi:

  # TODO: everywhere: sysctl -ar 'rp_filter' = 0

  # Make them accessible only from internal subnet:
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -p tcp -s ${addressing.subnet} --dport ${toString portHttp}   -j nixos-fw-accept
    iptables -A nixos-fw -p tcp -s ${addressing.subnet} --dport ${toString portSocks5} -j nixos-fw-accept
  '';

  # Add the 2nd routing table, bypassing VPN:

  # networking.iproute2.enable = true;
  # networking.iproute2.rttablesExtraConfig = ''
  #  1   novpn
  # '';

  # networking.networkmanager.dispatcherScripts = [
  #   {
  #     source = pkgs.writeText "up-hook-rt-novpn" ''
  #       logger "up-hook-rt-novpn: called with argv =" "$@"
  #       if [[ "$1" != "tun"* ]]; then

  #         if [[ "$2" = "up" ]]; then
  #           ip route show | grep -vF 'dev tun' | while IFS= read -r route ; do
  #             cmd="ip route add $route table novpn"
  #             logger "up-hook-rt-novpn: $cmd"
  #             $cmd
  #           done

  #           cmd="ip rule add fwmark 500 lookup novpn"
  #           logger "up-hook-rt-novpn: $cmd"
  #           $cmd
  #         fi

  #         if [[ "$2" = "down" ]]; then
  #           cmd="ip route flush table novpn"
  #           logger "up-hook-rt-novpn: $cmd"
  #           $cmd
  #         fi
  #       fi
  #     '';
  #     type = "basic";
  #   }
  # ];

  # TODO: make them use the 2nd routing table

  # iptables -t mangle -A OUTPUT -m owner --uid-owner proxy-http -j MARK --set-mark 500

  systemd.services.proxy-socks = {
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      User = userSocks5;
      Group = userSocks5;
      Restart = "always";
      ProtectSystem = "strict";
      ProtectHome = true;
      ExecStart = "${microsocks}/bin/microsocks";
      MemoryMax = "10M";
      TasksMax = 100;
    };
  };

  systemd.services.proxy-http = {
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      User = userHttp;
      Group = userHttp;
      Restart = "always";
      ProtectSystem = "strict";
      ProtectHome = true;
      ExecStart = "${tinyproxy}/bin/tinyproxy -d -c ${tinyproxyConf}";
      MemoryMax = "50M";
      TasksMax = 100;
    };
  };

  users.extraUsers."${userHttp}" = {
    group = userHttp;
    home = "/var/empty";
    shell = "/run/current-system/sw/bin/nologin";
    isSystemUser = true;
  };
  users.extraGroups."${userHttp}" = {};
  users.extraUsers."${userSocks5}" = {
    group = userSocks5;
    home = "/var/empty";
    shell = "/run/current-system/sw/bin/nologin";
    isSystemUser = true;
  };
  users.extraGroups."${userSocks5}" = {};
}
