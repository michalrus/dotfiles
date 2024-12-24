{ config, pkgs, lib, ... }:

let
  iface = "wg-michalrus";
  serverExternalIP = "54.229.14.161";
  serverInternalIP = "10.77.5.1";
  clientInternalIP = "10.77.5.11";
in

{
  age.secrets.wireguard_monstrum = {
    file = ../../../../secrets/wireguard_monstrum.age;
  };

  systemd.services."wireguard-michalrus" = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network-pre.target" ];
    wants = [ "network.target" ];
    before = [ "network.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 10;
    };
    unitConfig = {
      StartLimitIntervalSec = 0; # no restart rate limiting
    };
    path = with pkgs; [ kmod iproute2 wireguard-tools iputils ];
    script = ''
      set -euo pipefail

      modprobe wireguard || true
      ip link add dev ${iface} type wireguard
      ip address add ${clientInternalIP}/24 dev ${iface}
      wg set ${iface} private-key ${config.age.secrets.wireguard_monstrum.path}

      # Use a lower MTU, because we often run WireGuard-inside-WireGuard, and
      # the default of 1420 breaks large packets like those of SSH or HTTPS:
      ip link set dev ${iface} mtu 1280

      ip link set up dev ${iface}

      wg set ${iface} \
        peer $(cat ${../../../../secrets/wireguard_michalrus_com.pub}) \
        endpoint ${serverExternalIP}:51820 \
        persistent-keepalive 15 \
        allowed-ips ${serverInternalIP}/32

      # And a watchdog:
      while true ; do
        sleep 10
        if ! ping -I ${iface} -c1 -W10 ${serverInternalIP} >/dev/null ; then
          echo "Failed to reach ${serverInternalIP} through ${iface}, restarting..."
          exit 1
        fi
      done
    '';
    postStop = ''
      set -euo pipefail
      ip link del dev ${iface}
    '';
  };
}
