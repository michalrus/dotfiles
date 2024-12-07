{ config, pkgs, lib, ... }:

let iface = "wg0"; in

{
  age.secrets.wireguard_private_key = {
    file = ../../../../secrets/wireguard_dell-home-server.age;
  };

  networking.wireguard.interfaces = {
    "${iface}" = {
      ips = [ "10.77.5.11/24" ];
      privateKeyFile = config.age.secrets.wireguard_private_key.path;
      peers = [
        {
          publicKey = __readFile ../../../../secrets/wireguard_michalrus_com.pub;
          allowedIPs = [ "10.77.5.1/32" ];
          endpoint = "54.229.14.161:51820";
        }
      ];
    };
  };

  # For whatever reason, sometimes `dynamicEndpointRefreshSeconds` is not enough, and the link dies, so:
  systemd.services."wireguard-${iface}-watchdog" = {
    bindsTo = [ "wireguard-${iface}.service" ];
    after = [ "wireguard-${iface}.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.iputils ];
    script = ''
      while true ; do
        sleep 10
        if ! ping -I ${iface} -c1 -W10 10.77.5.1 >/dev/null ; then
          systemctl restart wireguard-${iface}.service
        fi
      done
    '';
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 5;
    };
  };
}
