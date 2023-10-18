{ config, pkgs, lib, ... }:

{
  age.secrets.wireguard_private_key = {
    file = ../../../../secrets/wireguard_dell-home-server.age;
  };

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.77.3.11/24" ];
      privateKeyFile = config.age.secrets.wireguard_private_key.path;
      peers = [
        {
          publicKey = __readFile ../../../../secrets/wireguard_michalrus_com.pub;
          allowedIPs = [ "10.77.3.1/32" ];
          endpoint = "54.229.14.161:51820";
          persistentKeepalive = 25;
        }
      ];
    };
  };
}
