{config, ...}: let
  port = 51820;
in {
  networking.firewall.allowedUDPPorts = [port];

  age.secrets.wireguard_private_key = {
    file = ../../../../secrets/wireguard_michalrus_com.age;
  };

  networking.wireguard.interfaces = {
    wg0 = {
      ips = ["10.77.5.1/24"];
      listenPort = port;
      privateKeyFile = config.age.secrets.wireguard_private_key.path;
      peers = [
        {
          publicKey = builtins.readFile ../../../../secrets/wireguard_monstrum.pub;
          allowedIPs = ["10.77.5.11/32"];
        }
      ];
    };
  };
}
