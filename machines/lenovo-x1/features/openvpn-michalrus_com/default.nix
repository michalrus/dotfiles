{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  dataDir = "/var/lib/openvpn/michalrus_com";
in {
  services.openvpn.servers = {
    michalrus_com = {
      config = ''
        client
        dev tun
        proto udp

        # Let’s try a raw IP address.
        remote 54.229.14.161 1194

        resolv-retry infinite
        nobind
        user nobody
        group nogroup
        persist-key
        persist-tun
        mute-replay-warnings
        ca ${../../../../machines/michalrus_com/features/openvpn/ca.crt}
        cert ${dataDir}/client.crt
        key ${dataDir}/client.key
        remote-cert-tls server
        tls-auth ${dataDir}/ta.key 1
        cipher AES-256-CBC
        verb 3
        mute 20
      '';
    };
  };

  #networking.extraHosts = (import ../../../../machines/michalrus_com/features/openvpn/common.nix { inherit lib; }).extraHosts;
}
