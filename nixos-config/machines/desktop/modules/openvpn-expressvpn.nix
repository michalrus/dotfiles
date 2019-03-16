{ config, lib, pkgs, ... }:

with lib;

let

  dataDir = "/var/lib/openvpn/expressvpn";

in

{

  services.openvpn.servers = {
    expressvpn = {
      config = ''
        dev tun
        fast-io
        persist-key
        persist-tun
        nobind

        config ${dataDir}/remote

        remote-random
        pull
        comp-lzo no
        tls-client
        verify-x509-name Server name-prefix
        ns-cert-type server
        key-direction 1
        route-method exe
        route-delay 2
        tun-mtu 1500
        fragment 1300
        mssfix 1200
        verb 3
        cipher AES-256-CBC
        keysize 256
        auth SHA512
        sndbuf 524288
        rcvbuf 524288

        auth-user-pass ${dataDir}/auth-user-pass
        cert ${dataDir}/cert
        key ${dataDir}/key
        tls-auth ${dataDir}/tls_auth
        ca ${dataDir}/ca
      '';
    };
  };

}
