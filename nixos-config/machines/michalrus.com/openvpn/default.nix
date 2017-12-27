{ config, lib, pkgs, ... }:

with lib;

with (import ./common.nix { inherit lib; });

let

  dataDir = "/var/lib/openvpn";

  ccdDir = pkgs.runCommand "ccd" {} ''
    mkdir -p $out
    ${concatStringsSep "\n" (mapAttrsToList (common: v: ''
      echo >$out/${common} \
        'ifconfig-push ${subnet}.${v.ip} 255.255.255.0
         ${v.extra or ""}'
    '') ccd)}
  '';

  ippFile = pkgs.writeText "ipp" ((concatStringsSep "\n" (mapAttrsToList (common: v: "${common},${subnet}.${v.ip}") ccd)) + "\n");

in

{

  networking.firewall.allowedUDPPorts = [ 1194 53 ];

  services.openvpn.servers = {
    server = {
      config = ''
        port 1194
        proto udp
        dev tun
        ca ${./ca.crt}
        cert ${./server.crt}
        key ${dataDir}/server.key
        dh ${./dh.pem}
        topology subnet
        server ${subnet}.0 255.255.255.0
        client-config-dir ${ccdDir}
        ifconfig-pool-persist ${ippFile}

              route ${subnet-chwalecice}.0 255.255.255.0 ${subnet}.${ccd.router-chwalecice.ip} 1200
        push "route ${subnet-chwalecice}.0 255.255.255.0 ${subnet}.${ccd.router-chwalecice.ip} 1200"

        client-to-client
        keepalive 10 60
        tls-auth ${dataDir}/ta.key 0
        cipher AES-256-CBC
        user nobody
        group nogroup
        persist-key
        persist-tun
        verb 3
        mute 20
        explicit-exit-notify 1
      '';
    };
  };

  networking.extraHosts = extraHosts;

}
