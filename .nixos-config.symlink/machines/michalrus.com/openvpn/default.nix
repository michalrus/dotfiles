{ config, lib, pkgs, ... }:

with lib;

let

  dataDir = "/var/lib/openvpn";

  subnet = "10.7.77";

  subnet-chwalecice = "10.7.74";

  ccd = {
    michalrus            = { ip = "10"; };
    elzbietarus          = { ip = "11"; };
    robertrus-acer       = { ip = "12"; };
    robertrus-np300e5a   = { ip = "13"; };
    mikolajrus           = { ip = "14"; };
    robertrus-asus-1225c = { ip = "15"; };
    router-chwalecice    = { ip = "40"; extra = "iroute ${subnet-chwalecice}.0 255.255.255.0"; };
  };

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

  networking.firewall.allowedUDPPorts = [ 1194 ];

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

              route ${subnet-chwalecice}.0 255.255.255.0 ${subnet}.${ccd.router-chwalecice.ip}
        push "route ${subnet-chwalecice}.0 255.255.255.0 ${subnet}.${ccd.router-chwalecice.ip}"

        push "dhcp-option DNS ${subnet}.1"

        client-to-client
        keepalive 10 60
        tls-auth ${dataDir}/ta.key 0
        cipher AES-256-CBC

        user nobody
        group nogroup
        persist-key
        persist-tun

        status ${dataDir}/openvpn-status.log
        verb 3
        mute 20

        explicit-exit-notify 1
      '';
    };
  };

}
