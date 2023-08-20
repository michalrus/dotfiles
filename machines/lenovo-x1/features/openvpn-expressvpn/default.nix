{ config, lib, pkgs, ... }:

with lib;

let

  dataDir = "/var/lib/openvpn/expressvpn";

in

{

  networking = {

    # FIXME:
    networkmanager.dispatcherScripts = [
      {
        source = pkgs.writeText "upHook" ''
          logger "upHook: got called with argv = " "$@"
          if [[ "$2" = "up" && "$1" != "tun"* ]]; then
            if ${pkgs.curl}/bin/curl -sS https://www.expressvpn.com/what-is-my-ip | cat | grep -qF 'Your IP address is secured.' ; then
              logger "upHook: alread behind ExpressVPN, stopping the local service"
              systemctl stop --no-block openvpn-expressvpn.service
            else
              logger "upHook: will restart openvpn-expressvpn.service"
              systemctl restart --no-block openvpn-expressvpn.service
            fi
          fi
        '';
        type = "basic";
      }
    ];

    # # FIXME: use dnsmasq
    # extraHosts = ''
    #   212.7.217.17   poland-ca-version-2.expressnetw.com
    #   212.7.219.116  poland-ca-version-2.expressnetw.com
    #   212.7.222.132  poland-ca-version-2.expressnetw.com
    # '';

    networkmanager.dns = lib.mkForce "none";

    # nameservers = lib.mkForce [
    #   "127.0.0.1"
    # ];
  };

  services.dnsmasq = {
    # enable = true;
    # alwaysKeepRunning = true;
    # extraConfig = ''

    # '';
  };

  services.openvpn.servers = {
    expressvpn = {
      updateResolvConf = true;
      autoStart = false;
      config = ''
        dev tun
        fast-io
        persist-key
        persist-tun
        nobind

        pull
        pull-filter ignore ping-restart
        ping-exit 60
        ping 10
        connect-retry 5 5
        connect-retry-max 1

        config ${dataDir}/remote

        remote-random
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

        cert     ${dataDir}/cert
        key      ${dataDir}/key
        tls-auth ${dataDir}/tls_auth
        ca       ${dataDir}/ca
      '';
    };
  };

}
