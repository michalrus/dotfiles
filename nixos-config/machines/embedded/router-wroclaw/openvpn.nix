{ config, lib, pkgs, ... }:

with lib;

let

  name = "expressvpn";

  dataDir = "/var/lib/openvpn/${name}";

  upScript = pkgs.writeScript "openvpn-up" ''
    #! ${pkgs.stdenv.shell}

    IF_DNS_NAMESERVERS=""

    for optionname in ''${!foreign_option_*} ; do
      option="''${!optionname}"
      echo $option
      part1=$(echo "$option" | cut -d " " -f 1)
      if [ "$part1" == "dhcp-option" ] ; then
        part2=$(echo "$option" | cut -d " " -f 2)
        part3=$(echo "$option" | cut -d " " -f 3)
        if [ "$part2" == "DNS" ] ; then
          IF_DNS_NAMESERVERS="$IF_DNS_NAMESERVERS $part3"
        fi
      fi
    done

    ${config.services.bind.script.setCustomForwarders} $IF_DNS_NAMESERVERS
  '';

in

{

  environment.systemPackages = [
    (pkgs.writeScriptBin "netflix-whitelist" ''
      #! ${pkgs.stdenv.shell}
      echo '#'
      echo '# OpenVPN config to bypass VPN when routing to Netflix servers.'
      echo '#'
      echo "# Generated at $(date -Ins)."
      echo '#'
      echo '# Re-generate contents of this file by running `netflix-whitelist`.'
      echo '#'
      echo
      ${pkgs.curl}/bin/curl https://ipinfo.io/AS2906 | \
        ${pkgs.gnugrep}/bin/grep -Po '\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/\d{1,3}' | \
        sort -u | \
        while IFS= read -r range ; do
          addr_mask=$(${pkgs.ipcalc}/bin/ipcalc -nb "$range" | ${pkgs.gnugrep}/bin/grep -P '^(Netmask|Address):' | ${pkgs.gawk}/bin/awk '{ print $2 }' | tr '\n' ' ')
          echo "route $addr_mask net_gateway"
        done
    '')
  ];

  networking = {

    # FIXME:
    networkmanager.dispatcherScripts = [
      {
        source = pkgs.writeText "upHook" ''
          logger "upHook: got called with argv = " "$@"
          if [[ "$2" = "up" && "$1" != "tun"* ]]; then
            logger "upHook: will restart openvpn-${name}.service"
            systemctl restart --no-block openvpn-${name}.service
          fi
        '';
        type = "basic";
      }
    ];

    networkmanager.dns = lib.mkForce "none";

  };

  systemd.services."openvpn-${name}" = {
    serviceConfig = {
      Restart = "always";
      RestartSec = 5;
    };

    postStop = ''
      ${config.services.bind.script.unsetCustomForwarders}
    '';
  };

  services.openvpn.servers."${name}" = {
    updateResolvConf = false;

    up = ''
      ${upScript}
    '';

    down = ''
      ${config.services.bind.script.unsetCustomForwarders}
    '';

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
      config ${dataDir}/netflix-whitelist.conf

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

      cert     ${dataDir}/client.crt
      key      ${dataDir}/client.key
      tls-auth ${dataDir}/ta.key
      ca       ${dataDir}/ca2.crt
    '';
  };

}

