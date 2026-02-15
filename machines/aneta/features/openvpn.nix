{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  name = "nordvpn";

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
in {
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
      config ${dataDir}/config
      auth-user-pass ${dataDir}/auth-user-pass
    '';
  };
}
