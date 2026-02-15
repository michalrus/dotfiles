{
  config,
  lib,
  pkgs,
  ...
}:
with import ./common.nix; let
  standardForwarders = [
    # Cloudflare
    "1.1.1.1"
    "1.0.0.1"
    # Google
    "8.8.8.8"
    "8.8.4.4"
  ];

  forwardersConf = "/etc/bind/forwarders.conf";

  unsetCustomForwarders = ''
    cat <<'EOF' > ${forwardersConf}
    forwarders { ${lib.concatMapStrings (entry: " ${entry}; ") standardForwarders} };
    EOF
  '';
in {
  options = {
    services.bind.script.setCustomForwarders = lib.mkOption {
      type = lib.types.path;
      internal = true;
      readOnly = true;
      default = pkgs.writeScript "setCustomForwarders" ''
        #! ${pkgs.stdenv.shell}

        (
          echo 'forwarders {'
          for entry in "$@" ; do echo "  $entry ;" ; done
          echo '};'
        ) > ${forwardersConf}

        ${config.systemd.package}/bin/systemctl reload --no-block bind.service
      '';
    };

    services.bind.script.unsetCustomForwarders = lib.mkOption {
      type = lib.types.path;
      internal = true;
      readOnly = true;
      default = pkgs.writeScript "unsetCustomForwarders" ''
        #! ${pkgs.stdenv.shell}
        ${unsetCustomForwarders}
        ${config.systemd.package}/bin/systemctl reload --no-block bind.service
      '';
    };
  };

  config = {
    services.resolved.enable = false;

    services.nscd.enable = false;
    system.nssModules = lib.mkForce [];

    services.dnsmasq.enable = false;

    networking.resolvconf.useLocalResolver = true;

    networking.firewall.allowedUDPPorts = [53];

    services.bind = {
      enable = true;

      configFile = pkgs.writeText "named.conf" (
        ''
          include "/etc/bind/rndc.key";
          controls {
            inet 127.0.0.1 allow {localhost;} keys {"rndc-key";};
          };

          acl cachenetworks { 127.0.0.0/8; ${addressing.subnet}; };
          acl badnetworks   { };

          options {
            listen-on    { any; };
            listen-on-v6 { any; };
            allow-query  { cachenetworks; };
            blackhole    { badnetworks;   };

            directory "/run/named";
            pid-file  "/run/named/named.pid";

            forward only;
            include "${forwardersConf}";
          };
        ''
        + (let
          domain = "openproject.michalrus.com";
        in ''
          zone "${domain}" {
            type master;
            file "${pkgs.writeText "${domain}.zone" ''
            $TTL    604800
            @       IN      SOA     ns.${domain}. admin.michalrus.com. (2 604800 86400 2419200 604800)
            @       IN      NS      ns.${domain}.
            @       IN      A       10.77.2.11
            ns      IN      A       10.77.2.11
          ''}";
          };
        '')
      );
    };

    systemd.services.bind.preStart = lib.mkAfter ''
      ${unsetCustomForwarders}
    '';
  };
}
