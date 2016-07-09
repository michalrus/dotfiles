{ config, lib, pkgs, ... }:

with lib;

let

  # Prosody already starts as `prosody` user, so we’ll have to copy
  # certs to a location readable by Prosody.
  certsDir = "/var/lib/prosody/certs";
  certsToBeUsed = [ "michalrus.com" ];

in

mkMerge (

  (map (cert: {
    security.acme.certs."${cert}".postRun = "systemctl restart prosody.service";
  }) certsToBeUsed)

  ++

  [{

    networking.firewall.allowedTCPPorts = [
      #5222  # XMPP client-to-server — not used remotely, only in local bitlbee
      5269   # XMPP server-to-server
    ];

    services = {

      prosody = {
        enable = true;

        admins = [ "m@michalrus.com" ];

        virtualHosts = builtins.listToAttrs (map (vhost: nameValuePair vhost {
          domain = vhost;
          enabled = true;
          ssl = {
            cert = "${certsDir}/${vhost}/fullchain.pem";
            key  = "${certsDir}/${vhost}/key.pem";
          };
        }) certsToBeUsed);

        extraConfig = ''
          c2s_require_encryption = true
          s2s_secure_auth = true
          ${concatMapStrings (vhost: ''
            Component "conference.${vhost}" "muc"
          '') certsToBeUsed}
        '';
      };

    };

    systemd.services.prosody.serviceConfig.PermissionsStartOnly = true;

    systemd.services.prosody.preStart = ''
      # A hack to restart prosody.service when config changes:
      # ${builtins.toFile "prosody.conf" config.environment.etc."prosody/prosody.cfg.lua".text}

      # Copy the certs to a location readable by Prosody…
      ${concatMapStrings (cert: ''
        mkdir -p "${certsDir}/${cert}"
        cp "${config.security.acme.directory}/${cert}/fullchain.pem" "${certsDir}/${cert}"
        cp "${config.security.acme.directory}/${cert}/key.pem"       "${certsDir}/${cert}"
      '') certsToBeUsed}

      # … and make it readable.
      chown -R prosody:prosody "${certsDir}"
      chmod -R o= "${certsDir}"
    '';

  }]

)
