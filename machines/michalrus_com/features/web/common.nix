{ config, pkgs }:

with pkgs.lib;

rec {

  acmeChallenges = "/var/www/acme-challenges";

  mkCert = primary: secondaries: {
    security.acme.certs."${primary}" = {
      webroot = acmeChallenges;
      email = "m@michalrus.com";
      postRun = "systemctl reload nginx.service";
      extraDomains = builtins.listToAttrs (map (secondary: nameValuePair secondary null) secondaries);
      allowKeysForGroup = true;
      group = "nginx";
    };
  };

  sslServer = { name
              , alternatives ? []
              , body
              , sslCert ? name
              , forcedCertDir ? null
              }:
              let certDir = (if isNull forcedCertDir then "${config.security.acme.certs."${sslCert}".directory}" else forcedCertDir); in
  ''
    server {
      listen 443 ssl;
      listen [::]:443 ssl;

      ssl_certificate     ${certDir}/fullchain.pem;
      ssl_certificate_key ${certDir}/key.pem;

      server_name ${name};

      ${body}
    }

    server {
      listen 80;
      listen [::]:80;

      server_name ${name} ${toString alternatives};

      location /.well-known/acme-challenge {
        root ${acmeChallenges};
      }

      location / {
        return 301 https://${name}$request_uri;
      }
    }

    ${optionalString (alternatives != []) ''
      server {
        listen 443 ssl;
        listen [::]:443 ssl;

        ssl_certificate     ${certDir}/fullchain.pem;
        ssl_certificate_key ${certDir}/key.pem;

        server_name ${toString alternatives};

        location / {
          return 301 https://${name}$request_uri;
        }
      }
    ''}
  '';

  # garbage-collected on purpose
  cloudflare-ips-v4 = pkgs.fetchurl {
    url = "https://www.cloudflare.com/ips-v4";
    sha256 = "0xfd3xhnly2822sljynk56dphg6ybbc4dw8nd0bgzdv7hifrkn0p";
  };

  # garbage-collected on purpose
  cloudflare-ips-v6 = pkgs.fetchurl {
    url = "https://www.cloudflare.com/ips-v6";
    sha256 = "18dwrf6iw35vwpgz3mscmpmhpjf2ifyzpdgpigrd3b6gwjm18i17";
  };

  setRealIPFromCloudflare =
    let
      readF = file:
        concatMapStrings (ip: "set_real_ip_from " + ip + ";\n")
        (filter (s: stringLength s > 0)
        (splitString "\n"
        (builtins.readFile file)));
    in ''
      ${readF cloudflare-ips-v4}
      ${readF cloudflare-ips-v6}
      real_ip_header CF-Connecting-IP;
    '';

}
