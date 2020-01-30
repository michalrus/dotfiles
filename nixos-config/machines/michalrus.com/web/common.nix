{ config, lib }:

with lib;

rec {

  acmeChallenges = "/var/www/acme-challenges";

  mkCert = primary: secondaries: {
    security.acme.certs."${primary}" = {
      webroot = acmeChallenges;
      email = "m@michalrus.com";
      postRun = "systemctl reload nginx.service";
      extraDomains = builtins.listToAttrs (map (secondary: nameValuePair secondary null) secondaries);
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
      listen 443;
      listen [::]:443;

      ssl on;
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

    ${lib.optionalString (alternatives != []) ''
      server {
        listen 443;
        listen [::]:443;

        ssl on;
        ssl_certificate     ${certDir}/fullchain.pem;
        ssl_certificate_key ${certDir}/key.pem;

        server_name ${toString alternatives};

        location / {
          return 301 https://${name}$request_uri;
        }
      }
    ''}
  '';

}
