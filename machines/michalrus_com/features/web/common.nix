{ flake, config, pkgs, ... }:

with pkgs.lib;

rec {

  acmeChallenges = "/var/www/acme-challenges";

  mkCert = primary: secondaries: {
    security.acme.certs."${primary}" = {
      webroot = acmeChallenges;
      email = "m@michalrus.com";
      postRun = "systemctl reload nginx.service";
      extraDomainNames = secondaries;
      group = "nginx";
    };
  };

  sslServer =
    { name
    , alternatives ? []
    , body
    , sslCert ? name
    , forcedCertDir ? null
    , setRealIPFromCloudflare ? false
    }:
    let
      certDir =
        if isNull forcedCertDir
        then "${config.security.acme.certs."${sslCert}".directory}"
        else forcedCertDir;
      readF = file:
        concatMapStrings (ip: "set_real_ip_from " + ip + ";\n")
          (filter (s: stringLength s > 0)
            (splitString "\n"
              (builtins.readFile file)));
      setRealIP =
        if setRealIPFromCloudflare then ''
          ${readF flake.inputs.cloudflare-ips-v4}
          ${readF flake.inputs.cloudflare-ips-v6}
          real_ip_header CF-Connecting-IP;
        '' else ''
          set_real_ip_from 127.0.0.1;
          real_ip_header proxy_protocol;
        '';
    in ''
      server {
        listen 8443 ssl proxy_protocol;
        listen [::]:8443 ssl proxy_protocol;

        ${setRealIP}

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
          listen 8443 ssl proxy_protocol;
          listen [::]:8443 ssl proxy_protocol;

          ${setRealIP}

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
