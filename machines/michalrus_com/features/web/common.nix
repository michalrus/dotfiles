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
      setRealIP =
        # XXX: we can’t have 2-stage real_ip_header (first proxy_protocol, and
        # then CF-Connecting-IP), so let’s 403 everything not coming from
        # Cloudflare, and then trust CF-Connecting-IP:
        if setRealIPFromCloudflare then ''
          if ($request_from_cloudflare = 0) {
            return 403 'Access denied. Only requests through Cloudflare are allowed.';
          }
          set_real_ip_from 127.0.0.1;
          real_ip_header CF-Connecting-IP;
        '' else ''
          set_real_ip_from 127.0.0.1;
          real_ip_header proxy_protocol;
        '';
    in ''
      server {
        listen 8443 ssl proxy_protocol;
        listen [::]:8443 ssl proxy_protocol;

        # It’s getting it wrong with :8443 and SSL streaming:
        port_in_redirect off;

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
