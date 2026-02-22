{
  config,
  pkgs,
  ...
}: let
  acmeChallenges = "/var/www/acme-challenges";
in {
  networking.firewall.allowedTCPPorts = [80 443 8443];

  systemd.tmpfiles.rules = [
    "d ${acmeChallenges}/.well-known                0755 root root -"
    "d ${acmeChallenges}/.well-known/acme-challenge 0750 acme nginx -"
  ];

  security.acme.acceptTerms = true;

  security.acme.certs."${config.services.openproject.hostname}" = {
    webroot = acmeChallenges;
    email = "m@michalrus.com";
    postRun = "systemctl reload nginx.service";
    extraDomainNames = [];
    group = "nginx";
  };

  services = {
    nginx = {
      enable = true;

      package = pkgs.nginxMainline;

      httpConfig = ''
        # Redirect all HTTP requests to HTTPS.
        server {
          listen 80 default;
          listen [::]:80 default;
          server_name _;

          location /.well-known/acme-challenge {
            root ${acmeChallenges};
          }

          location / {
            return 301 https://$host$request_uri;
          }
        }

        server {
          listen 8443 ssl proxy_protocol;
          listen [::]:8443 ssl proxy_protocol;

          set_real_ip_from 127.0.0.1;
          set_real_ip_from 10.77.5.1;
          real_ip_header proxy_protocol;

          ssl_certificate     ${config.security.acme.certs.${config.services.openproject.hostname}.directory}/fullchain.pem;
          ssl_certificate_key ${config.security.acme.certs.${config.services.openproject.hostname}.directory}/key.pem;

          server_name ${config.services.openproject.hostname};
          client_max_body_size 64M;

          location / {
            proxy_redirect off;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP  $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto https;
            proxy_pass http://127.0.0.1:${toString config.services.openproject.port};
          }
        }
      '';

      streamConfig = ''
        server {
          listen 443;
          proxy_pass 127.0.0.1:8443;
          proxy_protocol on;
        }
      '';
    };
  };
}
