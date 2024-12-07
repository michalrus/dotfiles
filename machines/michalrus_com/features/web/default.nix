modArgs@{ flake, config, lib, pkgs, ... }:

with lib;
with import ./common.nix modArgs;

let
  dellDomain = "openproject.michalrus.com";
  dellIP = "10.77.5.11";
in

{
  imports = [
    ./michalrus.com.nix
    ./home.nix
    ./andrzej-lewandowski.com.nix
  ];

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  systemd.tmpfiles.rules = [
    "d ${acmeChallenges}/.well-known                0755 root root -"
    "d ${acmeChallenges}/.well-known/acme-challenge 0750 acme nginx -"
  ];

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
          listen 80;
          listen [::]:80;
          server_name ${dellDomain};
          location / {
            proxy_redirect off;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP  $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto https;
            proxy_pass http://${dellIP};
          }
        }
      '';

      streamConfig = ''
        map $ssl_preread_server_name $upstream {
          ${dellDomain} dell-home-server;
          default 127.0.0.1:8443;
        }

        upstream dell-home-server {
          server ${dellIP}:8443;
        }

        server {
          listen 443;
          proxy_pass $upstream;
          proxy_protocol on;
          ssl_preread on;
        }
      '';
    };
  };

}
