modArgs@{ flake, config, lib, pkgs, ... }:

with lib;
with import ./common.nix modArgs;

let
  monstrumDomain1 = "openproject.michalrus.com";
  monstrumDomain2 = "torrents.michalrus.com";
  monstrumIP = "10.77.5.11";
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

      httpConfig = let
        readLines = file:
            filter (s: stringLength s > 0)
              (splitString "\n"
                (builtins.readFile file));
      in ''
        geo $proxy_protocol_addr $request_from_cloudflare {
          ${concatMapStrings (ip: ip + " 1;\n") (readLines flake.inputs.cloudflare-ips-v4)}
          ${concatMapStrings (ip: ip + " 1;\n") (readLines flake.inputs.cloudflare-ips-v6)}
          default 0;
        }

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
          server_name ${monstrumDomain1} ${monstrumDomain2};
          location / {
            proxy_redirect off;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP  $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto https;
            proxy_pass http://${monstrumIP};
          }
        }
      '';

      streamConfig = ''
        map $ssl_preread_server_name $upstream {
          ${monstrumDomain1} monstrum;
          ${monstrumDomain2} monstrum;
          default 127.0.0.1:8443;
        }

        upstream monstrum {
          server ${monstrumIP}:8443;
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
