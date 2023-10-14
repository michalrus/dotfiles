modArgs@{ flake, config, lib, pkgs, ... }:

with lib;
with import ./common.nix modArgs;

let

  domain = "openproject.michalrus.com";
  alternatives = [ "www.${domain}" ];

in

mkMerge [

  (mkCert domain alternatives)

  {
    services.openproject.hostname = domain;
    services.openproject.https = true;

    services.nginx.httpConfig = sslServer {
      name = domain;
      inherit alternatives;
      body = ''
        location / {
          proxy_redirect off;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP  $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto https;
          proxy_pass http://127.0.0.1:${toString config.services.openproject.port};
        }
      '';
    };
  }

]
