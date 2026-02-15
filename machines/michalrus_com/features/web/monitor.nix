modArgs @ {
  flake,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with import ./common.nix modArgs; let
  domain = "monitor.michalrus.com";
in
  mkMerge [
    (mkCert domain [])

    {
      services.grafana = {
        enable = true;
        domain = domain;
        rootUrl = "https://${domain}/";
      };

      services.nginx.httpConfig = sslServer {
        name = domain;
        body = ''
          location / {
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_pass http://${config.services.grafana.addr}:${toString config.services.grafana.port};
          }
        '';
      };
    }
  ]
