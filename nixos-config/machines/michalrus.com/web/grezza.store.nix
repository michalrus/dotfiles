{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

let

  domain = "grezza.store";
  alternatives = [ "www.${domain}" ];

in

mkMerge [

  (mkCert domain alternatives)

  {
    services.nginx.httpConfig = sslServer {
      name = domain;
      alternatives = alternatives ++ [ "*.grezza.store" ];
      body = ''
        root /var/www/${domain};
      '';
    };
  }

]
