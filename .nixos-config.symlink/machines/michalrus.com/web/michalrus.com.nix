{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix lib;

let

  domain = "michalrus.com";

in

mkMerge [

  (mkCert domain [
    "www.michalrus.com"
    "p.michalrus.com" "git.michalrus.com"
    "michalrus.pl" "www.michalrus.pl" "michalrus.eu" "www.michalrus.eu"
  ])

  {
    environment.systemPackages = with pkgs; [
      bindfs
    ];

    fileSystems."/var/www/${domain}" = {
      device = "/home/m/public_html";
      fsType = "fuse.bindfs";
      options = [ "ro" "force-user=nginx" "force-group=nginx" "perms=640:u+D:g+D" ];
    };

    services.nginx.httpConfig = ''
      server {
        listen 443;
        listen [::]:443;

        ssl on;
        ssl_certificate     ${config.security.acme.directory}/${domain}/fullchain.pem;
        ssl_certificate_key ${config.security.acme.directory}/${domain}/key.pem;

        server_name ${domain};

        access_log logs/${domain}.access;
        error_log logs/${domain}.error;

        location / {
          root /var/www/${domain};
        }
      }
    '';
  }

]
