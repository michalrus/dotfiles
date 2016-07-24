{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

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

    services.nginx.httpConfig = sslServer {
      name = domain;
      body = ''
        location / {
          root /var/www/${domain};
        }
      '';
    };
  }

]
