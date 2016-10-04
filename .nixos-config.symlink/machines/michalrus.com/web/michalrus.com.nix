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
        root /var/www/${domain};
        expires epoch; # Cache creates more trouble (psychologically) than it’s worth in this domain.

        rewrite ^/gpg$ /pgp permanent;

        location /pgp {
          add_header Content-type "text/plain";
          add_header Content-Disposition "inline; filename=michalrus.pgp.pub.asc";
          expires epoch;
        }

        location /ssh {
          add_header Content-type "text/plain";
          add_header Content-Disposition "inline; filename=michalrus.ssh.pub.asc";
          expires epoch;
        }

        rewrite ^/cv/$ /cv permanent;
        rewrite ^/cv$ /cv/en.pdf last;
        rewrite ^/cv-pl$ /cv/pl.pdf last;

        location ~ ^/cv/(.*)$ {
          add_header Content-Disposition "inline; filename=Michal_Rus_CV-$1";
        }
      '';
    };
  }

]
