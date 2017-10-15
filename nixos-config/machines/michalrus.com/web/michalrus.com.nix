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

        if ($request_uri ~ ^(.*/)index(\.html)?(\?.*)?$ ) {
          return 301 $scheme://$host$1$3;
        }

        # TODO: maybe build /cv from source on GH?
        rewrite ^/cv/$ /cv permanent;
        rewrite ^/cv$ /cv/en.pdf last;
        rewrite ^/cv-pl$ /cv/pl.pdf last;
        rewrite ^/\d\d\d\d-\d\d-\d\d/cv\.pdf$ /cv/en.pdf last;
        location ~ ^/cv/(.*)$ {
          add_header Content-Disposition "inline; filename=Michal_Rus_CV-$1";
        }

        location /avatar {
          alias ${../../../../dotfiles/michalrus/base/.config/avatar.jpg};
          add_header Content-Type "image/jpeg";
          add_header Content-Disposition "inline; filename=Michal_Rus.jpg";
        }

        location /ssh {
          alias ${../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub};
          add_header Content-Type "text/plain; charset=utf-8";
          add_header Content-Disposition "inline; filename=Michal_Rus_ssh.pub";
        }

        location / {
          root ${./michalrus.com};

          try_files $uri.html $uri.txt $uri $uri/ @mutable;

          error_page 404 = @mutable;

          location ~ /index\.html$ { }

          # Strip extensions only if that file actually exists in the immutable root.
          location ~ ^(.*)\.(html|txt)$ {
            if (-f $request_filename) {
              return 301 $scheme://$host$1$is_args$args;
            }
          }
        }

        location /hidden/accounting/ {
          autoindex on;
          autoindex_exact_size off;
          auth_basic "Speak, friend, and enter.";
          auth_basic_user_file "${config.services.nginx.stateDir}/auth/${domain}/accounting";
          try_files $uri $uri/ =404;
        }

        location @mutable { }
      '';
    };
  }

]
