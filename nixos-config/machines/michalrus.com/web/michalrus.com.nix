{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

let

  domain = "michalrus.com";
  alternatives = [ "www.${domain}" "michalrus.pl" "www.michalrus.pl" "michalrus.eu" "www.michalrus.eu" ];

in

mkMerge [

  (mkCert domain (alternatives ++ [ "p.${domain}" "git.${domain}" ]))

  {
    environment.systemPackages = with pkgs; [
      bindfs
    ];

    fileSystems."/var/www/${domain}" = {
      device = "/home/m/public_html";
      fsType = "fuse.bindfs";
      options = [ "ro" "force-user=${config.services.nginx.user}" "force-group=${config.services.nginx.group}" "perms=640:u+D:g+D" ];
    };

    services.nginx.httpConfig = sslServer {
      name = domain;
      alternatives = alternatives ++ [ "*.michalrus.com" "*.michalrus.pl" "*.michalrus.eu" ];
      body = ''
        root /var/www/${domain};
        expires epoch; # Cache creates more trouble in communication than it’s worth (for this site).

        location /protected/ {
          location ~ ^/protected/([^/]+)/\.htpasswd$ {
            return 403;
          }

          location ~ ^/protected/([^/]+)/ {
            autoindex on;
            autoindex_exact_size off;
            auth_basic "Speak, friend, and enter.";
            auth_basic_user_file "$document_root/protected/$1/.htpasswd";
          }
        }

        location /hidden/ {
          rewrite ^/hidden/(accounting(/.*)?)$ /protected/$1 permanent;
        }

        # TODO: maybe build /cv from source on GH?
        rewrite ^/cv$ /cv/en.pdf last;
        rewrite ^/cv/$ /cv permanent;
        rewrite ^/cv-pl$ /cv/pl.pdf last;
        rewrite ^/\d\d\d\d-\d\d-\d\d/cv\.pdf$ /cv/en.pdf last; # tracking ^.~
        location /cv/ {
          location ~ ^/cv/(.*)$ {
            add_header Content-Disposition "inline; filename=Michal_Rus_CV-$1";
          }
        }

        location = /avatar {
          alias ${../../../../dotfiles/michalrus/base/.config/avatar.jpg};
          add_header Content-Type "image/jpeg";
          add_header Content-Disposition "inline; filename=Michal_Rus.jpg";
        }

        location = /ssh {
          alias ${../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub};
          add_header Content-Type "text/plain; charset=utf-8";
          add_header Content-Disposition "inline; filename=Michal_Rus_ssh.pub";
        }

        location / {
          root ${./michalrus.com};

          rewrite ^/gpg$ /pgp permanent;

          try_files $uri.html $uri.txt $uri $uri/ =404;

          location = /index.html { }

          # Strip extensions only if that file actually exists in the immutable root.
          location ~ ^(.*)\.(html|txt)$ {
            if (-f $request_filename) {
              return 301 $scheme://$host$1$is_args$args;
            }
          }
        }

      '';
    };
  }

]
