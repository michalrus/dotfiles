{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config pkgs; };

let

  domain = "andrzej-lewandowski.com";
  altDomain = "andrzej-lewandowski.pl";

  webhookPort = 9974;
  user = lib.replaceStrings ["." "-"] ["_" "_"] domain;
  homeDir = "/var/www/${domain}";

in

{

  services.nginx.httpConfig = lib.concatStringsSep "\n" [

    (sslServer {
      name = domain;
      alternatives = [ "www.${domain}" "*.${domain}" ];
      forcedCertDir = "/var/lib/cloudflare/${domain}";
      # generate httpd password hashes with `openssl passwd -apr1`
      body = ''
        ${setRealIPFromCloudflare}
        root /var/www/${domain}/release/public;
        error_page 404 /pl/404.html;
        location = / { return 301 https://${domain}/pl/; }

        location = /webhook {
          auth_basic "Speak, friend, and enter.";
          auth_basic_user_file "${pkgs.writeText "htpasswd" ''
            github:$apr1$3ybX.s4D$1ThOk.sgr0ASzH.2YxSBo1
          ''}";
          proxy_pass http://127.0.0.1:${toString webhookPort};
          expires epoch;
        }
      '';
    })

    (sslServer {
      name = "dev.${domain}";
      forcedCertDir = "/var/lib/cloudflare/${domain}";
      body = ''
        ${setRealIPFromCloudflare}
        auth_basic "Speak, friend, and enter.";
        auth_basic_user_file "${pkgs.writeText "htpasswd" ''
          michalrus:$apr1$Q67KvdAC$tUSCO.hwI5nCOtmsbkjyX/
          Krzyś:$apr1$.UBqHkMw$iqeyhOiJ2Q2YHKeoVTMQu.
          mikolaj:$apr1$tVowKZpr$Nul4ytYxnMYvNESnBwb2L1
          slupska:$apr1$Kg/RRn6V$IV.hUtFMXsi48wAtHh4CL.
          Rndl:$apr1$3lFkYcib$rL3F9IhikosYLZM8gqjry1
        ''}";
        root /var/www/${domain}/master/public;
        error_page 404 /pl/404.html;
        location = / { return 301 https://dev.${domain}/pl/; }
        if_modified_since off;
        add_header Last-Modified "";
        expires epoch;
      '';
    })

    (sslServer {
      name = "${altDomain}";
      forcedCertDir = "/var/lib/cloudflare/${altDomain}";
      body = ''
        ${setRealIPFromCloudflare}
        return 301 https://${domain}$request_uri;
      '';
    })

    (sslServer {
      name = "dev.${altDomain}";
      forcedCertDir = "/var/lib/cloudflare/${altDomain}";
      body = ''
        ${setRealIPFromCloudflare}
        return 301 https://dev.${domain}$request_uri;
      '';
    })

  ];

  #
  # Rebuild GitHub webhook:
  #

  systemd.sockets."webhook-${user}" = {
    partOf = [ "webhook-${user}@.service" ];
    wantedBy = [ "sockets.target" ];
    socketConfig = {
      ListenStream = toString webhookPort;
      Accept = "yes";
    };
  };

  systemd.services."webhook-${user}@" = {
    requires = [ "webhook-${user}.socket" ];
    serviceConfig = {
      Type = "oneshot";
      StandardInput = "null";   # don’t ever change that, to avoid shell injection!
      StandardOutput = "socket";
    };
    script = ''
      systemctl start --no-block 'rebuild-${user}.service'

      printf 'HTTP/1.1 200 OK\r\n'
      printf 'Content-Type: text/plain; charset=utf-8\r\n'
      printf 'Content-Length: 3\r\n'
      printf 'Connection: close\r\n'
      printf '\r\n'
      printf 'ok\n'
    '';
  };

  users.extraUsers."${user}"  = { isSystemUser = true; home = homeDir; };
  users.extraGroups."${user}" = { };
  systemd.tmpfiles.rules = [ "d '${homeDir}' 0755 ${user} ${user}" ];

  systemd.services."rebuild-${user}" = {
    path = with pkgs; [ # <https://github.com/NixOS/nixpkgs/blob/89e9f68549f312d7b500f9cfe14c3f1d95ae2299/nixos/modules/tasks/auto-upgrade.nix#L91>
                        coreutils gnutar xz.bin gzip gitMinimal config.nix.package.out
                        openssh ];
    serviceConfig = {
      Type = "oneshot";
      User = user;
      Group = user;
      UMask = "0022";
      WorkingDirectory = homeDir;
      ExecStart = let exec = pkgs.writeScriptBin "rebuild" ''
        #! ${pkgs.stdenv.shell}

        set -o errexit

        src=${homeDir}/src
        if [ ! -e $src ] ; then
          git clone --bare git@github.com:michalrus/andrzej-lewandowski.com.git src
          cd $src
          git config --local --add remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
        fi

        cd ${homeDir}/src
        git fetch --all

        declare -A baseURL
        baseURL[master]='https://dev.${domain}/'
        baseURL[release]='https://${domain}/'

        for branch in master release ; do
          echo "--- Building: $branch ---"

          dst=${homeDir}/$branch
          if [ ! -e $dst ] ; then
            cd $src
            git worktree add $dst origin/$branch
          fi

          cd $dst
          git checkout origin/$branch

          nix-build -o public --arg baseURL "''${baseURL[$branch]}"
        done
      ''; in "${exec}/bin/rebuild";
    };
  };

}
