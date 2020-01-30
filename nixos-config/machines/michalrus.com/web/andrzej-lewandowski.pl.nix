{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

let

  domain = "andrzej-lewandowski.pl";

  webhookPort = 9974;
  user = lib.replaceStrings ["." "-"] ["_" "_"] domain;
  homeDir = "/var/www/${domain}";

  themeName = "hugo-theme-learn";
  theme = pkgs.fetchFromGitHub {
    owner = "matcornic"; repo = themeName;
    rev = "8cf7531a22972f6ff8898604e87ab6f5ee8c927e";
    sha256 = "1lwl6cwqi2p8zns0ajpfq9qpfyd8ipif3qbgnr8fsrg3dvj1nqrw";
  };

in

{

  services.nginx.httpConfig = lib.concatStringsSep "\n" [

    (sslServer {
      name = domain;
      alternatives = [ "www.${domain}" "*.${domain}" ];
      forcedCertDir = "/var/lib/cloudflare/${domain}";
      # generate httpd password hashes with `openssl passwd -apr1`
      body = ''
        root /var/www/${domain}/release/public;

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
        auth_basic "Speak, friend, and enter.";
        auth_basic_user_file "${pkgs.writeText "htpasswd" ''
          michalrus:$apr1$Q67KvdAC$tUSCO.hwI5nCOtmsbkjyX/
          Krzyś:$apr1$.UBqHkMw$iqeyhOiJ2Q2YHKeoVTMQu.
        ''}";
        root /var/www/${domain}/master/public;
        expires epoch;
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
    path = with pkgs; [ git openssh hugo ];
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
          git clone --bare git@github.com:michalrus/andrzej-lewandowski.pl.git src
          cd $src
          git config --local --add remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
        fi

        cd ${homeDir}/src
        git fetch --all

        declare -A baseURL
        baseURL[master]='https://dev.andrzej-lewandowski.pl/'
        baseURL[release]='https://andrzej-lewandowski.pl/'

        for branch in master release ; do
          echo "--- Building: $branch ---"

          dst=${homeDir}/$branch
          if [ ! -e $dst ] ; then
            cd $src
            git worktree add $dst origin/$branch
          fi

          ln -sfn ${theme} $dst/themes/${themeName}

          cd $dst
          git checkout origin/$branch
          hugo --baseURL "''${baseURL[$branch]}" --cacheDir $dst.cache
        done
      ''; in "${exec}/bin/rebuild";
    };
  };

}
