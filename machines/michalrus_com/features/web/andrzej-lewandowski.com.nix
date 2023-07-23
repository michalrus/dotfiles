{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config pkgs; };

let

  domain = "andrzej-lewandowski.com";
  altDomains = [ "andrzejlewandowski.com" "andrzej-lewandowski.pl" "andrzejlewandowski.pl" ];

  webhookPort = 9974;
  user = lib.replaceStrings ["." "-"] ["_" "_"] domain;
  homeDir = "/var/www/${domain}";

in

{

  services.nginx.httpConfig = lib.concatStringsSep "\n" ([

    (sslServer {
      name = domain;
      alternatives = [ "www.${domain}" "*.${domain}" ];
      forcedCertDir = "/var/lib/cloudflare/${domain}";
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
      # generate httpd password hashes with `openssl passwd -apr1`
      body = ''
        ${setRealIPFromCloudflare}
        auth_basic "Speak, friend, and enter.";
        auth_basic_user_file "${pkgs.writeText "htpasswd" ''
          michalrus:$apr1$Q67KvdAC$tUSCO.hwI5nCOtmsbkjyX/
          Krzyś:$apr1$.UBqHkMw$iqeyhOiJ2Q2YHKeoVTMQu.
          mikolaj:$apr1$tVowKZpr$Nul4ytYxnMYvNESnBwb2L1
          slupska:$apr1$Kg/RRn6V$IV.hUtFMXsi48wAtHh4CL.
          #Rndl:$apr1$3lFkYcib$rL3F9IhikosYLZM8gqjry1
          kuba:$apr1$nO2liTmW$rPiuo2hZo6MxSoxyDuG4.0
          #pacioraaa:$apr1$h5Vhypph$xYXbICM3MnvpjvwFUWo6p0
          jupblb:$apr1$oJM3RdYJ$MiN9l6hBSZiNMgP14QSEB0
          ania:$apr1$TJ9gf2ce$8ic6USdd8CUoD93D5Y/b41
          kjanosz:$apr1$AczmhGaa$YPD6fhFJ1DNBqQ3UOvQ400
          madziarar:$apr1$7dlOk0fF$d5sev/iD7FBWjgrodrzEK0
          psychiatra:$apr1$8/GLVfKU$0ueLvh60LQrst8pn/1s4r1
          Mateusz:$apr1$AzfYzV6k$wpSTsetgS60QE3ygglrLI0
          # Ha.:
          bartosz:$apr1$9QyXojiD$rtD/HEgD757hmhN0Schy.1
          #kevin:$apr1$AMhND1PY$UCfXf2gnf8ZMXaA3YEDBJ0
          pkarolczak:$apr1$stgl8558$W9QATO6UR6aql6nJ2/7Vu1
          Erni:$apr1$Ic80R/Xi$i2KQsPlTZOqMiGDVwZBlA/
          wodzu:$apr1$cHVvxRHB$cTc4gg3nMtcLkaEqc8NG7.
          # Kowi:
          doodleman:$apr1$pIj./Cyw$SbUMfQhmzbOOIysbbr431/:Kowi
          # Cannes:
          piotr:$apr1$YcnorKBI$IvmwtjMGXBTNaFere/ppT1
          # Cannes:
          alicja:$apr1$YcnorKBI$IvmwtjMGXBTNaFere/ppT1
          ruta:$apr1$aXI4o.5Y$5geFlfnJGbHu3yetK5gJT/
          # Staw
          Lukasz:$apr1$lTjGHPfl$DsPFl.7ibKKk.CzozPT.J.
        ''}";
        root /var/www/${domain}/master/public;
        error_page 404 /pl/404.html;
        location = / { return 301 https://dev.${domain}/pl/; }
      '';
    })

  ] ++ (lib.concatMap (altDomain: [

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

  ]) altDomains));

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
                        openssh bash ];
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

          mkdir -p config/production
          cat <<EOF >config/production/config.yaml

        baseURL: "''${baseURL[$branch]}"
        cacheDir: "$dst/cache"

        EOF

          nix-shell --run "make"
        done
      ''; in "${exec}/bin/rebuild";
    };
  };

}
