{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

let

  domain = "monitor.michalrus.com";

  collection3 = pkgs.stdenv.mkDerivation {
    name = "collection3";
    src = config.services.collectd.package.src;
    phases = "unpackPhase installPhase";
    buildInputs = (with pkgs; [ perl rrdtool makeWrapper ]) ++ (with pkgs.perlPackages; [
      CGI ConfigGeneral URI
    ]);
    installPhase = ''
      cp -r contrib/collection3 $out
      cp -r bindings/perl/lib/. $out/lib
      patchShebangs $out
      find $out -type f -executable | while read f ; do
        wrapProgram "$f" \
          --prefix PERL5LIB : "$PERL5LIB" \
          --prefix PERL5LIB : "$out/lib"
      done;
      substituteInPlace $out/etc/collection.conf \
        --replace '#DataDir ' 'DataDir ' \
        --replace '/var/lib/collectd/rrd' '${config.services.collectd.dataDir}' \
        --replace 'GraphWidth 400' 'GraphWidth 1200'
    '';
  };

in

mkMerge [

  (mkCert domain [])

  {
    services.fcgiwrap = {
      enable = true;
      user = "nginx";
      group = "nginx";
    };

    # Make RRDs readable to nginx.
    systemd.services.collectd.preStart = let d = config.services.collectd.dataDir; in ''
      chgrp ${config.services.nginx.group} ${d}
      chmod g+rX ${d}
    '';

    services.nginx.httpConfig = sslServer {
      name = domain;
      body = ''
        location = / { return 301 /bin/index.cgi; }

        location ~ \.cgi$ {
          root ${collection3};
          if (!-e $request_filename) { return 404; }
          expires off;
          fastcgi_pass unix:${config.services.fcgiwrap.socketAddress};
          #fastcgi_index index.cgi;
          fastcgi_param SCRIPT_NAME $fastcgi_script_name;
          fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
          include ${config.services.nginx.package}/conf/fastcgi_params;
        }

        location /share {
          alias ${collection3}/share;
        }
      '';
    };
  }

]
