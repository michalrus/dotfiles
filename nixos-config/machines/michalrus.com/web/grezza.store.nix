{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

let

  domain = "grezza.store";

  alternatives = [ "www.${domain}" ];
  user = lib.replaceStrings ["."] ["_"] domain;
  mutableDir = "/var/lib/${domain}";
  mutableDirNginxBind = "/var/www/${domain}";
  phpfpmDir = "/run/phpfpm";
  phpfpmSocket = "${phpfpmDir}/${user}.sock";
  timerName = "${user}-cron";

  # Basically, keep core & config immutable. The `wp-content/` one is
  # mutable, as it’s insane to keep it immutable, I really tried.

  sources = {
    wordpress = pkgs.fetchzip {
      url = "https://wordpress.org/wordpress-4.9.4.tar.gz";
      sha256 = "1s39zzkycbrx329biiy7nrsffkc7zxdgyclp2mgbayjpcj6587lj";
    };
  };

  wpConfig = rec {
    # WP_DEBUG = true; WP_DEBUG_LOG = true; WP_DEBUG_DISPLAY = false;
    FS_METHOD           = "direct";
    WP_AUTO_UPDATE_CORE = true; # Won’t work anyways, but at least it’ll scream in the panel… I hope.
    DB_NAME             = user;
    DB_USER             = DB_NAME;
    DISALLOW_FILE_MODS  = false; # Stuff is mounted read-only anyway, but with this @ true, we won’t get update nags.
    WC_LOG_HANDLER      = "WC_Log_Handler_DB"; # By default it wants to log directly to FS, with no pretty display.
    DISABLE_WP_CRON     = true; # We’ll call /wp-cron.php from a timer below.
  } // (import ./grezza.store.secrets.nix).wpConfig;

  wordpress = let

    escapePHP = arg:
      if builtins.isBool arg then (if arg then "true" else "false")
      else "'${lib.replaceStrings ["\\" "'"] ["\\\\" "\\'"] (toString arg)}'";

  in pkgs.runCommand "wordpress" {} ''
    mkdir -p $out
    ln -s ${pkgs.writeText "prevent-ifd-gc" (toString (lib.attrValues sources))} $out/.prevent-ifd-gc

    cp -r ${sources.wordpress}/. $out/
    chmod -R u+w $out

    ${pkgs.dos2unix}/bin/dos2unix $out/wp-config-sample.php # wut?… o_O’

    sed -i $out/wp-config-sample.php -r \
      ${toString (lib.mapAttrsToList (name: _: '' -e "s/^.*define.*'${name}'.*$/# \\0 # Defined at the bottom./" '') wpConfig)}

    sed -i $out/wp-config-sample.php -r -e "s/^.*define.*'WP_DEBUG'.*$/\\0\\n\\n@all_of_the_config@/"
    substituteInPlace $out/wp-config-sample.php \
      --subst-var-by ${lib.escapeShellArgs [ "all_of_the_config"
      (lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value:
        "define(${escapePHP name}, ${escapePHP value});" ) wpConfig)) ]}

    cat << 'EOF' >> $out/wp-config-sample.php

    add_filter('auto_update_plugin',          '__return_true');
    add_filter('auto_update_theme',           '__return_true');
    add_filter('auto_update_translation',     '__return_true');
    add_filter('auto_core_update_send_email', '__return_true');
    EOF

    mv $out/wp-config-sample.php $out/wp-config.php

    rm $out/readme.html $out/license.txt

    rm -r $out/wp-content && ln -s ${mutableDir} $out/wp-content
  '';

in

mkMerge [

  (mkCert domain alternatives)

  {
    services.nginx.httpConfig = sslServer {
      name = domain;
      alternatives = alternatives ++ [ "*.${domain}" ];
      body = ''
        root ${wordpress};

        index index.html index.htm index.php;
        try_files $uri $uri/ /index.php?q=$uri&$args;

        location ^~ /wp-content/ {
          alias ${mutableDirNginxBind}/;
        }

        location ~ \.php$ {
          try_files $fastcgi_script_name =404;
          fastcgi_pass unix:${phpfpmSocket};
          fastcgi_index index.php;
          include ${config.services.nginx.package}/conf/fastcgi.conf;
          fastcgi_intercept_errors on;
          fastcgi_ignore_client_abort off;
          fastcgi_connect_timeout 60;
          fastcgi_send_timeout 180;
          fastcgi_read_timeout 180;
          fastcgi_buffer_size 128k;
          fastcgi_buffers 4 256k;
          fastcgi_busy_buffers_size 256k;
          fastcgi_temp_file_write_size 256k;
        }
      '';
    };

    services.mysql = {
      enable = true;
      package = pkgs.mysql;
      bind = "127.0.0.1";
      initialScript = pkgs.writeText "mysql-init.sql" ''
        CREATE DATABASE ${wpConfig.DB_NAME};
        CREATE USER '${wpConfig.DB_USER}'@'localhost' IDENTIFIED BY '${wpConfig.DB_PASSWORD}';
        GRANT ALL PRIVILEGES ON ${wpConfig.DB_NAME}.* TO '${wpConfig.DB_USER}'@'localhost';
      '';
    };

    users.extraUsers."${user}"  = { isSystemUser = true; home = mutableDir; };
    users.extraGroups."${user}" = { };

    services.phpfpm = {
      pools."${user}" = {
        listen = phpfpmSocket;
        extraConfig = ''
          pm = dynamic
          pm.max_children = 5
          pm.start_servers = 5
          pm.min_spare_servers = 5
          pm.max_spare_servers = 5
          pm.max_requests = 500
        '';
      };
    };

    systemd.services."phpfpm-${user}" = {
      serviceConfig = {
        User = user;
        Group = user;
        PermissionsStartOnly = true;
      };
      preStart = ''
        mkdir -p ${phpfpmDir}
        chown -R ${user}:${config.services.nginx.group} ${phpfpmDir}

        mkdir -p ${mutableDir}
        chown -R ${user}:${user} ${mutableDir}
      '';
      postStart = ''
        chgrp ${config.services.nginx.group} ${phpfpmSocket}
      '';
    };

    fileSystems."${mutableDirNginxBind}" = {
      device = mutableDir;
      fsType = "fuse.bindfs";
      options = [ "ro" "force-user=${config.services.nginx.user}" "force-group=${config.services.nginx.group}" "perms=640:u+D:g+D" ];
    };

    systemd.timers."${timerName}" = {
      partOf = [ "${timerName}.service" ];
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "*:0/5"; # every 5 minutes
    };

    systemd.services."${timerName}" = {
      path = with pkgs; [ curl ];
      script = "exec curl -sS 'https://${domain}/wp-cron.php?doing_wp_cron'";
    };

  }

]
