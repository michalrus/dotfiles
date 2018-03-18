{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

let

  domain = "grezza.store";
  alternatives = [ "www.${domain}" ];
  phpfpmSocket = "/run/phpfpm-globalPool.sock";
  timerName = "grezza-store-cron";

  sources = {
    wordpress = pkgs.fetchzip {
      url = "https://wordpress.org/wordpress-4.9.4.tar.gz";
      sha256 = "1s39zzkycbrx329biiy7nrsffkc7zxdgyclp2mgbayjpcj6587lj";
    };
  };

  wpConfig = rec {
    DB_NAME            = "grezza_store";
    DB_USER            = DB_NAME;
    DISALLOW_FILE_MODS = false; # Stuff is mounted read-only anyway, but with this @ true, we won’t get update nags.
    WC_LOG_HANDLER     = "WC_Log_Handler_DB"; # By default it wants to log directly to FS, with no pretty display.
    DISABLE_WP_CRON    = true; # We’ll call /wp-cron.php from a timer below.
  } // (import ./grezza.store.secrets.nix).wpConfig;

  officialStuff = stuff: name: version: sha256: {
    "${name}" = pkgs.fetchzip {
      url = let ver = if builtins.isNull version then "" else ".${version}"; in
        "https://downloads.wordpress.org/${stuff}/${name}${ver}.zip";
      inherit sha256;
    };
  };

  plugins = let get = officialStuff "plugin"; in {}
    // get "affiliates" "3.0.1" "1vcq1af42yswhpxgp0sz45y8n80xxpg3hp2kd3qx35qb3dhq5q1x"
    // get "affiliates-manager" null "1mpjcgqyixqx85rx6xzzglrj2w7hrhk7y5qi2s2bw4g8qhkig35k"
    // get "affiliates-woocommerce-light" "1.3.3" "022ikalc7xyf3yr4pv00k30dbpwkdg3ckr2xjv1s3c70ma93apvs"
    // get "akismet" "4.0.3" "18vi92cnf3q6pg8c474l90nsmr6b9icbxm6mlh9v8v5z58n106qz"
    // get "all-in-one-seo-pack" "2.4.6.1" "0i78y1nvj6fbgc079gkfmls69rc29jlamgbpd16c35b7jxkwmblk" # Has free GoogleAnalytics for WooCommerce.
    // get "backwpup" "3.4.4" "13py4qin61j9r93gawlgys3z8bm4sa5j1pj9q9yws0bimprzk4hw"
    // get "broken-link-checker" "1.11.5" "1drbibnf6pyqk9yw4q9mzq1qx3bjr1iz9x7sy9v4nq8ac52l5n2v"
    // get "contact-form-7" "5.0.1" "0h5fyii09h5m19dil92d68wn76vnj2n7ass5lqpb7nrhx96cr8g6"
    // get "contact-widgets" "1.4.1" "0yxvzybiapvirbpjwvjb4iid5jijwb46z4r8rx4nghiahzhm5jjj"
    // get "cookie-notice" "1.2.41" "0g0mmq4idzwqs4q59ijwp4qfv7x1k0waxw5yb2vsyg9sz56qh46r"
    // get "custom-facebook-feed" "2.5" "0a4q09gfz0xan0wyjn53hw0rwjifsl2ik7wihabzasdh82yivgmi"
    // get "disqus-comment-system" "3.0.15" "1vjflg0q1jck49qdikmi66sn83djr5v2dzbm7j1bzhm5zq29bz0a"
    // get "flamingo" "1.8"   "12la5wp903px1vflc5lm7xrh58xp4qig4f72braizscw5i2n4wvf"
    // get "geoip-detect" "2.8.2" "1vxib021xk0ji479bmfmxlqld4q1vcmpxnwgh7hkna18xlmy0gh4"
    // get "google-analytics-for-wordpress" "7.0.4" "0dxiiz0wx1lhi9zgrys4pls8p1lhgi7aw2aw1byyx6m2jq0cj4wd" # WooCommerce support requires paid version.
    // get "google-sitemap-generator" "4.0.9" "0y3j4bza6v8hsyyqy9chhjkvcm2g1jinq4ccmpy1a56pwwd1b7p2"
    // get "instagram-feed" "1.6.2" "110lv9gxwy73y79vqzala4cy5g2blwzms60nlqzy0cmbyql85lss"
    // get "jetpack" "5.9" "1n68sih0pg9l8bz8fzyg163gkcn9whix6a15k0cliacmnlcmz9wk"
    // get "mailchimp-for-wp" "4.2" "0kya2z530a7d180nbkr8g8jfv784mmfmvkfh4zaxx8xnglrpp3pg"
    // get "recent-tweets-widget" "1.6.8" "0jhh6c4vlr1dqhi5mirfp0zrvn2w42qd0cafrbnpizb1kgmk021v"
    // get "wc-product-customer-list" "2.6.5" "10isqvx158a48pncm07fxz1f9hzn9aj2fpq4aibl8gwanmcgh7n6"
    // get "woocommerce" "3.3.3" "0ygcs5ywxq4vfhkvyk9z71qwcxj4amyh8c0xp1nb0059vdiqxj4c"
    // get "woocommerce-gateway-paypal-express-checkout" "1.5.2" "1w6jndxgz1jls0gqdc18l04mz2hkpackww4yxlfi3z4lwgrpajlc"
    // get "woocommerce-pdf-invoices-packing-slips" "2.1.5" "00wyr39xhw9q06hwybdfjp5idqh5wfdr3z7j3lxawkgkch6mqrq7"
    // get "woocommerce-product-price-based-on-countries" null "0rp9fpv5a4gvb2ff0v4qrj0nl0a9mwaydv2fmhr415ipgkh76pbq"
    // get "woocommerce-sequential-order-numbers" "1.8.2" "0zajwysb216wlssjr2h0cknzgw5791q85jrihjclnpi413cvfl81"
    // get "wp-polls" "2.73.8" "15qr45mmvynaypgcn6nmn2d8f7cb167l76g2xz7g5jx3v1zyn7wm"
    // get "yith-woocommerce-compare" "2.3.0" "0cw43l2jls14jfsjablgwfmyfxbzsghcrwrhwxxid96pjf6c8z4x"
    // get "yith-woocommerce-wishlist" "2.2.1" "13bmxjyi7xaf8j02v0x4w8k60lz954shzj9ma1zhdwi17845kyhi"
    // get "youtube-embed-plus" "11.8.6.1" "1plgi820ll1lkz4929diclkp26mkcfnycqwb6jbbhj8dz4xniqv1"
    ;

  themes = let get = officialStuff "theme"; in {}
    // get "twentyseventeen" "1.4" "1a73rlx58iyglsfm5ps574bn8j70qncvzprm072i15q0fvyblwwp"
    // get "twentysixteen" "1.4" "13wfdsfzq0c42hz5rb96l23nb8ifb05q6vvzv16r26s4927xbiyn"
    // get "twentyfifteen" "1.9" "11r9gjhsppsym48w91qma1pwcadfcjqg6lvypbs7zc68gx92bl25"
    // get "storefront" "2.2.8" "1i6prj2pm6xmijhyjwidq3slb5xsznwd5pyrp686c6vmqdhjsh8m"
    // get "boutique" "2.0.13" "1x8ry3dblmw8ra5f22fdal8307v6zrx0i8hyvms4q4qinbihq05f"
    ;

  languages = let get = lang: country: local: remote: sha256: {
    "${if builtins.isNull local then "${lang}_${country}" else "${local}-${lang}_${country}"}.mo" = pkgs.fetchurl {
      url = "https://translate.wordpress.org/projects/${remote}/${lang}/default/export-translations?format=mo";
      inherit sha256;
    };
  }; getPL = get "pl" "PL"; in {}
    // getPL null "wp/dev" "0xwv7xl88pzxrydispihssvq0l46x5qv513lwgb5mm37km7cxdxg"
    // getPL "continents-cities" "wp/dev/cc" "1lvwg91i4gbx1i4mah5xffkajqkgyv84y0hhf9x85xr1l7a4ml0k"
    // getPL "admin" "wp/dev/admin" "1b6c5mks52kabc5vmb76dmkn95i9wgbfb5lrcl8vc1lf23a1kjmz"
    // getPL "admin-network" "wp/dev/admin/network" "17pn5qf3zswzpl48k4rbnvx7k5sgfvkk1rqzdsvbcg81jqhxwmca"
    // getPL "woocommerce/woocommerce" "wp-plugins/woocommerce/stable" "1ndcyblkkzs5mlg6wcj9mik3acqy3wyp8bd7x5ss5pa5lpnh01kw"
    ;

  wordpress = let

    escapePHP = arg:
      if builtins.isBool arg then (if arg then "true" else "false")
      else "'${lib.replaceStrings ["\\" "'"] ["\\\\" "\\'"] (toString arg)}'";

    addStuff = target: sources: ''
      rm -r "${target}" || true ; mkdir -p "${target}"
      cp $out/wp-content/index.php "${target}"/
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: dir: ''
        mkdir -p "$(dirname "${target}/${name}")"
        ln -s "${dir}" "${target}/${name}"
      '') sources)}
    '';

  in pkgs.runCommand "wordpress" {} ''
    mkdir -p $out
    ln -s ${pkgs.writeText "prevent-ifd-gc" (toString (lib.attrValues sources))} $out/.prevent-ifd-gc

    cp -r ${sources.wordpress}/. $out/
    chmod -R u+w $out

    sed -i $out/wp-config-sample.php -r \
      ${toString (lib.mapAttrsToList (name: _: '' -e "s/^.*define.*'${name}'.*$/# ${name} is defined at the bottom./" '') wpConfig)}

    sed -i $out/wp-config-sample.php -r -e "s/^.*define.*'WP_DEBUG'.*$/\\0\\n\\n@all_of_the_config@/"
    substituteInPlace $out/wp-config-sample.php \
      --subst-var-by ${lib.escapeShellArgs [ "all_of_the_config"
      (lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value:
        "define(${escapePHP name}, ${escapePHP value});" ) wpConfig)) ]}

    mv $out/wp-config-sample.php $out/wp-config.php

    rm $out/readme.html $out/license.txt

    ${addStuff "$out/wp-content/plugins"   plugins}
    ${addStuff "$out/wp-content/themes"    themes}
    ${addStuff "$out/wp-content/languages" languages}
  '';

in

mkMerge [

  (mkCert domain alternatives)

  {
    services.nginx.httpConfig = sslServer {
      name = domain;
      alternatives = alternatives ++ [ "*.grezza.store" ];
      body = ''
        #root /var/www/${domain};
        root ${wordpress};

        index index.html index.htm index.php;
        try_files $uri $uri/ /index.php?q=$uri&$args;

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

    services.phpfpm = {
      pools.globalPool = {
        listen = phpfpmSocket;
        extraConfig = ''
          user = nobody
          listen.group = ${config.services.nginx.group}
          pm = dynamic
          pm.max_children = 5
          pm.start_servers = 5
          pm.min_spare_servers = 5
          pm.max_spare_servers = 5
          pm.max_requests = 500
        '';
      };
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
