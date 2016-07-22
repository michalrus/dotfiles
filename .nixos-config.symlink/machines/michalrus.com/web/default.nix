{ config, lib, pkgs, ... }:

with lib;

let

  acmeChallenges = "/var/www/acme-challenges";

  homeProxy = name: addr: auth: redirectRootTo: ''
    server {
      listen 443;
      listen [::]:443;

      ssl on;
      ssl_certificate     ${config.security.acme.directory}/home.michalrus.com/fullchain.pem;
      ssl_certificate_key ${config.security.acme.directory}/home.michalrus.com/key.pem;

      server_name ${name}.home.michalrus.com;

      access_log logs/${name}.home.michalrus.com.access;
      error_log logs/${name}.home.michalrus.com.error;

      # Generate passwords with `echo "user:$(openssl passwd)" >> .htpasswd`.
      auth_basic "Speak, friend, and enter.";
      auth_basic_user_file "${config.services.nginx.stateDir}/auth/home.michalrus.com";

      ${optionalString (redirectRootTo != null)
        ''location = / { return 301 /${redirectRootTo}; }''}

      location / {
        proxy_buffering off;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_pass http://${addr}:80;
        ${optionalString (auth != null)
          ''proxy_set_header Authorization "Basic ${auth}";''}
      }
    }
  '';

in

{

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  systemd.services.nginx.preStart = ''
    mkdir -p "${acmeChallenges}"
  '';

  environment.systemPackages = with pkgs; [
    bindfs
  ];

  fileSystems."/var/www/michalrus.com" = {
    device = "/home/m/public_html";
    fsType = "fuse.bindfs";
    options = [ "ro" "force-user=nginx" "force-group=nginx" "perms=640:u+D:g+D" ];
  };

  security.acme.certs."michalrus.com" = {
    webroot = acmeChallenges;
    email = "m@michalrus.com";
    postRun = "systemctl reload nginx.service";
    extraDomains = {
      "p.michalrus.com" = null;
      "git.michalrus.com" = null;
      "www.michalrus.com" = null;
      "michalrus.pl" = null; "www.michalrus.pl" = null;
      "michalrus.eu" = null; "www.michalrus.eu" = null;
    };
  };

  security.acme.certs."home.michalrus.com" = {
    webroot = acmeChallenges;
    email = "m@michalrus.com";
    postRun = "systemctl reload nginx.service";
    extraDomains = {
      "printer.home.michalrus.com" = null;
      "camera-kuchnia.home.michalrus.com" = null;
      "camera-salon.home.michalrus.com" = null;
      "camera-sypialnia.home.michalrus.com" = null;
    };
  };

  services = {
    nginx = {
      enable = true;

      httpConfig = ''
        charset utf-8;

        types {
          text/plain      log;
        }

        ssl_protocols TLSv1 TLSv1.1 TLSv1.2; # Dropping SSLv3, ref: POODLE

        # SSL ciphers, ref: LOGJAM → https://weakdh.org/sysadmin.html
        ssl_prefer_server_ciphers on;
        ssl_ciphers 'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA';

        # DH param generated with `openssl dhparam -out dhparam.pem 4096`.
        # It’s safe to keep it public, as long as it’s ≥ 2048 bits,
        # cf. http://security.stackexchange.com/a/42418
        ssl_dhparam ${./dhparam.pem};

        sendfile on;
        tcp_nopush on;
        tcp_nodelay on;
        types_hash_max_size 2048;

        default_type application/octet-stream;

        gzip on;
        gzip_disable "msie6";

        # Redirect all HTTP requests to HTTPS.
        server {
          listen 80 default;
          listen [::]:80 default;
          server_name _;

          access_log logs/default-http.access;
          error_log logs/default-http.error;

          location /.well-known/acme-challenge {
            root ${acmeChallenges};
          }

          location / {
            rewrite ^ https://$host$request_uri permanent;
          }
        }

        server {
          listen 443;
          listen [::]:443;

          ssl on;
          ssl_certificate     ${config.security.acme.directory}/michalrus.com/fullchain.pem;
          ssl_certificate_key ${config.security.acme.directory}/michalrus.com/key.pem;

          server_name michalrus.com;

          access_log logs/michalrus.com.access;
          error_log logs/michalrus.com.error;

          location / {
            root /var/www/michalrus.com;
          }
        }

        # The (random!) credentials hardcoded below are only useful in my
        # home LAN and on this server, so whatever, may as well be public.

        ${homeProxy "printer"          "10.0.1.5"  null null}
        ${homeProxy "camera-kuchnia"   "10.0.1.11" "cGlranBsZW06aHZicmpsZXk=" "index3.htm"}
        ${homeProxy "camera-salon"     "10.0.1.12" "eWJ4aGtrb3Y6bGZ3dmNzYXg=" null}
        ${homeProxy "camera-sypialnia" "10.0.1.13" "aHRicGxoamU6c2Nnc2JyZng=" null}
      '';
    };
  };

}
