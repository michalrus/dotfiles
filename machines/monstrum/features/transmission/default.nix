{ config, lib, pkgs, ... }:

let

  downloads = "${config.services.transmission.home}/Downloads";
  incomplete = "${config.services.transmission.home}/Incomplete";
  domain = "transmission.michalrus.com";

in

{
  age.secrets.tranmission_rpc_password = {
    file = ../../../../secrets/transmission_rpc_password.age;
    owner = config.services.transmission.user;
  };

  age.secrets.tranmission_rpc_password_nginx = {
    file = ../../../../secrets/transmission_rpc_password_nginx.age;
    owner = config.services.nginx.user;
  };

  services.transmission = {
    enable = true;

    credentialsFile = config.age.secrets.tranmission_rpc_password.path;

    settings = {
      umask = 2; # octal 0002 in decimal
      download-dir = downloads;
      incomplete-dir = incomplete;
      incomplete-dir-enabled = true;

      rpc-bind-address = "127.0.0.1";
      rpc-port = 9091;
      rpc-authentication-required = true;
      rpc-username = "transmission";

      message-level = 1; # 1 – only errors, otherwise very spammy

      speed-limit-up = 50;
      speed-limit-up-enabled = true;
    };

    downloadDirPermissions = "775";
  };

  security.acme.certs.${domain} = {
    webroot = "/var/www/acme-challenges";
    email = "m@michalrus.com";
    postRun = "systemctl reload nginx.service";
    extraDomainNames = [];
    group = "nginx";
  };

  services.nginx.httpConfig = ''
    server {
      listen 8443 ssl proxy_protocol;
      listen [::]:8443 ssl proxy_protocol;

      set_real_ip_from 127.0.0.1;
      set_real_ip_from 10.77.5.1;
      real_ip_header proxy_protocol;

      ssl_certificate     ${config.security.acme.certs.${domain}.directory}/fullchain.pem;
      ssl_certificate_key ${config.security.acme.certs.${domain}.directory}/key.pem;

      server_name ${domain};
      client_max_body_size 64M;

      location / {
        # Hash passwords with: `printf "password" | openssl passwd -apr1 -stdin`
        auth_basic "Transmission";
        auth_basic_user_file ${pkgs.writeText "htpasswd" ''
          m:$apr1$cGe0wruI$/p0LSmjDCms61WWNSCTai0
          krzyszu:$apr1$apKMJVRz$18dgpOMlP2DW5xZY51CmR/
        ''};

        proxy_redirect off;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP  $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto https;
        include ${config.age.secrets.tranmission_rpc_password_nginx.path};
        proxy_pass http://127.0.0.1:${toString config.services.transmission.settings.rpc-port};
      }
    }
  '';

}
