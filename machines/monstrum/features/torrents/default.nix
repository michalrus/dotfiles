{ config, lib, pkgs, ... }:

let

  serviceName = "qbittorrent";
  user = serviceName;
  dataDir = "/var/lib/${serviceName}";
  downloadsDir = "/var/media/torrents";
  domain = "torrents.michalrus.com";
  rpcPort = 9092;

  # An alternative web UI, see <https://github.com/VueTorrent/VueTorrent/wiki/Installation>:
  VueTorrent = pkgs.fetchzip {
    url = "https://github.com/VueTorrent/VueTorrent/releases/download/v2.19.0/vuetorrent.zip";
    hash = "sha256-cIY5fhcLyEPwt5D2T0S4KhAbb8Qmd9m3xcsQTa4FX+8=";
  };

in

{
  systemd.services.${serviceName} = {
    description = "qBittorrent-nox service";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 5;
      User = user;
      Group = user;
      UMask = "0002";
      WorkingDirectory = dataDir;
      BindPaths = [
        "${downloadsDir}:/Downloads"
        "${VueTorrent}:/VueTorrent"
      ];
      ExecStart = "${pkgs.qbittorrent-nox}/bin/qbittorrent-nox";
    };
    environment = {
      QBT_PROFILE = dataDir;
      QBT_WEBUI_PORT = toString rpcPort;
      QBT_CONFIRM_LEGAL_NOTICE = toString 1;
      QBT_RELATIVE_FASTRESUME = toString 1;
    };
  };

  users.users.${user} = {
    isSystemUser = true;
    group = user;
    home = dataDir;
  };
  users.groups.${user} = {};

  systemd.tmpfiles.rules = [
    "d ${dataDir}       0700 ${user} ${user} -"
    "d ${downloadsDir}  0755 ${user} ${user} -"
  ];

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
        proxy_redirect off;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP  $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto https;
        proxy_pass http://127.0.0.1:${toString rpcPort};

        # These happen a few times a second when a WebUI is opened:
        if ($request_uri ~* ^/api/v2/sync/) {
            access_log off;
        }
      }
    }
  '';

}
