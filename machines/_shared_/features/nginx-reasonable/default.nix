{
  flake,
  config,
  lib,
  pkgs,
  ...
}: {
  services.nginx.httpConfig = ''
    charset utf-8;

    log_format fmt_syslog '$http_host $remote_addr $remote_user $request_time '
                '"$request" $status $body_bytes_sent '
                '"$http_referer" "$http_user_agent"';

    map $status $log_is_error { "~^5"     1; default 0; }
    map $status $log_is_warn  { "~^4"     1; default 0; }
    map $status $log_is_info  { "~^[^54]" 1; default 0; }
    access_log syslog:server=unix:/dev/log,tag=,nohostname,facility=local2,severity=error fmt_syslog if=$log_is_error;
    access_log syslog:server=unix:/dev/log,tag=,nohostname,facility=local2,severity=warn  fmt_syslog if=$log_is_warn;
    access_log syslog:server=unix:/dev/log,tag=,nohostname,facility=local2,severity=info  fmt_syslog if=$log_is_info;
    error_log  syslog:server=unix:/dev/log,tag=,nohostname,facility=local2 error;

    types {
      #text/plain      log;
      text/plain      php inc;
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
    #types_hash_max_size 2048;

    #default_type application/octet-stream;

    gzip on;
    gzip_disable "msie6";
  '';
}
