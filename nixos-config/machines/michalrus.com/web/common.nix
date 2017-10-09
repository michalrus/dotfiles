{ config, lib }:

with lib;

rec {

  acmeChallenges = "/var/www/acme-challenges";

  mkCert = primary: secondaries: {
    security.acme.certs."${primary}" = {
      webroot = acmeChallenges;
      email = "m@michalrus.com";
      postRun = "systemctl reload nginx.service";
      extraDomains = builtins.listToAttrs (map (secondary: nameValuePair secondary null) secondaries);
    };
  };

  sslServer = args: let
    sslCert = if args ? sslCert then args.sslCert else args.name;
  in ''
    server {
      listen 443;
      listen [::]:443;

      ssl on;
      ssl_certificate     ${config.security.acme.directory}/${sslCert}/fullchain.pem;
      ssl_certificate_key ${config.security.acme.directory}/${sslCert}/key.pem;

      server_name ${args.name};

      access_log logs/${args.name}.access;
      error_log  logs/${args.name}.error;

      ${args.body}
    }
  '';

}
