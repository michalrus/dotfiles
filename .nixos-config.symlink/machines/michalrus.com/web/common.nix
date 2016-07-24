lib:

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

}
