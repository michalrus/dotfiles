{ config, lib, pkgs, ... }:

{
  services.resolved.enable = false;

  services.nscd.enable = false;
  system.nssModules = lib.mkForce [];

  services.dnsmasq.enable = false;

  networking.resolvconf.useLocalResolver = true;

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];

  services.bind = {
    enable = true;

    configFile = pkgs.writeText "named.conf" (''
      include "/etc/bind/rndc.key";
      controls {
        inet 127.0.0.1 allow {localhost;} keys {"rndc-key";};
      };

      acl cachenetworks { 127.0.0.0/8; 10.77.2.0/24; };
      acl badnetworks   { };

      options {
        listen-on    { any; };
        listen-on-v6 { any; };
        allow-query  { cachenetworks; };
        blackhole    { badnetworks;   };

        directory "/run/named";
        pid-file  "/run/named/named.pid";

        dnssec-validation auto;

        forward only;
        forwarders {
            10.5.0.1;       // NordVPN 1st hop - highest priority for privacy
            103.86.96.100;  // NordVPN
            103.86.99.100;  // NordVPN
            1.1.1.1;        // Cloudflare
            8.8.8.8;        // Google
            1.0.0.1;        // Cloudflare secondary
            8.8.4.4;        // Google secondary
        };
      };
    ''
    + (let domain = "openproject.michalrus.com"; in ''
      zone "${domain}" {
        type master;
        file "${pkgs.writeText "${domain}.zone" ''
          $TTL    604800
          @       IN      SOA     ns.${domain}. admin.michalrus.com. (2 604800 86400 2419200 604800)
          @       IN      NS      ns.${domain}.
          @       IN      A       10.77.2.1
          ns      IN      A       10.77.2.1
        ''}";
      };
    '')
    + (let domain = "torrents.michalrus.com"; in ''
      zone "${domain}" {
        type master;
        file "${pkgs.writeText "${domain}.zone" ''
          $TTL    604800
          @       IN      SOA     ns.${domain}. admin.michalrus.com. (2 604800 86400 2419200 604800)
          @       IN      NS      ns.${domain}.
          @       IN      A       10.77.2.1
          ns      IN      A       10.77.2.1
        ''}";
      };
    '')
    );
  };
}
