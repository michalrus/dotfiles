{ config, lib, pkgs, ... }:

with lib;

{
  networking.firewall.allowedUDPPorts = [
    500 4500
  ];

  environment.systemPackages = with pkgs; [
    strongswan  # for `ipsec status` in PATH etc.
  ];

  services.strongswan = {
    enable = true;
    secrets = [ "/var/lib/strongswan/ipsec.secrets" ];
    connections = {

      "%default" = {
        ikelifetime = "60m";
        keylife = "20m";
        rekeymargin = "3m";
        keyingtries = "1"; # why so low?
        keyexchange = "ikev1";
        authby = "secret";
      };

      "home" = {
        # 1° “left” is here, and “right” on the other side,
        # 2° peers will be matched using IDs (of DNS type!), not IPs, therefore "%any",
        # 3° when configuring the router, choose AES for ciphering,
        # 4° as always, `journalctl -f -u strongswan.service` is useful for debuging,
        # 5° the thing I don’t get is why is there no additional route and connections work.
        left = "%any";
        leftid = "michalrus.com";
        leftsubnet = "172.31.46.6/32";
        right = "%any";
        rightid = "home.michalrus.com";
        rightsubnet = "10.0.1.0/24";
        auto = "add"; # what does this one do?
      };

    };
  };
}
