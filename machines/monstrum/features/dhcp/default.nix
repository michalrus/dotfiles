{ config, lib, pkgs, ... }:

let
  internalInterface = "enp16s0";
in

{
  networking.interfaces.${internalInterface} = {
    useDHCP = false;
    ipv4.addresses = [{
      address = "10.77.2.1";
      prefixLength = 24;
    }];
  };

  services.kea.dhcp4 = {
    enable = true;
    settings = {
      interfaces-config.interfaces = [ internalInterface ];
      lease-database = {
        name = "/var/lib/kea/dhcp4.leases";
        persist = true;
        type = "memfile";
      };
      valid-lifetime = 3600;
      rebind-timer = 900;
      renew-timer = 600;
      subnet4 = [
        {
          id = 1;
          subnet = "10.77.2.1/24";
          pools = [
            {
              pool = "10.77.2.101 - 10.77.2.240";
            }
          ];
        }
      ];
      reservations = [
        { hw-address = "ac:15:a2:b3:e9:e9"; ip-address = "10.77.2.2";   } # Access Point
        { hw-address = "00:1a:4b:23:ba:6e"; ip-address = "10.77.2.5";   } # printer
        { hw-address = "e4:70:b8:f7:4b:8f"; ip-address = "10.77.2.11";  } # dell-home-server
        { hw-address = "d6:b8:1b:53:d8:20"; ip-address = "10.77.2.99";  } # Michal’s MacBook
      ];
      option-data = [
        {
          name = "routers";
          data = "10.77.2.1";
        }
        {
          name = "domain-name-servers";
          data = "10.77.2.1";
        }
      ];
    };
  };



}
