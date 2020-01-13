{ config, lib, pkgs, ... }:

with lib;
with import ./common.nix { inherit config lib; };

let

  inherit (import ../openvpn/common.nix { inherit lib; }) subnet-chwalecice;

  domain = "home.michalrus.com";

  # The (random!) credentials hardcoded below are only useful in my
  # home LAN and on this server, so whatever, may as well be public.

  # TODO: reuse this code in dhcpd config in ../../embedded/router-chwalecice--nat.nix
  machines = [
    { name = "printer-canon";    addr = "${subnet-chwalecice}.5"; }
    { name = "printer";          addr = "${subnet-chwalecice}.6"; }
    { name = "camera-kuchnia";   addr = "${subnet-chwalecice}.12"; auth = "eWJ4aGtrb3Y6bGZ3dmNzYXg="; }
    { name = "camera-michal";    addr = "${subnet-chwalecice}.13"; auth = "aHRicGxoamU6c2Nnc2JyZng="; }
    { name = "camera-salon";     addr = "${subnet-chwalecice}.14"; auth = "bHAzamtiZ3M6cTJzdHlpbmM="; }
    { name = "camera-sypialnia"; addr = "${subnet-chwalecice}.15"; auth = "YWRtaW46eTR5ZHZ6bXpnZ20="; }
  ];

in

mkMerge [

  (mkCert domain (map (m: "${m.name}.${domain}") machines))

  {
    # Add these fall-through rules with comments for collectd metrics.
    networking.firewall.comments = listToAttrs (map (m: {
      name = m.addr; value = m.addr;
    }) machines);

    services.nginx.httpConfig = concatMapStrings (m: (sslServer {
      name = "${m.name}.${domain}";
      sslCert = domain;
      body = ''
        # Generate passwords with `echo "user:$(openssl passwd)" >> .htpasswd`.
        auth_basic "Speak, friend, and enter.";
        auth_basic_user_file "${config.services.nginx.stateDir}/auth/${domain}";

        ${optionalString (m ? redirectRootTo)
          ''location = / { return 301 /${m.redirectRootTo}; }''}

        location / {
          proxy_buffering off;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_pass http://${m.addr}:80;
          ${optionalString (m ? auth)
            ''proxy_set_header Authorization "Basic ${m.auth}";''}
        }
      '';
    })) machines;
  }

]
