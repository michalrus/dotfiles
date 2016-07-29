{ config, lib, pkgs, ... }:

with lib;

# This is used to add comments to arbitrary source/destination
# addresses for monitoring in collectd. See
# https://collectd.org/wiki/index.php/Plugin:IPTables for details.

let
  cfg = config.networking.firewall.comments;
  chainIn  = "nixos-fw-comments-in";
  chainOut = "nixos-fw-comments-out";
in

{
  options = {
    networking.firewall.comments = mkOption {
      type = types.attrsOf types.string;
      default = {};
      example = {
        "192.168.1.1" = "machine A";
        "10.0.7.48" = "some other host";
      };
    };
  };

  config = mkMerge [
    {
      networking.firewall.extraCommands = ''
        # Comments: flush the old rules.
        ip46tables -D INPUT  -j ${chainIn}  2> /dev/null || true
        ip46tables -D OUTPUT -j ${chainOut} 2> /dev/null || true
        ip46tables -F ${chainIn}  2> /dev/null || true
        ip46tables -X ${chainIn}  2> /dev/null || true
        ip46tables -F ${chainOut} 2> /dev/null || true
        ip46tables -X ${chainOut} 2> /dev/null || true
      '';
    }

    (mkIf (cfg != {}) {
      networking.firewall = {
        enable = true;
        extraCommands = ''
          # Comments: "${chainIn}" and "${chainOut}" will contain the fall-through rules.
          iptables -N ${chainIn}
          iptables -N ${chainOut}

          # Comments: rules.
          ${concatMapStrings (h: ''
            iptables -A ${chainIn}  -s '${h.name}/32' -m comment --comment '${h.value}'
            iptables -A ${chainOut} -d '${h.name}/32' -m comment --comment '${h.value}'
          '') (mapAttrsToList nameValuePair cfg)}

          # Comments: hook up the new chains.
          iptables -A INPUT  -j ${chainIn}
          iptables -A OUTPUT -j ${chainOut}
        '';
      };
    })
  ];
}
