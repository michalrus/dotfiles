{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
# This is used to add comments to arbitrary source/destination
# addresses for monitoring in collectd. See
# https://collectd.org/wiki/index.php/Plugin:IPTables for details.
  let
    cfg = config.networking.firewall.comments;
    chainIn = "nixos-fw-comments-in";
    chainOut = "nixos-fw-comments-out";

    flushRules = ''
      # Comments: flush the old rules.
      ip46tables -w -D INPUT  -j ${chainIn}  2> /dev/null || true
      ip46tables -w -D OUTPUT -j ${chainOut} 2> /dev/null || true
      for chain in ${chainIn} ${chainOut} ; do
        ip46tables -w -F $chain 2> /dev/null || true
        ip46tables -w -X $chain 2> /dev/null || true
      done
    '';

    setupRules = ''
      # Comments: "${chainIn}" and "${chainOut}" will contain the fall-through rules.
      iptables -w -N ${chainIn}
      iptables -w -N ${chainOut}

      # Comments: rules.
      ${concatMapStrings (h: ''
        iptables -w -A ${chainIn}  -s '${h.name}/32' -m comment --comment '${h.value}'
        iptables -w -A ${chainOut} -d '${h.name}/32' -m comment --comment '${h.value}'
      '') (mapAttrsToList nameValuePair cfg)}

      # Comments: hook up the new chains.
      iptables -w -A INPUT  -j ${chainIn}
      iptables -w -A OUTPUT -j ${chainOut}
    '';
  in {
    options = {
      networking.firewall.comments = mkOption {
        type = types.attrsOf types.str;
        default = {};
        example = {
          "192.168.1.1" = "machine A";
          "10.0.7.48" = "some other host";
        };
      };
    };

    config = mkMerge [
      {networking.firewall.extraCommands = mkBefore flushRules;}

      (mkIf (cfg != {}) {
        networking.firewall = {
          enable = true;
          extraCommands = setupRules;
          extraStopCommands = flushRules;
        };
      })
    ];
  }
