{malicious-hosts}: {
  pkgs,
  lib,
  ...
}: let
  # XXX: raw StevenBlack contains non-malicious definitions, let’s filter them out:
  filtered = let
    marker = "# Start StevenBlack";
  in
    pkgs.runCommand "filtered-StevenBlack" {} ''
      grep >/dev/null -E ^${lib.escapeShellArg marker} ${malicious-hosts} || exit 77

      gawk <${malicious-hosts} >$out '
        BEGIN { do_print = 0 }
        {
          if (!do_print && $0 ~ /^'${lib.escapeShellArg marker}'/) { do_print = 1 }
          if (do_print || $0 ~ /^#/) { print }
        }'
    '';
in {
  networking.extraHosts = "\n" + builtins.readFile filtered;
}
