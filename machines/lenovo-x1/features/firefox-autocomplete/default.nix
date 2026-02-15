{
  config,
  lib,
  pkgs,
  ...
}: let
  getUserPort = pkgs.writeScript "getUserPort" ''
    ${lib.generators.toKeyValue {} config.services.firefox-autocomplete.userPorts}
    echo ''${!USER}
  '';
in {
  options.services.firefox-autocomplete.userPorts = lib.mkOption {
    type = lib.types.attrsOf lib.types.int;
    default = {};
  };

  config.systemd.user.services.firefox-autocomplete = {
    description = "Simple wrapper around various non-standard search auto-complete services.";
    path = [(pkgs.python3.withPackages (ps: with ps; [flask-restful requests]))];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
    };
    script = ''
      exec ${./firefox-autocomplete.py} "$(${getUserPort})"
    '';
  };
}
