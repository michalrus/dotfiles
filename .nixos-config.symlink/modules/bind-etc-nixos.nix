{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.boot.bindEtcNixos;
  dir = "/etc/nixos";
in

{
  options = {
    boot.bindEtcNixos = {
      enable = mkEnableOption "Bind ${dir} to somewhere else (editable by user)";
      location = mkOption { type = types.path; };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.bindfs ];
    fileSystems."${dir}" = {
      device = "${cfg.location}";
      fsType = "fuse.bindfs";
      options = [ "chown-ignore" "chgrp-ignore" "force-user=root" "force-group=root" "perms=0000:a+Dr" ];
    };
  };
}
