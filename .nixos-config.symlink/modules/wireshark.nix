{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.wireshark;
in

{
  options = {
    programs.wireshark = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      wireshark
    ];

    users.extraGroups.wireshark = {};

    security = {
      setuidOwners = [
        {
          program = "dumpcap";
          owner = "root";
          group = "wireshark";
          setuid = true;
          setgid = false;
          permissions = "u+rx,g+x";
        }
      ];
    };

  };
}
