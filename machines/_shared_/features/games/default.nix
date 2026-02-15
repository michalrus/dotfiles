{
  flake,
  config,
  lib,
  pkgs,
  ...
}: let
  unfree = import pkgs.path {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };
in {
  environment.systemPackages = with pkgs; [
    aisleriot
    unfree.retroarch-full
  ];
}
