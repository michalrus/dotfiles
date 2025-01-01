{ flake, config, lib, pkgs, ... }:

let
  unfree = import pkgs.path { inherit (pkgs) system; config.allowUnfree = true; };
in

{
  environment.systemPackages = with pkgs; [
    aisleriot
    unfree.retroarchFull
  ];
}
