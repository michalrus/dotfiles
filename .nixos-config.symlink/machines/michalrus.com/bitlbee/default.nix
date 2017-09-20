{ config, pkgs, ... }:

{

  # These are not configurable, so let’s override globally.
  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    inherit (super.nixos-unstable) bitlbee bitlbee-facebook;
  };

  services.bitlbee = {
    enable = true;
    plugins = with pkgs; [ bitlbee-facebook ];
  };

}
