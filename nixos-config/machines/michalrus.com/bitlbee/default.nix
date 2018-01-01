{ config, pkgs, ... }:

{

  nixpkgs.overlays = [ (self: super: {

    # These are not configurable in the module, so let’s override globally. :/
    inherit (self.nixos-unstable) bitlbee bitlbee-facebook;

  }) ];

  services.bitlbee = {
    enable = true;
    plugins = with pkgs; [ bitlbee-facebook ];
  };

}
