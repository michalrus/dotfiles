{ flake, config, lib, pkgs, ... }:

let
  pkgs-2305 = flake.inputs.nixpkgs-2305.legacyPackages.${pkgs.system};
in

{

  environment.systemPackages = [
    pkgs-2305.wineWowPackages.stableFull
    pkgs-2305.winetricks
  ];

  hardware = {
    opengl.driSupport32Bit = true; # for Wine
    pulseaudio = {
      enable = true;
      support32Bit = true; # for Wine
    };
  };

}
