{ flake, config, lib, pkgs, ... }:

let
  pkgs-23_05 = flake.inputs.nixpkgs.legacyPackages.${pkgs.system};
in

{

  environment.systemPackages = with pkgs; [
    pkgs-23_05.wineWowPackages.stableFull
    pkgs-23_05.winetricks
  ];

  hardware = {
    opengl.driSupport32Bit = true; # for Wine
    pulseaudio = {
      enable = true;
      support32Bit = true; # for Wine
    };
  };

}
