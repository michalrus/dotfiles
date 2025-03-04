{ config, lib, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    wineWowPackages.stableFull
    winetricks
  ];

  hardware = {
    graphics.enable32Bit = true; # for Wine
    pulseaudio = {
      #enable = true;  # FIXME: conflicts with hyprland which wants PipeWire
      support32Bit = true; # for Wine
    };
  };

}
