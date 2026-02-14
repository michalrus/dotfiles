{ flake, config, lib, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    flake.packages.${pkgs.stdenv.hostPlatform.system}.wine-bwrap
    # FIXME: remove normal Wine from PATH?
    wineWowPackages.stableFull
    winetricks
  ];

  hardware.graphics.enable32Bit = true; # for Wine
  services.pulseaudio = {
    #enable = true;  # FIXME: conflicts with hyprland which wants PipeWire
    support32Bit = true; # for Wine
  };

}
