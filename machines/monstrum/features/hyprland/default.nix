{ config, lib, pkgs, ... }:

{
  programs.hyprland.enable = true;

  home-manager.sharedModules = [{
    programs.kitty.enable = true; # FIXME: only temporarily for Hyprland

    home.file.".config/hypr/hyprland.conf".source = ./hyprland.conf;

    programs.wofi.enable = true;
  }];
}
