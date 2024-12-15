{ config, flake, pkgs, lib, ... }:

{
  home.packages = [ pkgs.alacritty ];
  home.sessionVariables.TERMINAL = "alacritty";
  home.file.".config/alacritty/alacritty.toml".source = ./alacritty.toml;
}
