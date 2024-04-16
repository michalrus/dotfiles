{
  config,
  pkgs,
  flake,
  lib,
  ...
}: {
  home.stateVersion = "23.05";

  home.packages = with pkgs; [
    anki-bin
  ];
}
