{ flake, pkgs, ... }:

{
  home.packages = [
    #flake.inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.zed-editor
    flake.packages.${pkgs.system}.zed-editor
  ];
}
