{ flake, pkgs, ... }:

{
  home.packages = [
    flake.inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.zed-editor
  ];
}
