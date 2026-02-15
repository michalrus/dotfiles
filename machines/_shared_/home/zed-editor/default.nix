{
  flake,
  pkgs,
  ...
}: {
  home.packages = [
    flake.inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.zed-editor
    #flake.packages.${pkgs.stdenv.hostPlatform.system}.zed-editor
  ];
}
