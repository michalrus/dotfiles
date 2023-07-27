{ lib, pkgs, ... }:

let

  # Additional IOG (Cardano) binary cache:
  substituters = lib.mkForce [
    "https://cache.nixos.org"
    "https://cache.iog.io"
  ];

  trusted-public-keys = lib.mkForce [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];

in

{
  nix = if pkgs.stdenv.isDarwin || (pkgs.stdenv.isLinux && lib.versionAtLeast lib.version "23.04") then {
    settings.substituters = substituters;
    settings.trusted-public-keys = trusted-public-keys;
  } else {
    binaryCaches = substituters;
    binaryCachePublicKeys = trusted-public-keys;
  };
}
