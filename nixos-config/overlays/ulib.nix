{ lib, fetchFromGitHub, ... }:

{

  inherit lib;

  # TODO: remove after https://github.com/NixOS/nixpkgs/issues/33258
  composeOverlays = overlays: self: super:
    lib.foldl' (lib.flip lib.extends) (lib.const super) overlays self;

  nixpkgsOf = rev: sha256:
    fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      inherit rev sha256;
    };

}
