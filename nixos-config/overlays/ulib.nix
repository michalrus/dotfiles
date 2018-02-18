{ lib, fetchFromGitHub, ... }:

rec {

  inherit lib;

  # TODO: remove after https://github.com/NixOS/nixpkgs/issues/33258
  composeOverlays = overlays: self: super:
    lib.foldl' (lib.flip lib.extends) (lib.const super) overlays self;

  nixpkgsOf = rev: sha256:
    fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      inherit rev sha256;
    };

  fromNixpkgs = pname: rev: sha: config: self: super:
    let nixpkgs = nixpkgsOf rev sha; in {
      "${pname}" = (import nixpkgs { inherit config; })."${pname}".overrideAttrs (oldAttrs: {
        postInstall = "echo ${nixpkgs} >$out/prevent-ifd-gc";
      });
    };

}
