{ inputs }:

# FIXME: use: lib.nixosSystem, but the current nixpkgs-aneta are too old for that:

let
  nixpkgs = inputs.nixpkgs-aneta;
  system = "aarch64-linux";
  pkgs = import nixpkgs { inherit system; };
in import "${nixpkgs}/nixos/lib/eval-config.nix" {
  inherit system pkgs;
  modules = [
    {
      system.nixos.versionSuffix = ".${pkgs.lib.substring 0 8 nixpkgs.lastModifiedDate}.${nixpkgs.shortRev}";
      system.nixos.revision = nixpkgs.rev;
      _module.args = { inherit inputs; };
    }
    ../../nixos-config/machines/embedded/router-wroclaw/default.nix
  ];
}
