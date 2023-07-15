{ inputs }:

# FIXME: use: lib.nixosSystem, but the current nixpkgs-aneta are too old for that:

let
  nixpkgs = inputs.nixpkgs-aneta;
  system = "aarch64-linux";
in import "${nixpkgs}/nixos/lib/eval-config.nix" {
  inherit system;
  pkgs = import nixpkgs { inherit system; overlays = [ (import ../nixos-config/overlays) ]; };
  modules = [
    {
      system.nixos.versionSuffix = ".${nixpkgs.lastModifiedDate}.${nixpkgs.shortRev}";
      system.nixos.revision = nixpkgs.rev;
      _module.args = { inherit (inputs) self; };
    }
    ../nixos-config/machines/embedded/router-wroclaw/default.nix
  ];
}
