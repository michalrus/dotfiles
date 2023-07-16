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
    ./hardware.nix
    { networking.hostName = "aneta"; }

    ./features/dns.nix
    ./features/nat.nix
    ./features/openvpn.nix
    #./features/proxy.nix  # TODO: needs more work
    ./features/users.nix

    # FIXME: drop:
    ../../nixos-config/modules
    ../../nixos-config/machines/common.nix
  ];
}
