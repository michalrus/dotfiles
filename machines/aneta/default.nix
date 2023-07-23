{ inputs }:

# FIXME: use: lib.nixosSystem, but the current nixpkgs-2003 are too old for that:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2003;
  system = "aarch64-linux";
  pkgs = import nixpkgs { inherit system; };
in import "${nixpkgs}/nixos/lib/eval-config.nix" {
  inherit system pkgs;
  modules = [
    {
      system.nixos.versionSuffix = ".${pkgs.lib.substring 0 8 nixpkgs.lastModifiedDate}.${nixpkgs.shortRev}";
      system.nixos.revision = nixpkgs.rev;
      _module.args = { inherit flake; };
    }
    ./hardware.nix
    { networking.hostName = "aneta"; }

    flake.nixosModules.dotfiles-old
    flake.nixosModules.gnu-screen

    ../common-features/fav-pkgs-cli-thin.nix
    ../common-features/immutable-users.nix
    ../common-features/ip-reject-not-drop.nix
    ../common-features/kill-user-processes.nix
    ../common-features/locale-en-iso.nix
    ../common-features/more-entropy.nix
    ../common-features/mtr-traceroute-fping.nix
    ../common-features/nix.conf.nix
    ../common-features/systemd-accounting.nix
    ../common-features/zsh.nix

    ./features/dns.nix
    ./features/nat.nix
    ./features/openvpn.nix
    #./features/proxy.nix  # TODO: needs more work
    ./features/users.nix

    {
      services.journald.extraConfig = ''
        SystemMaxUse=200M
      '';
    }
  ];
}
