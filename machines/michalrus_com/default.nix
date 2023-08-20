{ inputs }:

# FIXME: use: lib.nixosSystem, but the current nixpkgs-2003 are too old for that:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2003-michalrus_com;
  system = "x86_64-linux";
  pkgs = import nixpkgs { inherit system; };
in import "${nixpkgs}/nixos/lib/eval-config.nix" {
  inherit system pkgs;
  modules = [
    {
      system.nixos.versionSuffix = ".${pkgs.lib.substring 0 8 nixpkgs.lastModifiedDate}.${nixpkgs.shortRev}";
      system.nixos.revision = nixpkgs.rev;
      _module.args = { inherit flake; };
    }
    { networking.hostName = "michalrus_com"; }

    flake.nixosModules.firewall-comments
    flake.nixosModules.dotfiles-old
    flake.nixosModules.dynamic-profiles

    "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
    { ec2.hvm = true; }

    ../_shared_/features/fav-pkgs-cli-thin
    ../_shared_/features/immutable-users
    ../_shared_/features/ip-reject-not-drop
    ../_shared_/features/kill-user-processes
    ../_shared_/features/locale-en-iso
    ../_shared_/features/more-entropy
    ../_shared_/features/mtr-traceroute-fping
    ../_shared_/features/nix.conf
    ../_shared_/features/systemd-accounting
    ../_shared_/features/zsh

    ./features/default.nix

    ./features/gitolite
    ./features/bitlbee
    ./features/kornel
    ./features/openvpn
    ./features/web
    ./features/feeds/annibot.nix
    ./features/feeds/stosowana.nix
    ./features/feeds/rss2email.nix

  ];
}
