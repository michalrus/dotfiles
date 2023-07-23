{ inputs }:

# FIXME: use: lib.nixosSystem, but the current nixpkgs-aneta are too old for that:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-michalrus_com;
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
