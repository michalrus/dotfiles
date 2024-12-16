{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2411;
in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    { networking.hostName = "michalrus_com"; }

    flake.inputs.agenix.nixosModules.default

    flake.nixosModules.firewall-comments
    flake.nixosModules.dotfiles-old

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

    ../_shared_/features/nginx-reasonable
    ./features/web

    ./features/feeds/annibot.nix
    #./features/feeds/stosowana.nix
    #./features/feeds/rss2email.nix
    ./features/wireguard

    flake.inputs.home-manager-2411.nixosModules.home-manager
    {
      home-manager = {
        extraSpecialArgs = { inherit flake; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          ../_shared_/home/shells
          ../_shared_/home/gnu-screen
          { home.stateVersion = "23.05"; }
        ];
        users.root.imports = [ ];
        users.m.imports = [ ];
      };
    }
  ];
}
