{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2305;
in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    nixpkgs.nixosModules.notDetected
    ./hardware.nix

    { networking.hostName = "dell-home-server"; }
    { time.timeZone = "Europe/Warsaw"; }
    { system.stateVersion = "23.05"; }

    flake.inputs.agenix.nixosModules.default

    flake.nixosModules.firewall-comments
    flake.nixosModules.dotfiles-old

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

    ./features/users
    ./features/wireguard

    ../_shared_/features/nginx-reasonable
    ./features/nginx

    ./features/openproject
    {
      services.openproject.hostname = "openproject.michalrus.com";
      services.openproject.https = true;
    }

    flake.inputs.home-manager-2305.nixosModules.home-manager
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

    { networking.networkmanager.enable = true; }
  ];
}
