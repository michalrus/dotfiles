{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2305;
in

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    #nixpkgs.nixosModules.notDetected
    ./hardware.nix

    { networking.hostName = "aneta"; }
    { time.timeZone = "UTC"; }
    { system.stateVersion = "23.05"; }

    flake.nixosModules.dotfiles-old
    flake.nixosModules.gnu-screen

    flake.nixosModules.update-raspberry-pi-firmware
    { boot.loader.raspberryPiFirmware.version = 3; }

    ../_shared_/features/fav-pkgs-cli-thin.nix
    ../_shared_/features/immutable-users.nix
    ../_shared_/features/ip-reject-not-drop.nix
    ../_shared_/features/kill-user-processes.nix
    ../_shared_/features/locale-en-iso.nix
    ../_shared_/features/more-entropy.nix
    ../_shared_/features/mtr-traceroute-fping.nix
    ../_shared_/features/nix.conf.nix
    ../_shared_/features/systemd-accounting.nix
    ../_shared_/features/zsh.nix

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

    flake.inputs.home-manager-2305.nixosModules.home-manager
    {
      home-manager = {
        extraSpecialArgs = { inherit flake; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          ../_shared_/home/shells.nix
          ../_shared_/home/gnu-screen.nix
          { home.stateVersion = "23.05"; }
        ];
        users.root.imports = [ ];
        users.m.imports = [ ];
        users.krzyszu.imports = [ ];
      };
    }
  ];
}
