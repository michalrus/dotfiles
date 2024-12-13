{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2411;
in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    nixpkgs.nixosModules.notDetected
    ./hardware.nix

    { networking.hostName = "monstrum"; }
    { time.timeZone = "UTC"; }
    { system.stateVersion = "24.11"; }

    flake.inputs.agenix.nixosModules.default
    flake.nixosModules.malicious-hosts

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
    ./features/hyprland

    flake.inputs.home-manager-2411.nixosModules.home-manager
    {
      home-manager = {
        extraSpecialArgs = { inherit flake; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          { home.stateVersion = "24.11"; }

          ../_shared_/home/shells
          ../_shared_/home/gnu-screen
          ../_shared_/home/git
          ../_shared_/home/password-store

          ./home/chromium
        ];
      };
    }

    # {
    #   services.printing.enable = true;
    #   services.pipewire = {
    #     enable = true;
    #     pulse.enable = true;
    #   };
    #   networking.firewall.enable = false;
    # }
  ];
}
