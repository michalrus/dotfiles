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
    flake.nixosModules.lock-vts

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
    ../_shared_/features/hyprland
    ../_shared_/features/fonts
    ../_shared_/features/desktop-apps

    ./features/wireguard-michalrus
    ../_shared_/features/nginx-reasonable
    ./features/nginx
    ./features/openproject
    {
      services.openproject.hostname = "openproject.michalrus.com";
      services.openproject.https = true;
    }

    ./features/torrents
    ./features/users

    ./features/firewall
    ./features/connmon
    ./features/nordvpn
    ./features/dns-bind
    ./features/dhcp
    ./features/microsocks

    flake.inputs.home-manager-2411.nixosModules.home-manager
    {
      home-manager = {
        extraSpecialArgs = { inherit flake; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          { home.stateVersion = "24.11"; }

          ../_shared_/home/shells
          ../_shared_/home/gnupg
          ../_shared_/home/gnu-screen
          ../_shared_/home/git
          ../_shared_/home/password-store
          ../_shared_/home/mpv
          ../_shared_/home/alacritty
          ../_shared_/home/chromium
        ];
      };
    }

    ({ pkgs, ... }: {
      services.pipewire = {
        enable = true;
        pulse.enable = true;
      };
      environment.systemPackages = [ pkgs.pavucontrol ];
    })

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
