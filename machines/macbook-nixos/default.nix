{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2311;
in

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    ./hardware.nix

    { networking.hostName = "macbook-nixos"; }
    { time.timeZone = "Europe/Warsaw"; }
    { system.stateVersion = "23.11"; }

    flake.inputs.agenix.nixosModules.default

  ] ++ (with flake.nixosModules; [

    dotfiles-old
    malicious-hosts

  ]) ++ [

    ../_shared_/features/fav-pkgs-cli-thin
    ../_shared_/features/immutable-users
    ../_shared_/features/kill-user-processes
    ../_shared_/features/locale-en-iso
    ../_shared_/features/more-entropy
    ../_shared_/features/mtr-traceroute-fping
    ../_shared_/features/nix.conf/work-substituters.nix
    ../_shared_/features/nix.conf
    ../_shared_/features/systemd-accounting
    ../_shared_/features/zsh

    ./users.nix

    flake.inputs.home-manager-2311.nixosModules.home-manager
    {
      home-manager = {
        extraSpecialArgs = { inherit flake; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          ../_shared_/home/shells
          ../_shared_/home/gnupg
          ../_shared_/home/git
          ../_shared_/home/password-store
          ../_shared_/home/haskell
          ../_shared_/home/gnu-screen
          { home.stateVersion = "23.11"; }
        ];
        users.m.imports = [ ../_shared_/home/identity-personal ];
        users.mw.imports = [ ../_shared_/home/identity-work ];
        users.root.imports = [ ];
      };
    }

    { security.pam.services.su.requireWheel = true; }

    # FIXME: get rid of ↓
    { environment.variables.PATH = [ "$HOME/.bin" ]; }

    {
      services.openssh.enable = true;

      # using gpg-agent as ssh-agent
      programs.ssh.startAgent = false;

      networking.useDHCP = true;
      networking.nameservers = [
        "1.1.1.1"
        "1.0.0.1"
      ];

      services.journald.extraConfig = ''
        SystemMaxUse=200M
      '';
    }
  ];
}
