{ inputs }:

let flake = inputs.self; in

inputs.nix-darwin-2311.lib.darwinSystem {
  system = "aarch64-darwin";
  modules = [
    { _module.args = { inherit flake; }; }
    { nixpkgs.config.allowUnfree = true; }

    {
      networking.hostName = "macbook";
      networking.localHostName = "Michals-MacBook-Pro";  # local network
      networking.computerName = "Michal’s MacBook Pro";  # local network
    }

    ./features/configuration.nix
    ./features/programs-mtr.nix

    flake.inputs.agenix.darwinModules.default
    {
      age.secrets.ssh-key-personal-git-annex = { file = inputs.self + "/secrets/ssh-key-personal-git-annex.age"; owner = "m"; };
      age.secrets.ssh-config-work-devx = { file = inputs.self + "/secrets/ssh-config-work-devx.age"; owner = "mw"; };
      age.secrets.ssh-known_hosts-work-devx = { file = inputs.self + "/secrets/ssh-known_hosts-work-devx.age"; owner = "mw"; };
      age.secrets.ssh-config-work-iog = { file = inputs.self + "/secrets/ssh-config-work-iog.age"; owner = "mw"; };
      age.secrets.ssh-known_hosts-work-iog = { file = inputs.self + "/secrets/ssh-known_hosts-work-iog.age"; owner = "mw"; };
      age.secrets.ssh-config-work-lace = { file = inputs.self + "/secrets/ssh-config-work-lace.age"; owner = "mw"; };
      age.secrets.ssh-known_hosts-work-lace = { file = inputs.self + "/secrets/ssh-known_hosts-work-lace.age"; owner = "mw"; };
    }

    flake.inputs.home-manager-2311.darwinModules.home-manager
    ({ config, ... }: {
      home-manager = {
        extraSpecialArgs = { inherit flake; inherit (config.age) secrets; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          ../_shared_/home/shells
          ../_shared_/home/gnupg
          ../_shared_/home/git
          ../_shared_/home/password-store
          ../_shared_/home/haskell
          ../_shared_/home/gnu-screen
          ../_shared_/home/doom-emacs
          ./home/shared.nix
          ./home/link-darwin-apps.nix
        ];
        users.m.imports = [ ../_shared_/home/identity-personal ];
        users.mw.imports = [ ../_shared_/home/identity-work ];
        users.friends.imports = [ ];
      };
    })

    ../_shared_/features/nix.conf
    ../_shared_/features/nix.conf/work-substituters.nix

    # defaults write NSGlobalDomain KeyRepeat -int 1
    # defaults write NSGlobalDomain InitialKeyRepeat -int 10

    {
      # home directories for home-manager to pick up; they get messed up b/c of flakes
      users.users.m.home = "/Users/m";
      users.users.mw.home = "/Users/mw";
      users.users.friends.home = "/Users/friends";
    }
  ];
}
