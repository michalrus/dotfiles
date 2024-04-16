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

    flake.inputs.home-manager-2311.darwinModules.home-manager
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
          ../_shared_/home/doom-emacs
          ./home/shared.nix
          ./home/link-darwin-apps.nix
        ];
        users.m.imports = [ ../_shared_/home/identity-personal ];
        users.mw.imports = [ ../_shared_/home/identity-work ];
        users.friends.imports = [ ];
      };
    }

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
