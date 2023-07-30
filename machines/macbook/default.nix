{ inputs }:

let flake = inputs.self; in

inputs.nix-darwin-2305.lib.darwinSystem {
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

    flake.inputs.home-manager-2305.darwinModules.home-manager
    {
      home-manager = {
        extraSpecialArgs = { inherit flake; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          flake.inputs.nix-doom-emacs-2305.hmModule
          ../_shared_/home/shells.nix
          ../_shared_/home/gnupg.nix
          ../_shared_/home/git.nix
          ../_shared_/home/password-store.nix
          ../_shared_/home/emacs.nix
          ../_shared_/home/haskell.nix
          ./home/shared.nix
          ./home/link-darwin-apps.nix
        ];
        users.m.imports = [ ../_shared_/home/identity-personal.nix ];
        users.mw.imports = [ ../_shared_/home/identity-work.nix ];
        users.friends.imports = [ ];
      };
    }

    ../_shared_/features/nix.conf.nix
    ../_shared_/features/nix.conf-work-substituters.nix

    {
      # home directories for home-manager to pick up; they get messed up b/c of flakes
      users.users.m.home = "/Users/m";
      users.users.mw.home = "/Users/mw";
      users.users.friends.home = "/Users/friends";
    }
  ];
}
