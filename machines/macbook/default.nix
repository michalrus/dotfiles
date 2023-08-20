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
          ../_shared_/home/shells
          ../_shared_/home/gnupg
          ../_shared_/home/git
          ../_shared_/home/password-store
          ../_shared_/home/haskell
          ../_shared_/home/gnu-screen
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

    {
      # home directories for home-manager to pick up; they get messed up b/c of flakes
      users.users.m.home = "/Users/m";
      users.users.mw.home = "/Users/mw";
      users.users.friends.home = "/Users/friends";
    }
  ];
}
