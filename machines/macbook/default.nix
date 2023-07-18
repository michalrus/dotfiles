{ inputs }:

inputs.nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";
  modules = [
    ({
      config,
      pkgs,
      ...
    }: {
      _module.args.pkgsUnstable = import inputs.nixpkgsDarwinUnstable {
        inherit (pkgs) system;
        inherit (config.nixpkgs) config;
      };
    })
    ./configuration.nix
    inputs.home-manager.darwinModules.home-manager
    ({pkgsUnstable, ...}: {
      nixpkgs.config.allowUnfree = true;
      home-manager.extraSpecialArgs.pkgsUnstable = pkgsUnstable;
      home-manager.extraSpecialArgs.nix-doom-emacs = inputs.nix-doom-emacs;
      home-manager.sharedModules = [
        inputs.nix-doom-emacs.hmModule
        (import ./home-common.nix)
        (import ./home-link-darwin-apps.nix)
      ];
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.m = import ./home-m.nix;
      home-manager.users.mw = import ./home-mw.nix;
      home-manager.users.friends = import ./home-friends.nix;
    })
    {
      # home directories for home-manager to pick up; they get messed up b/c of flakes
      users.users.m.home = "/Users/m";
      users.users.mw.home = "/Users/mw";
      users.users.friends.home = "/Users/friends";
    }
  ];
}
