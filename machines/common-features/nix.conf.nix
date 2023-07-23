{ flake, config, lib, pkgs, ... }:

lib.mkMerge [

  {
    nix.package = let
      pkg = flake.inputs.nixpkgs.legacyPackages.${pkgs.system}.nixUnstable;
    in assert lib.versionAtLeast pkg.version "2.15.1"; pkg;

    nix.gc.automatic = lib.mkForce false;

    nix.extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    ''
    + lib.optionalString (pkgs.system == "aarch64-darwin") ''
      # Allow building for ‘x86_64-darwin’ using Rosetta 2:
      extra-platforms = x86_64-darwin aarch64-darwin
    '';

    nix.nixPath =
      if pkgs.stdenv.isDarwin
      then lib.mkForce [
        {darwin-config = "${config.environment.darwinConfig}";}
        {nixpkgs = pkgs.path;}
      ]
      else [
        "nixpkgs=${pkgs.path}"
      ];
  }

  {
    nix = if pkgs.stdenv.isLinux && lib.versionAtLeast lib.version "23.05" then {
      channel.enable = false;  # we’re using flakes
    } else {};
  }

  {
    nix = let
      trusted-users = lib.mkForce ["root"];  # disallow poisoning the cache
      auto-optimise-store = true;
    in if pkgs.stdenv.isDarwin || (pkgs.stdenv.isLinux && lib.versionAtLeast lib.version "23.05") then {
      settings.trusted-users = trusted-users;
      settings.auto-optimise-store = auto-optimise-store;
    } else {
      trustedUsers = trusted-users;
      autoOptimiseStore = auto-optimise-store;
    };
  }

  {
    nix = if pkgs.stdenv.isLinux then
      if lib.versionAtLeast lib.version "23.05" then {
        settings.sandbox = lib.mkForce true;
      } else {
        useSandbox = lib.mkForce true;
      }
    else {};
  }

]
