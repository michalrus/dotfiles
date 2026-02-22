{
  config,
  lib,
  pkgs,
  ...
}: {
  nix = lib.mkMerge [
    {
      gc.automatic = lib.mkForce false;

      extraOptions =
        ''
          experimental-features = nix-command flakes fetch-closure
          keep-outputs = true
          keep-derivations = true
        ''
        + lib.optionalString (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") ''
          # Allow building for ‘x86_64-darwin’ using Rosetta 2:
          extra-platforms = x86_64-darwin aarch64-darwin
        '';

      nixPath = lib.mkForce (
        if pkgs.stdenv.isDarwin
        then [
          {darwin-config = "${config.environment.darwinConfig}";}
          {nixpkgs = pkgs.path;}
        ]
        else [
          "nixpkgs=${pkgs.path}"
        ]
      );
    }

    (let
      trusted-users = lib.mkForce ["root"]; # disallow poisoning the cache
      auto-optimise-store = true;
    in
      if pkgs.stdenv.isDarwin || (pkgs.stdenv.isLinux && lib.versionAtLeast lib.version "23.04")
      then {
        settings.trusted-users = trusted-users;
        settings.auto-optimise-store = auto-optimise-store;
      }
      else {
        trustedUsers = trusted-users;
        autoOptimiseStore = auto-optimise-store;
      })

    (
      if pkgs.stdenv.isLinux
      then
        if lib.versionAtLeast lib.version "23.04"
        then {
          settings.sandbox = lib.mkForce true; # "relaxed" allows usage of `__noChroot = true;` for testing
        }
        else {
          useSandbox = lib.mkForce true;
        }
      else {}
    )

    {
      # Unfortunately, ‘false’ is broken w.r.t. ‘pkgs.buildEnv’ inheriting ‘config.system.path.postBuild’
      # – e.g. per-user packages.
      #channel.enable = false;  # we’re using flakes
    }
  ];
}
