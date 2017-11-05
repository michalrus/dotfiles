{ config, lib, pkgs, ... }:

with lib;

{
  options.users.users = mkOption {
    options = [{
      # Named `packages'`, because NixOS ≥ 17.09 has this, but w/ problems, cf. https://github.com/NixOS/nixpkgs/issues/31253
      packages' = mkOption {
        type = types.listOf types.path;
        default = [];
        description = ''
          List of packages to add to user’s profile. A sort of an
          immutable `nix-env -i`.
        '';
      };
    }];
  };

  config = let

    immutableProfile = name: paths:
      pkgs.buildEnv {
        name = "immutable-profile-${name}";
        inherit paths;
        inherit (config.environment) pathsToLink extraOutputsToInstall;
        inherit (config.system.path) ignoreCollisions postBuild;
      };

    # Named `profiles-`, because Nixpkgs will use `profiles` in the future, cf. https://github.com/NixOS/nixpkgs/pull/3123
    etcDirName = "profiles-/per-user";

  in {
    environment.profiles = [ "/etc/${etcDirName}/$USER" ];

    environment.etc = mapAttrs' (n: u: {
      name = "${etcDirName}/${n}";
      value.source = immutableProfile n u.packages';
    }) (filterAttrs (n: u: length u.packages' != 0) config.users.users);
  };
}
