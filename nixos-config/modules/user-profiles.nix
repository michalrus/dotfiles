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

    usersWithPackages = filterAttrs (n: u: length u.packages' != 0) config.users.users;

    etcSubdirName = "per-user-packages";

  in {
    environment.profiles = [ "/etc/${etcSubdirName}/$USER" ];

    environment.etc = mapAttrs' (name: { packages', ... }: {
      name = "${etcSubdirName}/${name}";
      value.source = immutableProfile name packages';
    }) usersWithPackages;
  };
}
