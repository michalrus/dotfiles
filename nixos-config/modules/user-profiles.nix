{ config, lib, pkgs, ... }:

with lib;

{
  options.users.users = mkOption {
    options = [{
      packages = mkOption {
        type = types.listOf types.path;
        default = [];
        description = ''
          List of packages to add to user’s `PATH`. A sort of an
          immutable `nix-env -i`. Quite primitive.
        '';
      };
    }];
  };

  config = let

    homeDirName = "$HOME/.nix-immutable-profile";

    usersWithPackages = attrValues (flip filterAttrs config.users.extraUsers (n: u:
      length u.packages != 0
    ));

    immutableProfile = paths:
      pkgs.buildEnv {
        name = "immutable-profile";
        inherit paths;
        inherit (config.environment) pathsToLink extraOutputsToInstall;
        inherit (config.system.path) ignoreCollisions postBuild;
      };

    symlinkUser = u: ''
      if [ "$USER" = "${u.name}" ] ; then
        ln -sfT ${immutableProfile u.packages} "${homeDirName}"
      fi
    '';

  in {
    environment.profiles = [ "${homeDirName}" ];

    environment.extraInit = ''
      (
        umask 0077
        ${concatMapStringsSep "\n" symlinkUser usersWithPackages}
      )
    '';
  };
}
