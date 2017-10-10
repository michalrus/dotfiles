{ config, lib, pkgs, ... }:

with lib;

{
  options.users.users = mkOption {
    options = [{
      dotfiles = mkOption {
        type = types.listOf types.path;
        default = [];
        description = ''
          List of directories, files of which will be symlinked to
          user’s home directory, when setting up the environment (on
          each login, session etc.).

          It is performed using `cp -srf $src $HOME`. This operation
          has a potential to lose data, as the new symlinks will
          overwrite what is currently residing in those locations.

          Idea: have all users symlinking somewhere to /nix/store, and
          one editing user, to their ~/.dotfiles/.

          Idea: create the /nix/store locations using
          ''${../../dotfiles/base} or similar, to avoid the need to
          provide r:sha256.
        '';
      };
    }];
  };

  config = let

    usersWithDotfiles = attrValues (flip filterAttrs config.users.extraUsers (n: u:
      length u.dotfiles != 0
    ));

    symlinkUser = u: ''
      if [ "$USER" = "${u.name}" ] ; then
        cp --no-preserve=mode --remove-destination --symbolic-link --recursive \
          ${concatMapStringsSep " " (src: ''"${src}/."'' ) u.dotfiles} \
          "$HOME/"
      fi
    '';

  in {
    environment.extraInit = ''
      (
        umask 0077
        ${concatMapStringsSep "\n" symlinkUser usersWithDotfiles}
      )
    '';
  };
}
