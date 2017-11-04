{ config, lib, pkgs, ... }:

with lib;

let

  proxyDirName = ".nix-immutable-dotfiles";

in {
  options.users.users = mkOption {
    options = [{
      dotfiles = {
        base = mkOption {
          type = types.path;
          default = ../../dotfiles;
          description = ''
            The `base` for the selected `profiles`.

            If this `isStorePath`, the dotfiles will be linked
            immutably, via `~/${proxyDirName}` proxy to the Nix store;
            so that removed dotfiles become broken symlinks in the new
            system. Without this proxy, they would only become broken
            after performing GC.

            If not a store path (probably your `~/.dotfiles`), they
            will be linked mutably and you can freely change them.
            Since you’re modifying that mutable location, deleted
            dotfiles will correctly become broken symlinks, as well.

            All symlinking in actual user’s home directory happens in
            `/etc/profile` (when setting up the environment, on each
            login, session etc.).

            It is performed using `cp -srf $src $HOME` (more or
            less). This operation will potentially lose data, as the
            new symlinks will overwrite what is currently residing in
            those locations.
          '';
        };

        profiles = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            List of directory roots under `base`, files of which will
            be indirectly (immutable `base`) or directly (mutable
            `base`) symlinked to user’s home directory.
          '';
        };
      };
    }];
  };

  config = let

    usersWithDotfiles = attrValues (flip filterAttrs config.users.users (n: u:
      length u.dotfiles.profiles != 0
    ));

    symlinkCmd = srcs: target: ''
      cp --no-preserve=mode --remove-destination --symbolic-link --recursive \
        ${escapeShellArgs (map (src: "${src}/.") srcs)} \
        "${target}/"
    '';

    mkImmutableProxy = srcs: pkgs.runCommand "immutable-dotfiles" {} ''
      mkdir -p $out
      ${symlinkCmd srcs "$out"}
    '';

    symlinkUser = u: ''
      if [ "$USER" = "${u.name}" ] ; then
        ${let srcs = map (profile: "${u.dotfiles.base}/${profile}") u.dotfiles.profiles;
              homeDirName = "${u.home}/${proxyDirName}";
              immutableProxy = mkImmutableProxy srcs;
          in if isStorePath "${u.dotfiles.base}"
               then ''
                 ln -sfT ${immutableProxy} "${homeDirName}"
                 ${symlinkCmd [homeDirName] u.home}
               ''
               else symlinkCmd srcs u.home
          }
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
