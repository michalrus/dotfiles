{ config, lib, pkgs, ... }:

with lib;

let

  etcDirName = "dotfiles-old/per-user";

in {
  options.users.users = mkOption {
    type = (if lib.versionAtLeast lib.version "23.04"
            then lib.types.attrsOf
            else lib.types.loaOf) (lib.types.submodule {
      options.dotfiles-old = {
        base = mkOption {
          type = types.path;
          default = ../../dotfiles;
          description = ''
            The `base` for the selected `profiles`.

            If this `isStorePath`, the dotfiles will be linked
            immutably, via `/etc/${etcDirName}/$USER` proxy to the Nix
            store; so that removed dotfiles become broken symlinks in
            the new system. Without this proxy, they would only become
            broken after performing GC.

            If not a store path (probably your `~/.dotfiles`), they
            will be linked mutably and you can freely change them.
            Since you’re modifying that mutable location, deleted
            dotfiles will correctly become broken symlinks, as well.

            All symlinking in the actual user’s home directory happens
            in `/etc/profile` (when setting up the environment, on
            each login, session etc.).

            It is performed using `cp -srf $src $HOME` (more or
            less). But prior to that, regular files, that would be
            overwritten, are backed up.
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
    });
  };

  config = let

    usersWithMutableDotfiles = filterAttrs  (n: u:
      length u.dotfiles-old.profiles != 0
      && !isStorePath "${u.dotfiles-old.base}"
    ) config.users.users;

    usersWithImmutableDotfiles = filterAttrs (n: u:
      length u.dotfiles-old.profiles != 0
      && isStorePath "${u.dotfiles-old.base}"
    ) config.users.users;

    symlinkCmd = srcs: target: ''
      # Let’s backup non-symlink files.
      (
        cd "${target}/"
        find \
          $( ${concatMapStringsSep " " (src: ''cd "${src}" ; find -not -type d ; '') srcs} ) \
          -type f -exec mv -v {} {}.$(date -Ins) \;
      ) 2>/dev/null || true

      # Display errors, but always return 0, since non-0 in `/profile` is rather problematic… in various ways.
      cp --no-preserve=mode --remove-destination --symbolic-link --recursive \
        ${concatMapStringsSep " " (src: ''"${src}/."'') srcs} \
        "${target}/" || true
    '';

    srcs = u: map (profile: "${u.dotfiles-old.base}/${profile}") u.dotfiles-old.profiles;

  in {
    environment.etc = mapAttrs' (n: u: {
      name = "${etcDirName}/${n}";
      value.source =
        # Both `pkgs.buildEnv` and `pkgs.symlinkJoin` do slightly
        # wrong things here. The user, within `srcs`, wants to be able
        # to symlink to files potentially non-symlinked in
        # `etcDirName`. Therefore we can’t resolve their symlinks
        # prior to our linking, as both of these commands do. Let’s
        # just blindly link to what the user supplies in `srcs`.
        pkgs.runCommand "dotfiles-old-${n}" {} ''
          mkdir -p $out
          ${symlinkCmd (srcs u) "$out"}
        '';
    }) usersWithImmutableDotfiles;

    environment.extraInit = ''
      (
        umask 0077
        if [ -e "/etc/${etcDirName}/$USER" ] ; then
          ${symlinkCmd ["/etc/${etcDirName}/$USER"] "$HOME"}
        else
          : # no-op in case of no mutable dotfiles
          ${concatMapStringsSep "\n" (u: ''
            if [ "$USER" = "${u.name}" ] ; then
              ${symlinkCmd (srcs u) "$HOME"}
            fi
          '') (attrValues usersWithMutableDotfiles)}
        fi
      )
    '';
  };
}
