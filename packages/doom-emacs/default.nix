{
  doom-emacs,
  stdenvNoCC,
  lib,
  makeWrapper,
  pkgs,
  linkFarm,
  symlinkJoin,
  gitMinimal,
  cacert,
  emptyFile,
  emptyDirectory,
  chosenEmacs ? pkgs.emacs30-pgtk,
  doomPackagesEl ? "${doom-emacs}/static/packages.example.el",
  doomInitEl ? "${doom-emacs}/static/init.example.el",
  doomConfigEl ? "${doom-emacs}/static/config.example.el",
  vendorHash ? "sha256-xajw0wwjPRVjSlN+zcWW/kviRyQNvXdL88UJvjjG37I=",
  doom-data-dir ? "~/.local/share/doom",
  doom-cache-dir ? "~/.cache/doom",
  doom-state-dir ? "~/.local/state/doom",
}:
assert lib.versionAtLeast chosenEmacs.version "29"; # we need `--init-directory`

  let
    doomDir = {withConfig ? true}:
      linkFarm "doom-dir" {
        "packages.el" = doomPackagesEl;
        "init.el" = doomInitEl;
        "config.el" =
          if withConfig
          then doomConfigEl
          else emptyFile;
        "snippets" = emptyDirectory;
      };

    # Short hash of user config files that affect vendored packages.
    # Embedding it in the derivation name ensures that any change to
    # packages.el or init.el invalidates the old vendorHash — so you
    # get an immediate build failure reminding you to update it.
    doomConfigFingerprint = builtins.substring 0 8 (builtins.hashString "sha256"
      (builtins.hashFile "sha256" doomPackagesEl
        + builtins.hashFile "sha256" doomInitEl));

    vendor = stdenvNoCC.mkDerivation {
      name = "doom-emacs-vendor-${doom-emacs.rev}-${doomConfigFingerprint}";

      # These ↓ prevent rebuilds after determining ‘vendorHash’ in the first pass:
      outputHashMode = "recursive";
      outputHash = vendorHash;
      outputHashAlgo =
        if vendorHash != ""
        then null
        else "sha256";
      preferHashedMirrors = true;
      preferLocalBuild = true;

      nativeBuildInputs = [chosenEmacs gitMinimal];
      buildCommand = ''
        export HOME=$(mktemp -d)
        mkdir -p ~/.config/emacs
        cp -r ${doom-emacs}/. ~/.config/emacs/
        chmod -R +w ~/.config/emacs/
        patchShebangs ~/.config/emacs
        export NIX_SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
        export DOOMPAGER=cat
        export DOOMDIR=${doomDir {withConfig = false;}}

        ~/.config/emacs/bin/doom install --force

        # Remove impurities that change too often, but save the source information for later investigation:
        find ~/.config/emacs/.local/straight/repos -type d -name '.git' -prune | while IFS= read -r gitDir ; do
          (
            cd "$(dirname "$gitDir")"
            ( git remote get-url origin ; git show-ref | grep $(git rev-parse HEAD) ; ) >git-source-info
          )
          rm -rf "$gitDir"
        done

        cp -r ~/.config/emacs/.local/straight/repos $out
      '';
    };

    fake-git = pkgs.writeShellScriptBin "git" ''
      if [ "$1" == "-C" ] && [ "$3" == "log" ] && [ "$4" == "-1" ] && [ "$5" == "--format=%D %h %ci" ] ; then
        echo 'HEAD -> master, origin/master, origin/HEAD 0000000 1970-01-01 00:00:00 +0000'
      else
        exit 1
      fi
    '';

    emacsDir = stdenvNoCC.mkDerivation {
      name = "emacs-dir";
      nativeBuildInputs = [chosenEmacs fake-git];
      buildCommand = ''
        cp -r ${doom-emacs} $out
        chmod -R +w $out
        patchShebangs $out

        export DOOMPAGER=cat
        export DOOMDIR=${doomDir {withConfig = false;}}
        export EMACSDIR=$out

        # Unfortunately ‘repos’ need to be writable during ‘doom build’:
        mkdir -p        $out/.local/straight
        cp -r ${vendor} $out/.local/straight/repos
        chmod -R +w     $out/.local/straight/repos

        find "$out/.local/straight/repos" -mindepth 1 -maxdepth 1 -type d -exec mkdir -p {}/.git \;

        # Or else, the true `config.el` won’t be loaded:
        sed -r '/module-list-loader post-config-modules config-file/a\              (doom-load (expand-file-name "config" (getenv "DOOMDIR")))' \
          -i $out/lisp/lib/profiles.el

        $out/bin/doom install --force
        $out/bin/doom sync -U --force

        rm -rf $out/.local/{env,state}

        sed -r '
          s,\(file-name-concat doom-local-dir "etc/"\),'${lib.escapeShellArg (builtins.toJSON "${doom-data-dir}/")}',g
          s,\(file-name-concat doom-local-dir "cache/"\),'${lib.escapeShellArg (builtins.toJSON "${doom-cache-dir}/")}',g
          s,\(file-name-concat doom-local-dir "state/"\),'${lib.escapeShellArg (builtins.toJSON "${doom-state-dir}/")}',g
          s,(\(file-name-concat) doom-profile-data-dir ("@"),\1 "'"$out/.local/etc"'" \2,g
        ' -i $out/lisp/doom.el

        sed -r 's,(file-name-concat) doom-data-dir (name "@"),\1 "'"$out/.local/etc/"'" \2,g' -i $out/lisp/doom-lib.el

        rm -r $out/.local/straight/repos
        ln -sf ${vendor} $out/.local/straight/repos
      '';
    };

    finalEmacs = symlinkJoin {
      name = "doom-emacs";
      meta.mainProgram = "emacs";
      meta.platforms = lib.platforms.linux ++ lib.platforms.darwin;
      paths = [chosenEmacs];
      nativeBuildInputs = [makeWrapper];
      passthru = {
        inherit vendor emacsDir;
        doomDir = doomDir {withConfig = false;};
      };
      postBuild = ''
        ( cd $out ; grep -RF --binary-files=without-match ${chosenEmacs} . ) \
          | cut -d: -f1 | sort --unique | grep -vF './share/emacs/${chosenEmacs.version}/src/' \
          | while IFS= read -r target ; do
          source="${chosenEmacs}/$target"
          target="$out/$target"
          rm "$target"
          cp -a "$source" "$target"
          sed -r 's,${chosenEmacs},'$out',g' -i "$target"
        done
        for exe in bin/emacs bin/emacs-${chosenEmacs.version} ${
          if stdenvNoCC.isDarwin
          then "Applications/Emacs.app/Contents/MacOS/Emacs"
          else ""
        } ; do
          rm $out/$exe
          makeWrapper ${chosenEmacs}/$exe $out/$exe \
            --set DOOMDIR ${doomDir {withConfig = true;}} \
            --set EMACSDIR ${emacsDir} \
            --add-flags --init-directory=${emacsDir}
        done
      '';
    };
  in
    finalEmacs
