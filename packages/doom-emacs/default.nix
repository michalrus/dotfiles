{ doom-emacs, stdenvNoCC, lib, makeWrapper, pkgs, linkFarm, symlinkJoin, gitMinimal, cacert, emptyFile, writeText

, chosenEmacs    ? pkgs.emacs29
, doomPackagesEl ? "${doom-emacs}/templates/packages.example.el"
, doomInitEl     ? "${doom-emacs}/templates/init.example.el"
, doomConfigEl   ? "${doom-emacs}/templates/config.example.el"
, vendorHash     ? "sha256-VcToMwtvAoGOnpvjcCkkEIoDz33p99NPtoYFL05jZdg="

, doom-data-dir  ? "~/.local/share/doom"
, doom-cache-dir ? "~/.cache/doom"
, doom-state-dir ? "~/.local/state/doom"
}:

assert lib.versionAtLeast chosenEmacs.version "29";  # we need `--init-directory`

let

  doomDir = { withConfig ? true }: linkFarm "doom-dir" {
    "package.el" = doomPackagesEl;
    "init.el" = doomInitEl;
    "config.el" = if withConfig then doomConfigEl else emptyFile;
  };

  vendor = stdenvNoCC.mkDerivation {
    name = "doom-emacs-vendor-${doom-emacs.rev}";

    # These ↓ prevent rebuilds after determining ‘vendorHash’ in the first pass:
    outputHashMode = "recursive";
    outputHash = vendorHash;
    outputHashAlgo = if vendorHash != "" then null else "sha256";
    preferHashedMirrors = true;
    preferLocalBuild = true;

    nativeBuildInputs = [ chosenEmacs gitMinimal ];
    buildCommand = ''
      export HOME=$(mktemp -d)
      mkdir -p ~/.config/emacs
      cp -r ${doom-emacs}/. ~/.config/emacs/
      chmod -R +w ~/.config/emacs/
      patchShebangs ~/.config/emacs
      export NIX_SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
      export DOOMPAGER=cat
      export DOOMDIR=${doomDir { withConfig = false; }}

      ~/.config/emacs/bin/doom install \
        --strict-load ${writeText "no-native-comp.el" ''
          (setq no-native-compile t
                no-byte-compile t
                straight-disable-compile t
                straight-disable-native-compile t
                straight-vc-git-default-clone-depth 1)
        ''} \
        --no-config --no-fonts --force

      # Remove impurities that change too often, but save the source information for later investigation:
      find ~/.config/emacs/.local/straight/repos -type d -name '.git' -prune | while IFS= read -r gitDir ; do
        (
          cd "$(dirname "$gitDir")"
          ( git remote get-url origin ; git rev-parse HEAD ; ) >git-source-info
        )
        rm -rf "$gitDir"
      done

      cp -r ~/.config/emacs/.local/straight/repos $out
    '';
  };

  emacsDir = stdenvNoCC.mkDerivation {
    name = "emacs-dir";
    nativeBuildInputs = [ chosenEmacs gitMinimal ];
    buildCommand = ''
      cp -r ${doom-emacs} $out
      chmod -R +w $out
      patchShebangs $out

      export DOOMPAGER=cat
      export DOOMDIR=${doomDir { withConfig = false; }}
      export EMACSDIR=$out

      # Unfortunately ‘repos’ need to be writable during ‘doom build’:
      mkdir -p        $out/.local/straight
      cp -r ${vendor} $out/.local/straight/repos
      chmod -R +w     $out/.local/straight/repos

      $out/bin/doom install --no-config --no-fonts --force

      rm -rf $out/.local/state

      sed -r '
        s,\(file-name-concat doom-local-dir "etc/"\),'${  lib.escapeShellArg (__toJSON "${doom-data-dir}/" )}',g
        s,\(file-name-concat doom-local-dir "cache/"\),'${lib.escapeShellArg (__toJSON "${doom-cache-dir}/")}',g
        s,\(file-name-concat doom-local-dir "state/"\),'${lib.escapeShellArg (__toJSON "${doom-state-dir}/")}',g
        s,(\(file-name-concat) doom-profile-data-dir ("@"),\1 "'"$out/.local/etc"'" \2,g
      ' -i $out/lisp/doom.el

      sed -r 's,doom-data-dir,"'"$out/.local/etc/"'",g' -i $out/lisp/doom-profiles.el
    '';
    passthru = { inherit vendor; };
  };

  finalEmacs = symlinkJoin {
    name = "doom-emacs";
    meta.mainProgram = "emacs";
    meta.platforms = lib.platforms.linux;
    paths = [ chosenEmacs ];
    nativeBuildInputs = [ makeWrapper ];
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
      for exe in emacs emacs-${chosenEmacs.version} ; do
        rm $out/bin/$exe
        makeWrapper ${chosenEmacs}/bin/$exe $out/bin/$exe \
          --set DOOMDIR ${doomDir { withConfig = true; }} \
          --add-flags --init-directory=${emacsDir}
      done
    '';
  };

in

finalEmacs
