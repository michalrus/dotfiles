{ stdenv, git, gitAndTools, writeScriptBin, symlinkJoin, fetchurl, writeScript, writeText}:

let

  inherit (gitAndTools) gitAnnex;

  git-annex = let
    git-post-show-ref = writeScriptBin "git" ''
      #!/usr/bin/env bash

      set -o errexit -o nounset -o noclobber -o pipefail

      argv=("$@")
      cfg=()
      cmd=

      while [ $# -gt 0 ] ; do
        case "$1" in
          -C) cfg+=($1) ; shift ;;
          -c) cfg+=($1) ; shift ;;
          -*) ;;
          *) cmd=$1 ; shift ; break ;;
        esac
        cfg+=($1) ; shift
      done

      if [ "$cmd" = "show-ref" ] ; then
        ${git}/bin/git "''${argv[@]}"

        gitDir=$( ${git}/bin/git "''${cfg[@]}" rev-parse --git-dir)
        gitRoot=$(${git}/bin/git "''${cfg[@]}" rev-parse --show-toplevel)
        hook="$gitDir/hooks/post-show-ref"

        cd "$gitRoot"
        if [ -x "$hook" ] ; then
          exec "$hook" "$@"
        fi
      else
        exec ${git}/bin/git "''${argv[@]}"
      fi
    '';

    git-annex-bin = writeScriptBin "git-annex" ''
      #!/bin/sh
      export PATH=${git-post-show-ref}/bin:$PATH
      exec ${gitAnnex}/bin/git-annex "$@"
    '';
  in
  symlinkJoin {
    inherit (gitAnnex) name;
    paths = [ git-annex-bin gitAnnex git-annex-desktop ];
  };

  git-annex-desktop = let
    script = writeScript "script" ''
      #!/bin/sh
      cd "$HOME"
      as="$HOME/.config/git-annex/autostart"
      pwd
      [ -f "$as" ] && cd "$(head -n 1 "$as")"
      pwd
      exec /run/current-system/sw/bin/git-annex webapp
    '';

    launcher = writeText "launcher" ''
      [Desktop Entry]
      Name=git-annex webapp
      Exec=${script}
      Icon=git-annex
      Type=Application
      Terminal=false
    '';

    logo = fetchurl {
      url = https://git-annex.branchable.com/logo.svg;
      sha256 = "0wrzllbb39l6zkmf7r0migwqdvcinjkhln24953cxyv1g6jrsarh";
    };
  in
  stdenv.mkDerivation {
    name = "git-annex-desktop";

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/share/applications
      mkdir -p $out/share/icons/hicolor/scalable/apps
      ln -s ${launcher} $out/share/applications/git-annex.desktop
      ln -s ${logo} $out/share/icons/hicolor/scalable/apps/git-annex.svg
    '';
  };

in git-annex
