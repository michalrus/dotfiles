super: self:

let

  pkgGit = super.git;
  pkgAnnex = super.gitAndTools.gitAnnex;

  git-post-show-ref = super.writeScriptBin "git" ''
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
      ${pkgGit}/bin/git "''${argv[@]}"

      gitDir=$( ${pkgGit}/bin/git "''${cfg[@]}" rev-parse --git-dir)
      gitRoot=$(${pkgGit}/bin/git "''${cfg[@]}" rev-parse --show-toplevel)
      hook="$gitDir/hooks/post-show-ref"

      cd "$gitRoot"
      if [ -x "$hook" ] ; then
        exec "$hook" "$@"
      fi
    else
      exec ${pkgGit}/bin/git "''${argv[@]}"
    fi
  '';

  git-annex-bin = super.writeScriptBin "git-annex" ''
    #!/bin/sh
    export PATH=${git-post-show-ref}/bin:$PATH
    exec ${pkgAnnex}/bin/git-annex "$@"
  '';

in

super.symlinkJoin {
  inherit (pkgAnnex) name;
  paths = [ git-annex-bin pkgAnnex ];
}
