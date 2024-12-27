{ writeShellApplication, grimblast, lib
, alsoSaveToDir ? "$HOME/Pictures"  # override with ‘null’ to skip saving
}:

let

  targetDir =
    if alsoSaveToDir == null
    then null
    else ''"'' + lib.replaceStrings [''"''] [''\"''] alsoSaveToDir + ''"'';

  # Get rid of the black border, because slurp is not fast enough to exit:
  grimblastPatched = grimblast.overrideAttrs (drv: {
    postInstall = (drv.postInstall or "") + ''
      sed -r 's/sleep 0\.2/#\0/g'                       -i $out/bin/.grimblast-wrapped
      sed -r 's/slurp \$SLURP_ARGS/\0 \&\& sleep 0.1/g' -i $out/bin/.grimblast-wrapped
    '';
  });

in

writeShellApplication {
  name = "hyprland-screenshot";
  runtimeInputs = [ grimblastPatched ];
  text = ''
    set -euo pipefail

    ${if targetDir != null then ''
      mkdir -p ${targetDir}
      target=${targetDir}/Screenshot-$(date +%Y%m%d-%H%M%S).png
    '' else ""}

    exec grimblast --freeze copysave area "$target"
  '';
  derivationArgs.meta.description = "Takes a screenshot on Hyprland (has window selection)";
}
