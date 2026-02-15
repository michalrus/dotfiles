{
  writeShellApplication,
  grimblast,
  lib,
  alsoSaveToDir ? "$HOME/Pictures", # override with ‘null’ to skip saving
}: let
  targetDir =
    if alsoSaveToDir == null
    then null
    else ''"'' + lib.replaceStrings [''"''] [''\"''] alsoSaveToDir + ''"'';
in
  # XXX: Remember to add these to `hyprland.conf` to remove the black border around screenshots:
  #
  # layerrule = noanim, hyprpicker
  # layerrule = noanim, selection
  writeShellApplication {
    name = "hyprland-screenshot";
    runtimeInputs = [grimblast];
    text = ''
      set -euo pipefail

      ${
        if targetDir != null
        then ''
          mkdir -p ${targetDir}
          target=${targetDir}/Screenshot-$(date +%Y%m%d-%H%M%S).png
        ''
        else ""
      }

      exec grimblast --freeze copysave area "$target"
    '';
    derivationArgs.meta.description = "Takes a screenshot on Hyprland (has window selection)";
  }
