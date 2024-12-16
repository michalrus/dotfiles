{ writeShellApplication, hyprland, jq, slurp, grim, wl-clipboard, coreutils, lib
, alsoSaveToDir ? "$HOME/Pictures"  # override with ‘null’ to skip saving
}:

let

  targetDir =
    if alsoSaveToDir == null
    then null
    else ''"'' + lib.replaceStrings [''"''] [''\"''] alsoSaveToDir + ''"'';

in

writeShellApplication {
  name = "hyprland-screenshot";
  runtimeInputs = [ hyprland jq slurp grim wl-clipboard ];
  text = ''
    set -euo pipefail

    ${if targetDir != null then ''
      mkdir -p ${targetDir}
      target=${targetDir}/Screenshot-$(date +%Y%m%d-%H%M%S).png
    '' else ""}

    # Taken from <https://github.com/emersion/slurp/issues/16#issuecomment-2374038213>:
    geometry=$(hyprctl clients -j \
      | jq -r --argjson active \
        "$(hyprctl monitors -j | jq -c '[.[].activeWorkspace.id]')" \
        '.[]
           | select((.hidden | not) and .workspace.id as $id | $active | contains([$id]))
           | "\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"' \
      | slurp)

    grim -g "$geometry" \
      - ${if alsoSaveToDir != null then ''| tee "$target"'' else ""} \
      | wl-copy
  '';
  derivationArgs.meta.description = "Takes a screenshot on Hyprland (has window selection)";
}
