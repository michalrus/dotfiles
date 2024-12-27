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

    # For slurp’s black border to disappear, see
    # <https://github.com/hyprwm/contrib/blob/d7c55140f1785b8d9fef351f1cd2a4c9e1eaa466/grimblast/grimblast#L217>:
    sleep 0.1

    grim -g "$geometry" \
      - ${if alsoSaveToDir != null then ''| tee "$target"'' else ""} \
      | wl-copy --type image/png
  '';
  derivationArgs.meta.description = "Takes a screenshot on Hyprland (has window selection)";
}
