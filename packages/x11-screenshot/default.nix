{ lib, writeShellScriptBin, shotgun, hacksaw, coreutils, xclip
, alsoSaveToDir ? "$HOME/Pictures"  # override with ‘null’ to skip saving
}:

let

  targetDir =
    if alsoSaveToDir == null
    then null
    else ''"'' + lib.replaceStrings [''"''] [''\"''] alsoSaveToDir + ''"'';

in writeShellScriptBin "x11-screenshot" ''
  set -euo pipefail

  ${if targetDir != null then ''
    mkdir -p ${targetDir}
    target=${targetDir}/Screenshot-$(date +%Y%m%d-%H%M%S).png
  '' else ""}

  ${lib.getExe shotgun} \
    $(${lib.getExe hacksaw} -f "-i %i -g %g") \
    - ${if alsoSaveToDir != null then ''| ${coreutils}/bin/tee "$target"'' else ""} \
    | ${lib.getExe xclip} -t 'image/png' -selection clipboard
''
