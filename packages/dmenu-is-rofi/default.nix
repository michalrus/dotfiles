{ rofi, writeShellScriptBin, lib }:

writeShellScriptBin "dmenu" ''
  exec ${rofi}/bin/rofi -dmenu "$@"
'' // { meta.platforms = lib.platforms.linux; }
