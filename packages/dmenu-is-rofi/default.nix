{ rofi, writeShellScriptBin }:

writeShellScriptBin "dmenu" ''
  exec ${rofi}/bin/rofi -dmenu "$@"
''
