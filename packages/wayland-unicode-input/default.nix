{ lib, fetchurl, writeShellApplication, python3, runCommand, fuzzel, wtype, wl-clipboard, writeText

, onlyEmoji ? false

}:

let

  raw = fetchurl {
    url = "https://www.unicode.org/Public/UNIDATA/UnicodeData.txt";
    hash = "sha256-gG6a7WUDcZfx7IXhK+bozYcPxWCLTeD//ZkPaJ83anM=";
  };

  table = runCommand "unicode-table" {} ''
    ${lib.getExe python3} <${raw} ${writeText "transform.py" ''
      import sys
      emojiLo = int('1F100', 16)
      emojiHi = int('1FAFF', 16)
      onlyEmoji = ${if onlyEmoji then "True" else "False"}
      for line in sys.stdin:
        try:
          fields = line.split(';')
          codepoint = fields[0]
          codepoint_int = int(codepoint, 16)
          if onlyEmoji and (codepoint_int < emojiLo or codepoint_int > emojiHi):
            continue
          name = fields[1].lower()
          altname = fields[10]
          rune = chr(codepoint_int)
          if len(altname) > 0:
            altname = " (" + altname.lower() + ")"
          print(rune + "\t" + "U+" + codepoint + "\t" + name + altname)
        except UnicodeEncodeError as e:
          continue
    ''} >$out
  '';

  exeName = "wayland-unicode-input${if onlyEmoji then "-emoji" else ""}";

in

writeShellApplication {
  name = exeName;
  runtimeInputs = [ fuzzel wtype wl-clipboard ];
  text = ''
    set -euo pipefail

    cacheDir="$HOME"/.cache/wayland-unicode-input
    mkdir -p "$cacheDir"

    selected=$(fuzzel <${table} --dmenu --width=100 --cache="$cacheDir"/${if onlyEmoji then "emoji" else "default"})

    # This substring() needs to be multibyte-aware:
    character="''${selected:0:1}"

    # `wtype` doesn’t work directly with Chromium, see
    # <https://github.com/atx/wtype/issues/31>, so let’s go via `wl-clipboard`:
    saved_clipboard=$(mktemp -p "$XDG_RUNTIME_DIR")
    trap 'rm "$saved_clipboard"' EXIT

    do_restore=1
    wl-paste --no-newline >"$saved_clipboard" || do_restore=

    wl-copy --trim-newline <<<"$character"
    wtype -M shift -k insert -m shift

    if [ -n "$do_restore" ] ; then
      wl-copy <"$saved_clipboard"
    fi
  '';
  derivationArgs.meta.description = "Takes a screenshot on Hyprland (has window selection)";
  derivationArgs.meta.platforms = lib.platforms.linux;
}
