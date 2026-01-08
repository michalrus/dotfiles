{ lib, fetchurl, writeShellApplication, jq, python3, runCommand, fuzzel, wtype, wl-clipboard, writeText

, onlyEmoji ? false

}:

let

  raw-unicode = fetchurl {
    url = "https://www.unicode.org/Public/UNIDATA/UnicodeData.txt";
    hash = "sha256-gG6a7WUDcZfx7IXhK+bozYcPxWCLTeD//ZkPaJ83anM=";
  };

  raw-emoji = fetchurl {
    url = "https://github.com/muan/emojilib/raw/refs/tags/v4.0.2/dist/emoji-en-US.json";
    hash = "sha256-PjrIs6OhLkFIV++80GwcdPtFeEjlZezeM3LP+Ca/wDI=";
  };

  table-unicode = runCommand "unicode-table" {} ''
    ${lib.getExe python3} <${raw-unicode} ${writeText "transform.py" ''
      import sys
      for line in sys.stdin:
        try:
          fields = line.split(';')
          codepoint = fields[0]
          codepoint_int = int(codepoint, 16)
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

  table-emoji = runCommand "emoji-table" {
    nativeBuildInputs = [jq];
  } ''
    jq --raw-output '
        . | to_entries | .[] | .key + "\t" + (.value | join(" · ") | sub("_"; " "; "g"))
      ' \
      <${raw-emoji} \
      >$out
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

    selected=$(
      fuzzel \
        <${if onlyEmoji then table-emoji else table-unicode} \
        --dmenu --width=100 \
        --cache="$cacheDir"/${if onlyEmoji then "emoji" else "default"} \
        --accept-nth=1 \
      | tr -d '\n'
    )

    printf '%s' "''${selected}" | wtype -
  '';
  derivationArgs.meta.description = "${if onlyEmoji then "Emoji" else "Unicode"} selector on Wayland";
  derivationArgs.meta.platforms = lib.platforms.linux;
}
