{ lib, fetchurl, writeShellScriptBin, python3, runCommand, rofi, xdotool, writeText

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

  exeName = "rofi-unicode-input${if onlyEmoji then "-emoji" else ""}";

in

writeShellScriptBin exeName ''
  set -euo pipefail

  selected=$(${lib.getExe rofi} <${table} -dmenu -i)

  # This substring() needs to be multibyte-aware:
  character="''${selected:0:1}"

  ${lib.getExe xdotool} type "$character"
''

// {
  meta.platforms = lib.platforms.linux;
  meta.mainProgram = exeName;
}
