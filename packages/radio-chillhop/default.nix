{
  writeShellApplication,
  coreutils,
  curl,
  jq,
  mpv,
  mpvScripts,
}: let
  mpv' = mpv.override {scripts = with mpvScripts; [mpris];};
in
  writeShellApplication {
    name = "radio-chillhop";
    runtimeInputs = [coreutils curl jq mpv'];
    text = builtins.readFile ./radio.sh;
    derivationArgs.meta.description = "Plays Chillhop.com live streams in the terminal";
  }
