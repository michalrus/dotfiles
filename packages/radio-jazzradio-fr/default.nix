{
  writeShellApplication,
  coreutils,
  curl,
  jq,
  gnugrep,
  gnused,
  skim,
  mpv,
  mpvScripts,
}: let
  mpv' = mpv.override {scripts = with mpvScripts; [mpris];};
in
  writeShellApplication {
    name = "radio-jazzradio-fr";
    runtimeInputs = [coreutils curl jq gnugrep gnused skim mpv'];
    text = builtins.readFile ./radio-jazzradio-fr.sh;
    derivationArgs.meta.description = "Plays jazzradio.fr webradios in the terminal";
  }
