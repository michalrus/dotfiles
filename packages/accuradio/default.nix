{ writeShellApplication, coreutils, util-linux, skim, gnused, gnugrep, curl, jq, mpv, mpvScripts }:

let
  mpv' = mpv.override { scripts = with mpvScripts; [ mpris ]; };
in

writeShellApplication {
  name = "accuradio";
  runtimeInputs = [ coreutils util-linux skim gnused gnugrep curl jq mpv' ];
  text = builtins.readFile ./accuradio.sh;
  derivationArgs.meta.description = "Plays AccuRadio.com in the terminal";
}
