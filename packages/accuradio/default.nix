{ writeShellApplication, coreutils, util-linux, skim, gnused, gnugrep, curl, jq, mpv }:

writeShellApplication {
  name = "accuradio";
  runtimeInputs = [ coreutils util-linux skim gnused gnugrep curl jq mpv ];
  text = builtins.readFile ./accuradio.sh;
  derivationArgs.meta.description = "Plays AccuRadio.com in the terminal";
}
