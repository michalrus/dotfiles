{
  lib,
  writeShellApplication,
  writeTextDir,
  skim,
  mpv,
  mpvScripts,
  accuradio,
  yq,
}: let
  mpv' = mpv.override {scripts = with mpvScripts; [mpris];};
  radioScript =
    lib.replaceStrings
    [
      "@mpvMuteScript@"
      "@stationsYaml@"
    ]
    [
      "${writeTextDir "mute-script.lua" (builtins.readFile ./mpv-mute-script.lua)}/mute-script.lua"
      "${./stations.yml}"
    ]
    (builtins.readFile ./radio.sh);
in
  writeShellApplication {
    name = "radio";
    runtimeInputs = [skim mpv' accuradio yq];
    text = radioScript;
    derivationArgs.meta.description = "Interactive terminal radio menu";
  }
