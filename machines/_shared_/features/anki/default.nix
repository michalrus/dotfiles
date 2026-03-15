{
  lib,
  pkgs,
  ...
}: let
  ankiPackage = pkgs.anki;
in {
  environment.systemPackages = [
    ankiPackage
    # Separate Anki instance for testing without risking the main collection(s)
    (pkgs.writeShellApplication {
      name = "anki-dev";
      text = ''
        export ANKI_BASE="$HOME"/.local/share/Anki2-dev
        exec ${lib.getExe ankiPackage} -b "$ANKI_BASE" "$@"
      '';
    })
  ];
}
