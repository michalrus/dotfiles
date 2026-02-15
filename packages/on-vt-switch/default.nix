{
  stdenv,
  lib,
}: let
  name = "on-vt-switch";
in
  stdenv.mkDerivation {
    inherit name;

    src =
      builtins.filterSource (
        path: type: let b = baseNameOf path; in b == "${name}.c" || b == "Makefile"
      )
      ./.;

    installPhase = ''
      mkdir -p $out/bin
      cp ${name} $out/bin
    '';

    meta.platforms = lib.platforms.linux;
  }
