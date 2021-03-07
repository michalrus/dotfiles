{ pkgs ? (import <nixpkgs> {}) }:

let name = "on-vt-switch"; in

pkgs.stdenv.mkDerivation {
  inherit name;

  src = builtins.filterSource (
    path: type: let b = baseNameOf path; in b == "${name}.c" || b == "Makefile"
  ) ./.;

  installPhase = ''
    mkdir -p $out/bin
    cp ${name} $out/bin
  '';
}
