{
  stdenv,
  lib,
  fetchFromGitHub,
  autoreconfHook,
  flex,
  bison,
  fontforge,
  texlive,
  python3,
  which,
}: let
  version = "6.1.0";

  pname = "gregorio";

  pkg = stdenv.mkDerivation {
    inherit pname version;

    src = fetchFromGitHub {
      owner = "gregorio-project";
      repo = "gregorio";
      rev = "v" + version;
      hash = "sha256-GzodkL0CFq6zYsJrHe05hHpl9mmm/moaKAAte/AYqFg=";
    };
    TEXHASH = "true"; # we don’t use neither ‘texhash’ nor ‘mktexlsr’ in Nixpkgs
    buildInputs = [
      autoreconfHook
      flex
      bison
      (fontforge.override {
        withPython = true;
      })

      # for ‘make doc’:
      (texlive.combine {
        inherit
          (texlive)
          scheme-small
          latexmk
          luatex
          luamplib
          luaotfload
          appendix
          libertine
          inconsolata
          enumitem
          units
          framed
          tabulary
          adjustbox
          minted
          luacolor
          bookhands
          multirow
          lstaddons
          alegreya
          luacode
          supertabular
          xstring
          ;
      })
      (python3.withPackages (ps: with ps; [pygments]))
      which
    ];
    postConfigure = ''
      patchShebangs ./install-gtex.sh
    '';
    postBuild = ''
      (
        # luaotfload […] no writeable cache path, quiting
        export HOME=$(mktemp -d)
        make doc
      )
    '';
    postInstall = ''
      ./install-gtex.sh dir:$out/
    '';
    dontFixup = true;
    meta.platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
in
  pkg
  // {
    forTexlive = {
      pkgs =
        [
          (pkg
            // {
              tlType = "run";
              inherit pname;
            })
          #(pkg // { tlType = "bin"; pname = pname; }) # I’ve had enough.
        ]
        ++ texlive.luatex.pkgs ++ texlive.luamplib.pkgs;
    };
  }
