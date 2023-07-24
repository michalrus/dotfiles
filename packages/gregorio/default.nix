{ stdenv, lib, fetchFromGitHub, autoreconfHook, flex, bison, fontforge, texlive, python3, which }:

let

  version = "6.0.0";

  pname = "gregorio";

  pkg = stdenv.mkDerivation {
    name = pname + "-" + version;
    src = fetchFromGitHub {
      owner = "gregorio-project";
      repo = "gregorio";
      rev = "v" + version;
      hash = "sha256-OHeihPGDqc55od3J+VQ+nwV0G5Gcf7ib/G0K3oCnJRU=";
    };
    TEXHASH = "true";  # we don’t use neither ‘texhash’ nor ‘mktexlsr’ in Nixpkgs
    buildInputs = [
      autoreconfHook flex bison
      (fontforge.override {
        withPython = true;
      })

      # for ‘make doc’:
      (texlive.combine {
        inherit (texlive) scheme-small latexmk luatex luamplib luaotfload
          appendix libertine inconsolata enumitem units framed tabulary adjustbox
          minted luacolor bookhands multirow lstaddons alegreya luacode supertabular;
      })
      (python3.withPackages (ps: with ps; [ pygments ]))
      which
    ];
    postConfigure = ''
      # Convert Python 2.7 scripts to 3.×:
      find -iname '*.py' -not -name 'checkSyllabation.py' | xargs ${python3}/bin/2to3 -w

      sed -r 's/(newfont\[glyphnumber\]\.width) = (width)/\1 = int(\2)/g' -i fonts/squarize.py

      patchShebangs ./install-gtex.sh
      #substituteInPlace ./install-gtex.sh --replace 'doc/Gregorio*Ref.pdf' ""
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

pkg // {
  forTexlive = {
    pkgs = [
      (pkg // { tlType = "run"; pname = pname; })
      #(pkg // { tlType = "bin"; pname = pname; }) # I’ve had enough.
    ] ++ texlive.luatex.pkgs ++ texlive.luamplib.pkgs;
  };
}
