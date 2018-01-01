self: super:

{

  gregorio = self.callPackage (

    { stdenv, fetchFromGitHub, autoreconfHook, flex, bison, fontforge, texlive }:

    let

      version = "4.1.4";

      pname = "gregorio";

      pkg = stdenv.mkDerivation {
        name = pname + "-" + version;
        src = fetchFromGitHub {
          owner = "gregorio-project";
          repo = "gregorio";
          rev = "v" + version;
          sha256 = "12mhfcivivc9vr4kdrh6w0zw3405hbnpv0ya18z0x0m8zqy71sz8";
        };
        buildInputs = [
          autoreconfHook flex bison
          (fontforge.override {
            withPython = true;
          })
          (texlive.combine {
             inherit (texlive) scheme-minimal; # `install-gtex.sh` needs `texhash`
          })
        ];
        postPatch = ''
          patchShebangs ./install-gtex.sh
          substituteInPlace ./install-gtex.sh --replace 'doc/Gregorio*Ref.pdf' ""
          '';
        postInstall = ''
          ./install-gtex.sh dir:$out/
          '';
        dontFixup = true;
      };

    in

    pkg // {
      forTexlive = {
        pkgs = [
          (pkg // { tlType = "run"; pname = pname; })
          (pkg // { tlType = "bin"; pname = pname; }) # I’ve had enough.
        ] ++ texlive.luatex.pkgs ++ texlive.luamplib.pkgs;
      };
    }

  ) {};

}
