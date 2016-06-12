super: self:

let

  version = "4.1.4";

  pkg = super.stdenv.mkDerivation {
    name = "gregorio-" + version;
    src = super.fetchFromGitHub {
      owner = "gregorio-project";
      repo = "gregorio";
      rev = "v" + version;
      sha256 = "12mhfcivivc9vr4kdrh6w0zw3405hbnpv0ya18z0x0m8zqy71sz8";
    };
    buildInputs = with super; [
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
    pkgs = [ (pkg // {
      tlType = "bin";
      pname = "gregorio";
    }) ];
  };
}
