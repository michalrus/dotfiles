self: super:

{

  tcp-broadcast = self.callPackage (

    { stdenv, fetchFromGitHub, indent }:

    stdenv.mkDerivation {
      name = "tcp-broadcast";
      src = fetchFromGitHub {
        owner = "michalrus";
        repo = "tcp-broadcast";
        rev = "8360dc769f721e6feef1675a3f453f43cc38422e";
        sha256 = "1izb3406v0vid3zfanf4p7z07dly0vr2b00sg6rfd8aa2il7f29x";
      };
      nativeBuildInputs = [ indent ];
      installPhase = ''
        mkdir -p $out/bin
        cp tcp-broadcast $out/bin
      '';
    }

  ) {};

}
