self: super:

{

  arping = self.callPackage (

    { stdenv, fetchzip, libnet, libpcap }:

    stdenv.mkDerivation rec {
      name = "arping-2.19";

      buildInputs = [ libnet libpcap ];

      src = fetchzip {
        url = "http://www.habets.pp.se/synscan/files/${name}.tar.gz";
        sha256 = "193w6pgpjc0yhx8jfx3z8yb0g4v4k2pj3z58z7zj9gin3miy5sq5";
      };
    }

  ) {};

}
