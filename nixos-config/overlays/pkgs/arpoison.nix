self: super:

{

  arpoison = self.callPackage (

    { stdenv, fetchzip, libnet }:

    stdenv.mkDerivation rec {
      name = "arpoison-0.7";

      buildInputs = [ libnet ];

      src = fetchzip {
        url = "http://www.arpoison.net/${name}.tar.gz";
        sha256 = "0krhszx3s0qwfg4rma5a51ak71nnd9xfs2ibggc3hwiz506s2x37";
      };

      installPhase = ''
        mkdir -p $out/bin $out/share/man/man8
        gzip arpoison.8
        cp arpoison $out/bin
        cp arpoison.8.gz $out/share/man/man8
      '';
    }

  ) {};

}