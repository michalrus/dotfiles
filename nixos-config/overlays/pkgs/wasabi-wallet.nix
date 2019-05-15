self: super:

{

  wasabi-wallet = self.callPackage (

    { stdenv, fetchurl, dpkg, lib, zip, makeWrapper, autoPatchelfHook,
      curl, fontconfig, lttng-ust, zlib, xorg, openssl }:

    stdenv.mkDerivation rec {
      name = "wasabi-wallet-${version}";
      version = "1.1.4";

      src = fetchurl {
        url    = "https://github.com/zkSNACKs/WalletWasabi/releases/download/v${version}/Wasabi-${version}.deb";
        sha256 = "1hs25akx58bll7yzb2b19qxkzks5ay239bagv7f431l1y9bdsdmv";
      };
      unpackCmd = "mkdir root ; dpkg-deb -x $curSrc root";

      nativeBuildInputs = [ dpkg makeWrapper autoPatchelfHook zip ];
      buildInputs = [ curl fontconfig lttng-ust zlib stdenv.cc.cc.lib ];

      dontBuild = true;
      dontStrip = true;

      libPath = lib.makeLibraryPath [ xorg.libX11 openssl ];

      installPhase = ''
        mkdir -p $out/libexec $out/bin
        mv usr/local/bin/wasabiwallet $out/libexec/
        mv usr/share $out/

        find $out -type f -not -name '*.so' -not -name 'wassabee' -exec chmod a-x {} \;

        # Get rid of bundled TOR.
        torDir=$out/libexec/wasabiwallet/TorDaemons
        rm $torDir/*
        touch empty-data
        mkdir -p Data/Tor
        mkdir -p Tor
        cat >Tor/tor <<EOF
        #!/bin/sh
        true
        EOF
        chmod +x Tor/tor
        zip -r $torDir/data-folder.zip Data
        zip -r $torDir/tor-linux64.zip Tor

        bin=$out/libexec/wasabiwallet/wassabee
        wrapProgram $bin --prefix LD_LIBRARY_PATH : "${libPath}"
        ln -s $bin $out/bin/
      '';

    }

  ) {};

}
