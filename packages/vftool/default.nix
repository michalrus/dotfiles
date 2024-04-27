{ system, nixpkgs-macos-sdk-13, runCommandNoCC }:

let

  nixpkgsPatched = runCommandNoCC "nixpkgs-patched" {} ''
    cp -r ${nixpkgs-macos-sdk-13} $out
    chmod -R +w $out
    cd $out
    patch -p1 -i ${./nixpkgs-macos-sdk-virtualization.patch}
  '';

  pkgs = import nixpkgsPatched { inherit system; };

  sdk = pkgs.darwin.apple_sdk_13_1;

in

sdk.callPackage (
  { lib, fetchFromGitHub, fetchurl }:

  sdk.clang15Stdenv.mkDerivation rec {
    name = "vftool";

    src = fetchFromGitHub {
      owner = "evansm7";
      repo = "vftool";
      rev = "928324524883d24bd16d1528da9ae677943482b6";
      hash = "sha256-4/wRBrROWS6xLUt5mp4N4WuIkgCQypL73VKlXjZszEI=";
    };

    patches = [
      # Enables Rosetta and shared directories:
      (fetchurl {
        url = "https://github.com/evansm7/vftool/pull/39.diff";
        hash = "sha256-iqKwYQmt5xyoZ/29SvOIGfBh9RzPn/oCYt4+poeV0Vo=";
      })
    ];

    buildInputs = [
      sdk.frameworks.Foundation
      sdk.frameworks.Virtualization
      sdk.frameworks.CloudKit
      pkgs.darwin.sigtool
    ];

    buildPhase = ''
      make
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp build/* $out/bin/
    '';

    meta = with lib; {
      homepage = "https://github.com/evansm7/vftool";
      description = "A simple macOS Virtualisation.framework wrapper";
      license = licenses.mit;
      maintainers = [ maintainers.michalrus ];
      platforms = platforms.darwin;
    };
  }
) {}
