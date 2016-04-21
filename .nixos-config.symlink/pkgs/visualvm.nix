{ fetchurl, pkgs, stdenv, unzip }:

stdenv.mkDerivation rec {
  name = "visualvm-1.3.8";
  src = fetchurl {
    url = "https://java.net/projects/visualvm/downloads/download/release138/visualvm_138.zip";
    sha256 = "16fqfz0fzshx6hmh55ac4hvggxl646mk4z0d2p8l4ajmavkq3yh5";
  };

  installPhase = ''
    rm bin/visualvm.exe
    substituteInPlace etc/visualvm.conf --replace "#visualvm_jdkhome=" "visualvm_jdkhome="
    substituteInPlace etc/visualvm.conf --replace "/path/to/jdk" "${pkgs.openjdk8}/lib/openjdk/"
    mkdir $out
    cp -R . $out/
  '';

  buildInputs = [ unzip ];
}
