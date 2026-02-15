{
  stdenv,
  lib,
  fetchFromGitHub,
  indent,
  writeText,
}:
stdenv.mkDerivation {
  name = "pms5003";

  src = fetchFromGitHub {
    owner = "michalrus";
    repo = "pms5003";
    rev = "b6474bb77638da6a9701d7d9c0cf130b36ae0796";
    sha256 = "0m530alq5zvj8iyab0i3qpxda64cal5wk3nhllgj5zg1781jv8px";
  };
  nativeBuildInputs = [indent];
  postConfigure = lib.optionalString stdenv.isDarwin ''
    mkdir -p darwin
    cp ${writeText "byteswap.h" ''
      #include <libkern/OSByteOrder.h>
      #define __bswap_16(x) OSSwapInt16(x)
    ''} darwin/byteswap.h
    sed -r 's/gcc/clang -Idarwin/g' -i Makefile
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp pms5003 $out/bin
  '';
  meta.platforms = lib.platforms.linux ++ lib.platforms.darwin;
}
