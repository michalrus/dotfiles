{ stdenv, fetchFromGitHub, indent }:

stdenv.mkDerivation {
  name = "pms5003";

  src = fetchFromGitHub {
    owner = "michalrus"; repo = "pms5003";
    rev = "b6474bb77638da6a9701d7d9c0cf130b36ae0796";
    sha256 = "0m530alq5zvj8iyab0i3qpxda64cal5wk3nhllgj5zg1781jv8px";
  };
  nativeBuildInputs = [ indent ];
  installPhase = ''
    mkdir -p $out/bin
    cp pms5003 $out/bin
  '';
}
