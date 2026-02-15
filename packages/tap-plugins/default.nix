{
  stdenv,
  lib,
  fetchFromGitHub,
  perl,
}:
stdenv.mkDerivation rec {
  pname = "tap-plugins";
  version = "1.0.1";

  src = fetchFromGitHub {
    repo = "tap-plugins";
    owner = "tomszilagyi";
    rev = "v${version}";
    sha256 = "0c6qhyf8smlypc36vmpr42dm3mrzk6pg9cc9r0vx22qbrd5zfpjw";
  };

  nativeBuildInputs = [perl];

  # To avoid name clashes, plugins should be compiled with symbols hidden, except for `ladspa_descriptor`:
  preConfigure = ''
    sed -r 's/^CFLAGS.*$/\0 -fvisibility=hidden/' -i Makefile

    perl -p -e 's/^\s*const\s*\n$/const /g' -i *.c

    sed -r 's/^const LADSPA_Descriptor \*/__attribute__ ((visibility ("default"))) \0/' -i *.c
  '';

  makeFlags = [
    "INSTALL_PLUGINS_DIR=$(out)/lib/ladspa"
    "INSTALL_LRDF_DIR=$(out)/lib/ladspa/rdf"
  ];

  meta = with lib; {
    homepage = "http://tap-plugins.sourceforge.net/";
    description = "Tom's Audio Processing plugins for audio engineering on the Linux platform";
    license = licenses.gpl2;
    maintainers = [maintainers.michalrus];
    platforms = platforms.linux;
  };
}
