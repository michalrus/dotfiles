self: super:

let

  lv2-cpp-tools = self.callPackage (

    { stdenv, lib, fetchzip, pkgconfig, lv2, gtkmm2, boost }:

    stdenv.mkDerivation rec {
      pname = "lv2-cpp-tools";
      version = "1.0.5";

      src = fetchzip {
        url = "http://deb.debian.org/debian/pool/main/l/lv2-c++-tools/lv2-c++-tools_${version}.orig.tar.bz2";
        sha256 = "039bq7d7s2bhfcnlsfq0mqxr9a9iqwg5bwcpxfi24c6yl6krydsi";
      };

      preConfigure = ''
        sed -r 's,/bin/bash,${stdenv.shell},g' -i ./configure
        sed -r 's,/sbin/ldconfig,ldconfig,g' -i ./Makefile.template
      '';

      nativeBuildInputs = [ pkgconfig ];

      buildInputs = [ lv2 gtkmm2 boost ];

      meta = with lib; {
        homepage = "http://ll-plugins.nongnu.org/hacking.html";
        description = "Tools and libraries that may come in handy when writing LV2 plugins in C++";
        license = licenses.gpl3;
        maintainers = [ maintainers.michalrus ];
        platforms = platforms.linux;
      };
    }

  ) {};

in

{

  vocproc = self.callPackage (

    { stdenv, lib, fetchzip, pkgconfig, lvtk, lv2, fftw, gtkmm2 }:

    stdenv.mkDerivation rec {
      pname = "vocproc";
      version = "0.2.1";

      src = fetchzip {
        url = "https://hyperglitch.com/files/vocproc/${pname}-${version}.default.tar.gz";
        sha256 = "07a1scyz14mg2jdbw6fpv4qg91zsw61qqii64n9qbnny9d5pn8n2";
      };

      nativeBuildInputs = [ pkgconfig ];

      buildInputs = [ lv2 fftw lv2-cpp-tools gtkmm2 ];

      makeFlags = [
        "INSTALL_DIR=$(out)/lib/lv2"
      ];

      meta = with lib; {
        homepage = "https://hyperglitch.com/dev/VocProc";
        description = "An LV2 plugin for pitch shifting (with or without formant correction), vocoding, automatic pitch correction and harmonizing of singing voice (harmonizer)";
        license = licenses.gpl2;
        maintainers = [ maintainers.michalrus ];
        platforms = platforms.linux;
      };
    }

  ) {};

}
