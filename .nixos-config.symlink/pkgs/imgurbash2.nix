{ stdenv, fetchurl, bash, curl, xsel }:

stdenv.mkDerivation {
  name = "imgurbash2-2.0";

  src = fetchurl {
    url = "https://raw.githubusercontent.com/ram-on/imgurbash2/88042366eb52a9540973c933336422d5cccb50e1/imgurbash2";
    sha256 = "1kfii7xh82y504wna8dj57k2ymmm1sq06dj6gxq5qjxx8jas2k7m";
  };

  buildCommand = ''
    mkdir -p $out/bin
    cat <<EOF >$out/bin/imgurbash2
    #!${bash}/bin/bash
    PATH=${stdenv.lib.makeSearchPath "bin" [curl xsel]}:\$PATH
    EOF
    cat $src >>$out/bin/imgurbash2
    chmod 755 $out/bin/imgurbash2
  '';
}