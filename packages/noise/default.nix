{ stdenv, fetchzip, sox }:

stdenv.mkDerivation {
  name = "noise";

  # <https://gist.github.com/rsvp/1209835>
  src = fetchzip {
    url = "https://gist.github.com/rsvp/1209835/archive/161f112588f66865c41ecdb9d26f7f19d852533f.zip";
    sha256 = "0gmbpbz4q4cvzd5rqix3k27jjjyc0mmx3cqizh5n9ali7ycsf67w";
  };
  installPhase = ''
    sed -r 's,^play ,exec ${sox}/bin/play ,' -i noise.sh
    chmod +x noise.sh
    mkdir -p $out/bin
    cp noise.sh $out/bin/noise
  '';
}
