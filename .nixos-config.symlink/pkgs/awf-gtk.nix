{ stdenv, pkgs, fetchgit }:

pkgs.stdenv.mkDerivation {
  name = "awf-1.3.0";
  buildInputs = with pkgs; [ autoconf automake pkgconfig gnome.gtk gnome3.gtk ];
  preConfigure = ''
    sed 's,/bin/bash,${pkgs.bash}/bin/bash,' -i ./autogen.sh
    ./autogen.sh
    '';
  src = pkgs.fetchgit {
    url = https://github.com/valr/awf.git;
    rev = "191e4283e29845191d46a5eb38baa7b31bbbe8f7";
    sha256 = "0d3hb5gx5ijivzd0d3ldjwzv5sy1cnafvhi534g0wyci62505h0b";
  };
}
