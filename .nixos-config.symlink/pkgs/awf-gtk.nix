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
    sha256 = "81e92ccc07c0cac168c849e2ce9fc4cf12c3390c834ee6c3f2664bc80ec36042";
  };
}
