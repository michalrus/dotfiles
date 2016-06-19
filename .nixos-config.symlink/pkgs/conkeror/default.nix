super: self:

let

  ublock = super.fetchurl {
    url = "https://github.com/gorhill/uBlock/releases/download/1.7.4/uBlock0.firefox.xpi";
    sha256 = "13av7d9ayps9n0fas672agqpl1fy3ad73nc4kbfxab97s6jqv90q";
  };

in

super.conkeror-unwrapped.overrideDerivation (oldAttrs: {

  name = "conkeror-1.0.3";

  src = super.fetchgit {
    url = git://repo.or.cz/conkeror.git;
    rev = "772615e013f72a594720ddeedade327fd7eb40a2";
    sha256 = "0vci9nqdaky4l0a2sxa8x359z645vy628zxmc6wviznbmkanxkm2";
  };

  patches = [ ./ctrl-click-in-new-buffer.patch ];

  installPhase = oldAttrs.installPhase + ''
    ext=$out/libexec/conkeror/extensions
    mkdir -p $ext
    ln -s ${ublock} $ext/'uBlock0@raymondhill.net.xpi'
    '';

})
