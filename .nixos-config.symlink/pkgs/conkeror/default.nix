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
    rev = "6d4599333c402f1dcdfdb60812068060e30a10d9";
    sha256 = "1vqyv9ga1ksqq1mkn92r0wbxgpbpd0cq46hfrfknaik0nzca7pq2";
  };

  patches = [ ./ctrl-click-in-new-buffer.patch ];

  installPhase = oldAttrs.installPhase + ''
    ext=$out/libexec/conkeror/extensions
    mkdir -p $ext
    ln -s ${ublock} $ext/'uBlock0@raymondhill.net.xpi'
    '';

})
