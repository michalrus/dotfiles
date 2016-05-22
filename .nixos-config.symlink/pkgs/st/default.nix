super: self:

super.st.overrideDerivation (oldAttrs: {
  patches = [
    (super.fetchpatch { url = "http://st.suckless.org/patches/st-0.6-hidecursor.diff";   sha256 = "1h9l590in3kmlrlalg4raz0mm3vp997d3c2h41a2zf852ivwh701"; })
    (super.fetchpatch { url = "http://st.suckless.org/patches/st-0.6-externalpipe.diff"; sha256 = "0g0g6dsify1gd4rdpqfzv35h51nxvksxzz031lff89zhhw4lh65d"; })
    ./externalpipe-zombies.patch
    ./shortcuts.patch
    ./escape-seqs.patch
    ./solarized-dark.patch
  ];
  postPatch = ''
    substituteInPlace config.def.h --replace "Liberation Mono:pixelsize=12:antialias=false:autohint=false" "Monospace:pixelsize=15:antialias=true:autohint=true"
    '';
})
