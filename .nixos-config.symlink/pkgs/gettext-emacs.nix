super: self:

super.stdenv.mkDerivation {
  name = "gettext-emacs-mode";
  src = super.gettext.src;
  phases = "unpackPhase installPhase";
  installPhase = ''
    el=$out/share/emacs/site-lisp
    mkdir -p $el
    find . -name '*.el' -exec mv '{}' $el \;
  '';
}
