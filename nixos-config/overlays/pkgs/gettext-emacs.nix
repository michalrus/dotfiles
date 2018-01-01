self: super:

{

  gettext-emacs = self.callPackage (

    { stdenv, gettext }:

    stdenv.mkDerivation {
      name = "gettext-emacs-mode";
      src = gettext.src;
      phases = "unpackPhase installPhase";
      installPhase = ''
        el=$out/share/emacs/site-lisp
        mkdir -p $el
        find . -name '*.el' -exec mv '{}' $el \;
      '';
    }

  ) {};

}
