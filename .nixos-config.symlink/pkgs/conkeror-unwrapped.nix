super: self:

super.conkeror-unwrapped.overrideDerivation (oldAttrs: {
  name = "conkeror-1.0.3";
  src = super.fetchgit {
    url = git://repo.or.cz/conkeror.git;
    rev = "772615e013f72a594720ddeedade327fd7eb40a2";
    sha256 = "0vci9nqdaky4l0a2sxa8x359z645vy628zxmc6wviznbmkanxkm2";
  };
})
