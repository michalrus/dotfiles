super: self:

super.mtr.overrideDerivation (oldAttrs: {
  src = super.fetchgit {
    url = https://github.com/traviscross/mtr.git;
    rev = "faa1bd87e4325b604223aaa8ad5517872ccb7336";
    sha256 = "13sgv38zljybv3fz5s595wgw3bdcb402alalf7j9xlf14ib0027p";
  };
  buildInputs = [ super.automake ];
  preConfigure = "./bootstrap.sh";
})
