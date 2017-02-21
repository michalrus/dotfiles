super: self:

super.lemonbar-xft.overrideDerivation (oldAttrs: {
  src = super.fetchFromGitHub {
    owner = "krypt-n";
    repo = "bar";
    rev = "043ad4757cc079666f50212ee0a2ef0729ecac6b";
    sha256 = "0plarlqdc54xlhz4np1xr231xs4lkhgaphrqzycljav35wskpsc8";
  };
})
