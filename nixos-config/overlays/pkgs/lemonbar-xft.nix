self: super:

{

  lemonbar-xft = super.lemonbar-xft.overrideAttrs (oldAttrs: {
    src = self.fetchFromGitHub {
      owner = "krypt-n";
      repo = "bar";
      rev = "043ad4757cc079666f50212ee0a2ef0729ecac6b";
      sha256 = "0plarlqdc54xlhz4np1xr231xs4lkhgaphrqzycljav35wskpsc8";
    };
  });

}
