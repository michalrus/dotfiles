self: super:

{

  mtr = super.mtr.overrideAttrs (drv: {
    # For https://github.com/traviscross/mtr/pull/209
    src = self.fetchFromGitHub {
      owner  = "traviscross";
      repo   = "mtr";
      rev    = "60e5c5c8c6a55e911f9598441764ae3e7c337dc2";
      sha256 = "1i1za66z9350bmybppb9qnag98bwbcxfvjr0vs182blcy46rvrmq";
    };
  });

}
