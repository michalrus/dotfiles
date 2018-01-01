self: super:

{

  mtr = super.mtr.overrideAttrs (drv: {
    # For https://github.com/traviscross/mtr/pull/209
    src = self.fetchFromGitHub {
      owner  = "traviscross";
      repo   = "mtr";
      rev    = "eec614bad42bc4adf519b5165b52e03b9e9b1b84";
      sha256 = "1fqxp9hlgmpi34k3p47d3kn1bs3blkhrqiw5gm23awqhw44l47l1";
    };
  });

}
