self: super:

{

  # FIXME: remove after https://github.com/schell/steeloverseer/issues/27 lands in haskellPackages
  steeloverseer = let

      src = super.fetchFromGitHub {
        owner = "schell";
        repo = "steeloverseer";
        rev = "eada6fe806d9dee91cf1d0b3a5f66005a252c182";
        sha256 = "1k4684rddx5aqxdgxcwp105i6vqlgcm13kzh1zcrh5y544m64n4x";
      };

    in (super.haskellPackages.callCabal2nix "steeloverseer" src {})
          .overrideAttrs (oldAttrs: {
            postInstall = "echo ${src} ${super.haskellPackages.cabal2nix} >$out/prevent-ifd-gc";
          });

}
