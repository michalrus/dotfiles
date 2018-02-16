self: super:

with (import ../ulib.nix super);

{

  # FIXME: remove after https://github.com/schell/steeloverseer/issues/27
  steeloverseer = let

      nixpkgs = nixpkgsOf "ade98dc442ea78e9783d5e26954e64ec4a1b2c94"
                  "1ymyzrsv86mpmiik9vbs60c1acyigwnvl1sx5sd282gndzwcjiyr";

      inherit (import nixpkgs {}) pkgs;

      src = pkgs.fetchFromGitHub {
        owner = "schell";
        repo = "steeloverseer";
        rev = "85d40083ac893bebc3696ab48f223da8af928874";
        sha256 = "0c019kj6iggizbpac0ybdshq66p96zjbdjrn8jvhv7bbbfja84ba";
      };

      cabal2nix = pkgs.haskellPackages.cabal2nix;

    in (pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callCabal2nix "steeloverseer" src {})
          (drv: { patches = [ ./steeloverseer.patch ]; })).overrideAttrs (oldAttrs: {
            postInstall = "echo ${nixpkgs} ${src} ${cabal2nix} >$out/prevent-ifd-gc";
          });

}
