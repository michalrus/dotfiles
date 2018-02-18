self: super:

with (import ../ulib.nix super);

let nixpkgs = nixpkgsOf "1bccb28904ff1c1ea2fb6278fc950ebd5c8aed1d"
                "04i20pwq1cfgqs2ds358yzq9c38ip55mkx35w8nhx44xs6y27g9x";

in {

  watchexec = (import nixpkgs {}).watchexec.overrideAttrs (oldAttrs: {
    postInstall = "echo ${nixpkgs} >$out/prevent-ifd-gc";
  });

}
