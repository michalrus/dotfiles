self: super:

with (import ../ulib.nix super);

let nixpkgs = nixpkgsOf "cd8d6ef9e48642c1dddcc52b01a42beea3e5ff74"
                "10l25qcpjzmnrr2y1hbqxfga2n2r5dzgp1hl81av8ii596qrjlj8";

in {

  hubstaff = (import nixpkgs { config.allowUnfree = true; }).hubstaff.overrideAttrs (oldAttrs: {
    postInstall = "echo ${nixpkgs} >$out/prevent-ifd-gc";
  });

}
