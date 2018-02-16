self: super:

with (import ../ulib.nix super);

let nixpkgs = nixpkgsOf "8753b10808338c52d59dc5f2f74fb9c089ab134c"
                "0ln82yxrb0j6vik49q0hwh4m5j18fnmg5p3nlm25cqnd0xlp63r6";

in {

  discord = (import nixpkgs { config.allowUnfree = true; }).discord.overrideAttrs (oldAttrs: {
    postInstall = "echo ${nixpkgs} >$out/prevent-ifd-gc";
  });

}
