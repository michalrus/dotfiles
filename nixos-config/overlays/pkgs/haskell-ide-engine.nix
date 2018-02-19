self: super:

let

  hie-nix = super.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "2b7965f26009b43ecd65c2dcb4d15a53941b726e";
    sha256 = "1b2mv9pvbzk0fy1zjchfmkayya9dg1kq4xk0dqm9bzphz2f4icsv";
  };

in {

  haskell-ide-engine = (import hie-nix { }).hies.overrideAttrs (oldAttrs: {
    postInstall = "echo ${hie-nix} >$out/prevent-ifd-gc";
  });

}
