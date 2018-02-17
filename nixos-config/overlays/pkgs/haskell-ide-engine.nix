self: super:

with (import ../ulib.nix super);

let

  hie-nix = super.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "7a8bf3c879e4e3f7b833aacfdef523dd7c92947c";
    sha256 = "1b2mv9pvbzk0fy1zjchfmkayya9dg1kq4xk0dqm9bzphz2f4icsv";
  };

in {

  haskell-ide-engine = (import hie-nix { pkgs = super; }).hies.overrideAttrs (oldAttrs: {
    postInstall = "echo ${hie-nix} >$out/prevent-ifd-gc";
  });

}
