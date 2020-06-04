self: super:

let

  pr = self.fetchurl {
    url = "https://raw.githubusercontent.com/NixOS/nixpkgs/c0980223e1fcd31fc7d13eefab4441df32ddeb33/pkgs/applications/audio/surge/default.nix";
    sha256 = "03ng6rr15x0hvx5xq9nhz25iy30b76im2clvggz4bvgfd3v7rzx0";
  };

in

{

  surge = (self.callPackage (import pr) {}).overrideAttrs (oldAttrs: {
    installPhase = (oldAttrs.installPhase or "") + ''
      ln -s ${pr} $out/prevent-gc
    '';
  });

}
