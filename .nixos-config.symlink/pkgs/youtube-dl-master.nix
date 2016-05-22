super: self:

let
  expr = super.fetchurl {
    url = "https://raw.githubusercontent.com/NixOS/nixpkgs/af8c1f33688b6c05822269af44996839f758b08c/pkgs/tools/misc/youtube-dl/default.nix";
    sha256 = "05158e5fddf9925b52c1da1149b8ede5dbead7a3d889dd88bb2f18e8d45a309f";
  };

in
  super.callPackage expr { pandoc = null; }
