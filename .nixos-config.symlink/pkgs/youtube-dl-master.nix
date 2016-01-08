{ stdenv, fetchurl, callPackage }:

let
  nixExpr = fetchurl {
    url = "https://raw.githubusercontent.com/NixOS/nixpkgs/1e630749d8c17525015ab2f929edeebeaab3a611/pkgs/tools/misc/youtube-dl/default.nix";
    sha256 = "5b8fd4247d1a9ef53810ba612c8050f995644a3fcc3c0228609aa1ca24dc2601";
  };

in
  callPackage nixExpr { pandoc = null; }
