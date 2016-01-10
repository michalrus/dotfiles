{ stdenv, fetchurl, callPackage }:

let
  nixExpr = fetchurl {
    url = "https://raw.githubusercontent.com/NixOS/nixpkgs/64a2b7f11cbcef841b32384a0be1a3c56740efa5/pkgs/tools/security/logkeys/default.nix";
    sha256 = "165b974ad8ee1eee4eb86a86f138a638e23d3c7164394cdc9923b6b7fc1a491f";
  };

in
  callPackage nixExpr {}
