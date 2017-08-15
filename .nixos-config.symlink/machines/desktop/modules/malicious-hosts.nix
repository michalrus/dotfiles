{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "0z24w2rjz75yl6r3k7bvhnawr3a1zly9sfzrl2y39azizman6dbw";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
