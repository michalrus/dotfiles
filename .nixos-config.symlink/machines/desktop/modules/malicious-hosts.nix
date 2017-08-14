{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "06dw80d5k7hvyzf2dix4djq9w8psq6fv95r86i7drw8svfv8r510";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
