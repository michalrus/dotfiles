{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "1qsr56qq8zna848ss402c64d866w44mg9whs0f636vf26ykcbc90";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
