{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "0m06j0k4489cdq674yqlvlkdpnxaxy5qqbr1xzcqrrsr8fmc87gf";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
