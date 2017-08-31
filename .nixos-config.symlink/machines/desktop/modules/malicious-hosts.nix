{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "02h3mrrhxmz54wmsfsnakk83lbggnr3rq18sgwfqm5h690fd2i0x";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
