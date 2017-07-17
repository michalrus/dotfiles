{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "0g6xzrq1imml2ld54i2jijfz7xr7ik0l6a9vpxb1fhcbaczrdgsd";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
