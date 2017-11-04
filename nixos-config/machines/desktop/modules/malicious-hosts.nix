{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "068p0asf0d6h20bjrjlnpibz22ja81hkngx87xyq4yhhq3ibw8nw";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
