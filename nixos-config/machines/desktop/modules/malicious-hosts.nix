{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "0a9c0670sg0kfai6k4b8q499id6zgxr1rz0fnz64jwsh36rqfl6h";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
