{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "13q99mz85gzzkl996zn3p9x0z330a4g2a8sp99sfal676cxh8vk8";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
