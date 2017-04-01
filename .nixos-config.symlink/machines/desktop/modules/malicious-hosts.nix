{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "039yvcgr5lrdsgrcw39svx9dhjh53hjrf9zznj97yl001qdslpbp";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
