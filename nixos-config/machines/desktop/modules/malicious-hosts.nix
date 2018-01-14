{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "1dk9514hpsv74dmpvzghh7nrw6z0m0gcsx1f5ki3vc2dyvl8wghd";
  };

in

{
  networking.extraHosts = ''
    ### Taken from ${danPollock} :
    ${builtins.readFile danPollock}
  '';
}
