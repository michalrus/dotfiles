{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "1csy63vrb6ih65y97c10xyl2skyz8ppql6b9y23xarxwvl7xmr1q";
  };

in

{
  networking.extraHosts = ''
    ### Taken from ${danPollock} :
    ${builtins.readFile danPollock}
  '';
}
