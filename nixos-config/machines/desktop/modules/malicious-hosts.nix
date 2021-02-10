{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "0mpzxm4kp66sa6sgpw0yyn7sdps6y32cazrpvfz68b5cmy4wsq9l";
  };

in

{
  networking.extraHosts = ''
    ### Taken from ${danPollock} :
    ${builtins.readFile danPollock}
  '';
}
