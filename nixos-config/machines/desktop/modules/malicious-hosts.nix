{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "13bzg4c7q1z443lzdi03yr1iacsah0wp0gpfsf66nmjx64jjnavb";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
