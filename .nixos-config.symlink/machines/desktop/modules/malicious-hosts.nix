{ config, lib, pkgs, ... }:

let

  danPollock = pkgs.fetchurl {
    url = "http://someonewhocares.org/hosts/zero/hosts"; # FIXME: not an immutable URL…
    sha256 = "0lzv5xjq3jmyffnxxny6c8p98jm0nh8cvaapmip6a0zq2hm740v6";
  };

in

{
  networking.extraHosts = builtins.readFile danPollock;
}
