{ config, pkgs, ... }:

let

  # Break the circular dependency.
  #
  # <clever> michalrus: it needs to fully eval all imports statements,
  # to find every instance of nixpkgs.config, to configure the pkgs
  # argument, which imports depends on
  fetchFromGitHub = (import <nixpkgs> { config = {}; }).fetchFromGitHub;

  repo = fetchFromGitHub {
    owner = "musnix"; repo = "musnix";
    rev = "1b64078cb0e1fc40e85ee80c5f2dd7e3c3624ef2";
    sha256 = "0p32c454id9avmc7qbli9ff479mivpbmcshf2b2i3vwi2aivfaq9";
  };

in

{

  imports = [ repo.outPath ];

  musnix = {
    enable = true;
    # soundcardPciId = ""; # TODO: Set me. // Maybe this should go to `hardware-configuration.nix`?
    kernel.optimize = true;
    kernel.realtime = true;
    rtirq.enable = true;
    # rtirq.highList = [ ??? ]; # TODO: Set me.
  };

  environment.systemPackages = with pkgs; [
    bitwig-studio
    guitarix
  ];

  users.extraUsers.m.extraGroups = [ "audio" ];

}
