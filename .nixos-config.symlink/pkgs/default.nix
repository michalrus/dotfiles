{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "c8e7aab0c8bae8a49ec5bd87ace65b237c8e3d18"
    "0dq2ymqygc6dadrlm1jcbqsg7w34yihb7gss9yk42lknajzvm9pm";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {

    inherit nixos-unstable;

    # These are not configurable, so let’s override globally. :/
    inherit (nixos-unstable) bitlbee bitlbee-facebook;

    michalrus = {
      git-annex-desktop  = (import ./git-annex-desktop.nix super self);
      influxdb10         = (import ./influxdb super self);
      tcp-broadcast      = (import ./tcp-broadcast.nix super self);
      leksah             = (import ./leksah.nix super self);
      transcribe         = (import ./transcribe.nix nixos-unstable self);

      # Left to contribute:

      gettext-emacs      = (import ./gettext-emacs.nix super self);
      gregorio           = (import ./gregorio.nix super self);
      lemonbar-xft       = (import ./lemonbar-xft.nix super self);

      intero-nix-shim = import (self.fetchFromGitHub {
        owner = "michalrus";
        repo = "intero-nix-shim";
        rev = "dd852a9f7d830d30875a203df145e9a0b5e40606";
        sha256 = "0jq1hzhx8ax69l4nq58avh0wjyzfychagckla7vx750pgj4jrgd5";
      }) {};

      inherit (getCommit "9702d2e57c84ea241c4b8a9731cf0a8de4932264" "0hi156c8bqdrbwaqk74whpxa6x26b0md12b3690s6hr1rp9mv9mj") xpad;
    };

  };

}
