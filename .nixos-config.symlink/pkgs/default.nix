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

    };

  };

}
