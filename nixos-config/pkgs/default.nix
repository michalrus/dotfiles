{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "eb21d193063400a147f533441ab0460c0ab4583f"
    "1q7zr1d40ai8lh00ya3q2v80cgzf5vbdbnlrszwmwmh1fdd7lfkk";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {

    inherit nixos-unstable;

    # These are not configurable, so let’s override globally. :/
    inherit (nixos-unstable) bitlbee bitlbee-facebook;

    michalrus = {
      git-annex-desktop  = (import ./git-annex/desktop.nix super self);
      git-annex          = (import ./git-annex/post-show-ref.nix super self);
      influxdb10         = (import ./influxdb super self);
      tcp-broadcast      = (import ./tcp-broadcast.nix super self);
      leksah             = (import ./leksah.nix super self);
      transcribe         = (import ./transcribe.nix nixos-unstable self);

      inherit (getCommit "9702d2e57c84ea241c4b8a9731cf0a8de4932264" "0hi156c8bqdrbwaqk74whpxa6x26b0md12b3690s6hr1rp9mv9mj") xpad;

      inherit (getCommit "8b0bd11a88c05b7228100ff79542f9a9f0bff9ec" "0pjf83390gnh46l8bdm884w6i1w0fsrm95w43abq21akh2qv6lw4") hubstaff;

      # https://github.com/NixOS/nixpkgs/pull/30234#issuecomment-338445243
      inherit ((import (pkgs.fetchFromGitHub {
        owner = "michalrus"; repo = "nixpkgs"; rev = "evolution-aspell-fix";
        sha256 = "073r143dxyfnm455jqqlnw0q4p3yd7rzvhagk1a0dhhc9i43sq5c";
      }) { inherit config; }).pkgs.gnome3) evolution;

      # Left to contribute:

      catdocx            = (import ./catdocx.nix super self);
      gettext-emacs      = (import ./gettext-emacs.nix super self);
      gregorio           = (import ./gregorio.nix super self);
      lemonbar-xft       = (import ./lemonbar-xft.nix super self);

      # FIXME: This is awfully global, but will always be in sync (intero.el vs. intero vs. GHC). What to do?
      intero = import ./intero.nix {
        pkgs = getCommit "799435b7cab97a39893a104999b3bc589e1172b1" "1x61hpkagydrf05y0sa1ynmi8z3sm2377f4f6yiqlj9yvkg57jv3";
      };

    };

  };

}
