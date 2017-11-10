{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "66b63d2f5a442d642ac2b226d71b4f97bafce5c8"
    "0vss6g2gsirl2ds3zaxwv9sc6q6x3zc68431z1wz3wpbhpw190p5";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {

    inherit nixos-unstable;

    # These are not configurable, so let’s override globally. :/
    inherit (nixos-unstable) bitlbee bitlbee-facebook;

    # config.programs.mtr uses the global version. :<
    mtr = super.mtr.overrideDerivation (drv: {
      # For https://github.com/traviscross/mtr/pull/209
      src = super.fetchFromGitHub {
        owner  = "traviscross";
        repo   = "mtr";
        rev    = "eec614bad42bc4adf519b5165b52e03b9e9b1b84";
        sha256 = "1fqxp9hlgmpi34k3p47d3kn1bs3blkhrqiw5gm23awqhw44l47l1";
      };
    });

    michalrus = {
      git-annex-desktop  = (import ./git-annex/desktop.nix super self);
      git-annex          = (import ./git-annex/post-show-ref.nix super self);
      influxdb10         = (import ./influxdb super self);
      tcp-broadcast      = (import ./tcp-broadcast.nix super self);
      leksah             = (import ./leksah.nix super self);
      transcribe         = (import ./transcribe.nix nixos-unstable self);

      i3 = super.i3.overrideDerivation (oldAttrs: {
        patches = [ (super.fetchurl {
          url = "https://patch-diff.githubusercontent.com/raw/i3/i3/pull/2953.diff";
          sha256 = "05f3xibr9yx6hm1hzryvagfryn0mlh1vrx182frszxmcygc4kl3z";
        }) ];
      });

      # old nixos-unstable
      inherit (getCommit "eb21d193063400a147f533441ab0460c0ab4583f" "1q7zr1d40ai8lh00ya3q2v80cgzf5vbdbnlrszwmwmh1fdd7lfkk") alacritty;

      inherit (getCommit "07e2460afd4b2de598cebd4c7466e63e0a28d872" "0rzy64fw3qs0i44kaz3iirwv188fgaxhd2jvfiikfpf2dzm03vq4") hubstaff;

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
