{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "015c0fc9af429c5f1b81643f2c9232e020ddb4a7"
    "1k7iw9kg2kgnixss43k4vmsqdj1h9r7rfz1vnzbcdlzcl2qp1g7h";

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

      inherit (getCommit "2de81a55e05a56a8e94d3b5b0876f48c2ff088c5" "1vfn2rpmra11pm884xqkiw3q4ym7m9sqj72v3w4wds9za309dd4q") discord;

      # Left to contribute:

      catdocx            = (import ./catdocx.nix super self);
      gettext-emacs      = (import ./gettext-emacs.nix super self);
      gregorio           = (import ./gregorio.nix super self);
      lemonbar-xft       = (import ./lemonbar-xft.nix super self);

      # FIXME: This is awfully global, but will always be in sync (intero.el vs. intero vs. GHC). What to do?
      intero = import ./intero.nix {
        pkgs = getCommit "2f1a818d00f957f3102c0b412864c63b6e3e7447" "1g9yvbkayjv4w9sa99g2zfys4kq9mrp3fznfm6qy0n5h4kqc0ifd";
      };

    };

  };

}
