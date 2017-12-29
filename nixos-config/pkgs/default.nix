{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "1bc288591ea4fe3159b7630dcd2b57733d80a2ff"
    "1d4q92jw42d51s9bn380jayy2rs1v0h1y8kvrbjg3i43f72ck5q5";

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

      # FIXME: remove after https://github.com/schell/steeloverseer/issues/27
      steeloverseer =
        let
          pkgs = getCommit "ade98dc442ea78e9783d5e26954e64ec4a1b2c94" "1ymyzrsv86mpmiik9vbs60c1acyigwnvl1sx5sd282gndzwcjiyr";
        in pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "steeloverseer" (pkgs.fetchFromGitHub {
          owner = "schell";
          repo = "steeloverseer";
          rev = "85d40083ac893bebc3696ab48f223da8af928874";
          sha256 = "0c019kj6iggizbpac0ybdshq66p96zjbdjrn8jvhv7bbbfja84ba";
        }) {}) (drv: {
          patches = [ ./steeloverseer.patch ];
        });

    };

  };

}
