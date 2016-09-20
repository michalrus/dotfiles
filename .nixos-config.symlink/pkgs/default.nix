{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "2b0eace6cf021eed7123fb828483a00c95796829"
    "187fmlkr4d1rbhrmcw8r3xnpxl22rvmisk1iswz3hj99j57ds5r6";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # My customizations:

    conkeror-unwrapped = (import ./conkeror super self);
    evince             = (import ./evince.nix super self);
    influxdb           = (import ./influxdb super self);
    mtr                = (import ./mtr.nix super self);
    mu                 = (import ./mu super self);
    st                 = (import ./st super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);

    # Cherry-pick some packages from nixos-unstable:

    inherit (nixos-unstable) airwave;

    transcribe = let super' = super // { inherit (nixos-unstable) transcribe; }; in (import ./transcribe.nix super' self);

    # Interesting PRs not yet in nixos-unstable:

    inherit (getCommit "9b4ef5bb54d99b0ca93c9705fb0aee7c2d7b8805" "1fm1p20ka27w38khb0ns4yqpn3q7bp540dh4qr5briqy3zyxy3i4") awf;
    inherit (getCommit "328180bc2f27d9cec92f15f1726c111d0d5dc4a9" "1h3jb2kjvwvl4wyqgn30k023awgbxpgpb4pfbry9bx4hkvwra673") beets;
    inherit (getCommit "8a8c1a950162f0451a47912ce124a7a95fd98ada" "1hfb7ghh6amzc5qbd596b7df08ks4z7cy2sl8lmqrjinv79jwiq9") squishyball;

    # Left to contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
  };

}
