{ config, lib, pkgs, ... }:

let

  getPR = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    conkeror-unwrapped = (import ./conkeror super self);
    evince             = (import ./evince.nix super self);
    influxdb           = (import ./influxdb super self);
    mtr                = (import ./mtr.nix super self);
    mu                 = (import ./mu super self);
    st                 = (import ./st super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);

    # The following are not yet in nixos-unstable.

    inherit (getPR "9b4ef5bb54d99b0ca93c9705fb0aee7c2d7b8805" "1fm1p20ka27w38khb0ns4yqpn3q7bp540dh4qr5briqy3zyxy3i4") awf;
    inherit (getPR "328180bc2f27d9cec92f15f1726c111d0d5dc4a9" "1h3jb2kjvwvl4wyqgn30k023awgbxpgpb4pfbry9bx4hkvwra673") beets;
    inherit (getPR "8a8c1a950162f0451a47912ce124a7a95fd98ada" "1hfb7ghh6amzc5qbd596b7df08ks4z7cy2sl8lmqrjinv79jwiq9") squishyball;
    inherit (getPR "e734326c7ce817c0302df75746dadf3684a9c773" "02n8150cqq5k0c353ahv1myjxznifkcmk9ajn718nysn36fv04as") airwave;

    transcribe = let super' = super // { inherit ((getPR "f6ac9fd246c99107807bac7e8db46cd24bcf3dc9" "1nbi60k7qhb5cilln304gnl81fwafvzv58krd2152d8qhpj8d0dg").pkgs) transcribe; }; in
                         (import ./transcribe.nix super' self);

    # To contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
  };

}
