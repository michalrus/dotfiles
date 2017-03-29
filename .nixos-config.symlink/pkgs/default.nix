{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "2839b101f927be5daab7948421de00a6f6c084ae"
    "0a863cc5462gn1vws87d4qn45zk22m64ri1ip67w0b1a9bmymqdh";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # My customizations:

    android-udev-rules = (import ./android-udev-rules super self);
    conkeror-unwrapped = (import ./conkeror super self);
    evince             = (import ./evince.nix super self);
    idea               = (import ./idea.nix super self);
    influxdb10         = (import ./influxdb super self);
    mtr                = (import ./mtr.nix super self);
    mu                 = (import ./mu super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);

    # Cherry-pick some packages from nixos-unstable:

    inherit (nixos-unstable) airwave awf beets octave octaveFull squishyball youtube-dl;
    ansible22 = nixos-unstable.python27Packages.ansible2;
    transcribe = let super' = super // { inherit (nixos-unstable) transcribe; }; in (import ./transcribe.nix super' self);
    unstable-evolution = nixos-unstable.gnome3.evolution;
    unstable-haskell = nixos-unstable.haskell;
    unstable-emacsPackagesNgGen = nixos-unstable.emacsPackagesNgGen;

    inherit (getCommit "1d6c8538600abb49f39c54e53e7d2f399b02dfea" "19pxp3mddb776i6z0r1kqh01vq4zji5gxn7h80gwl83d38y22lq8") termite;

    inherit (getCommit "13a9059c5556a1fecc35dd6f2a7242e73b7c5b34" "0f6v57hqsjxmv86a34myahga8ayskwrbp47ybl9275cxvv3gyxmc") devede;

    inherit (getCommit "20772c6d68b8e6d61d87dc6d611cec4579a961f8" "11ri6ncw1j8h7dyxxd2yasls6dgmv3bsikqp39s5yljm3lsjf7w5") bitlbee-facebook;

    # Left to contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
    lemonbar-xft       = (import ./lemonbar-xft.nix super self);
  };

}
