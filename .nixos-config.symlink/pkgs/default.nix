{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "08c87eed00c7478a03d965570feb50c8f317ce5f"
    "0z5r3c76md5dlckacsjljip09yavqyiz55w073hjxv244bkl3mky";

  nixos-1609 = getCommit "cbf3d0387a76e2f2c1f6cdbf657a20aeffbf0e69"
   "1lcadpa4zx98cq4m7lj0di7l0qskyrm0dznrk3bjf6s05mg11dha";

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
    unstable-emacsPackagesNgGen = nixos-unstable.emacsPackagesNgGen;

    inherit (getCommit "13a9059c5556a1fecc35dd6f2a7242e73b7c5b34" "0f6v57hqsjxmv86a34myahga8ayskwrbp47ybl9275cxvv3gyxmc") devede;

    inherit (getCommit "20772c6d68b8e6d61d87dc6d611cec4579a961f8" "11ri6ncw1j8h7dyxxd2yasls6dgmv3bsikqp39s5yljm3lsjf7w5") bitlbee-facebook;

    intero-nix-shim = import (pkgs.fetchFromGitHub {
      owner = "michalrus";
      repo = "intero-nix-shim";
      rev = "8e0405f6d693dfaef3ae124adc37cd34f46c25c9";
      sha256 = "08r18lsf0b4bi20fcfranb80pdqjd12wdi9zgh2z2xnicrlpbjk3";
    }) {
      nixpkgs = super;
      inherit (super) haskellPackages;
    };

    # Keep some from older versions.

    inherit (nixos-1609) gnucash26;

    # Left to contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
    lemonbar-xft       = (import ./lemonbar-xft.nix super self);
  };

}
