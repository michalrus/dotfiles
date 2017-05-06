{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "7369fd0b51f4cfb4e47b19d4bdaf6809f099e747"
    "04d59cksi89q8s9wm4gw769yc488caq2bj7ifxmy7b8hjhchqwym";

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

    inherit (nixos-unstable) airwave awf beets bitlbee-facebook devede octave octaveFull squishyball youtube-dl;

    ansible22 = nixos-unstable.python27Packages.ansible2;
    transcribe = let super' = super // { inherit (nixos-unstable) transcribe; }; in (import ./transcribe.nix super' self);
    unstable-emacsPackagesNgGen = nixos-unstable.emacsPackagesNgGen;

    # Keep some from older versions.

    inherit (nixos-1609) gnucash26;

    # Left to contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
    lemonbar-xft       = (import ./lemonbar-xft.nix super self);
  };

}
