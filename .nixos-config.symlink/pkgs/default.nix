{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "6011e3ea93e78e45dc2cb6cdf63afeb516670681"
    "03r7ra8m1k33aghhfa9b41zgs6clmlj50bzaq9fsy52j6w8w5kwd";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # My customizations:

    android-udev-rules = (import ./android-udev-rules super self);
    conkeror-unwrapped = (import ./conkeror super self);
    evince             = (import ./evince.nix super self);
    influxdb10         = (import ./influxdb super self);
    mtr                = (import ./mtr.nix super self);
    mu                 = (import ./mu super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);

    # Cherry-pick some packages from nixos-unstable:

    inherit (nixos-unstable) airwave awf beets octave octaveFull squishyball youtube-dl;
    ansible22 = nixos-unstable.python27Packages.ansible2;
    transcribe = let super' = super // { inherit (nixos-unstable) transcribe; }; in (import ./transcribe.nix super' self);
    unstable-evolution = nixos-unstable.gnome3.evolution;

    inherit (getCommit "1d6c8538600abb49f39c54e53e7d2f399b02dfea" "19pxp3mddb776i6z0r1kqh01vq4zji5gxn7h80gwl83d38y22lq8") termite;

    # Left to contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
    lemonbar-xft       = (import ./lemonbar-xft.nix super self);
  };

}
