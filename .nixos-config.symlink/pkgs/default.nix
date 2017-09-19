{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "c8e7aab0c8bae8a49ec5bd87ace65b237c8e3d18"
    "0dq2ymqygc6dadrlm1jcbqsg7w34yihb7gss9yk42lknajzvm9pm";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # My customizations:

    android-udev-rules = (import ./android-udev-rules super self);
    git-annex-desktop  = (import ./git-annex-desktop.nix super self);
    influxdb10         = (import ./influxdb super self);
    mu                 = (import ./mu super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);
    leksah             = (import ./leksah.nix super self);

    # Cherry-pick some packages from nixos-unstable:

    inherit (nixos-unstable) airwave awf beets devede geekbench ltris octave octaveFull squishyball youtube-dl bitlbee bitlbee-facebook;

    inherit (getCommit "0e39979ae0bb6db8d1f8eb833c3d30a67e2c5536" "1yffh7nnh8h2zxxk3w0zbbvqzjk3av6k65ismw1gvdz1iavvsdlc") hubstaff;

    transcribe = let super' = super // { inherit (nixos-unstable) transcribe; }; in (import ./transcribe.nix super' self);
    unstable-emacsPackagesNgGen = nixos-unstable.emacsPackagesNgGen;

    # Left to contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
    lemonbar-xft       = (import ./lemonbar-xft.nix super self);
  };

}
