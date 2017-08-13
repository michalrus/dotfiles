{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "a7c8f5e419ba07711c132bf81baaab0e74862cce"
    "1y8j32a2ni8ji94bhlmpakikq3f62z040b71966y23jy7nvf8656";

  nixos-1609 = getCommit "25f4906da6387e132823417bc54ea86040fb9bd5"
    "0pa01hfsz1ddma5d3x41i049wrn176sggr290wpbbhw9arx3nx2i";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # My customizations:

    android-udev-rules = (import ./android-udev-rules super self);
    influxdb10         = (import ./influxdb super self);
    mu                 = (import ./mu super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);

    # Cherry-pick some packages from nixos-unstable:

    inherit (nixos-unstable) airwave awf beets devede octave octaveFull squishyball youtube-dl teamspeak_client;

    inherit (getCommit "a809cf409a772f15ba4f71ec86ac35dc91a13ddb" "1vny01qc0660ah2yaa1rhrpwbf539d6paap1vds0q5hjwrkxbw72") geekbench;

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
