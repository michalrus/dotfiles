{ config, lib, pkgs, ... }:

let

  getCommit = rev: sha256: (import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    inherit rev sha256;
  }) { config = config // {
    allowUnfree = true; # for `transcribe`
  }; }).pkgs;

  nixos-unstable = getCommit "1c50bdd928cec055d2ca842e2cf567aba2584efc"
    "1g1504x8wbrvjzhjqmpl2c05wxglljxncqmfh1q38hfvkmmfl17g";

in

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # My customizations:

    android-udev-rules = (import ./android-udev-rules super self);
    conkeror-unwrapped = (import ./conkeror super self);
    evince             = (import ./evince.nix super self);
    influxdb           = (import ./influxdb super self);
    mtr                = (import ./mtr.nix super self);
    mu                 = (import ./mu super self);
    st                 = (import ./st super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);

    # Cherry-pick some packages from nixos-unstable:

    inherit (nixos-unstable) awf beets squishyball youtube-dl;

    ansible22 = nixos-unstable.python27Packages.ansible2;

    transcribe = let super' = super // { inherit (nixos-unstable) transcribe; }; in (import ./transcribe.nix super' self);

    # Interesting PRs not yet in nixos-unstable:

    inherit (getCommit "ce22a9c7baee91e11fe05e47f0c7d76f6a118d3b" "1zm2ar128a17c9ddabgywcwk2wl0wsra42dwjcsjwq282za4kr2r") airwave;

    inherit (getCommit "f263d9844e14ac95e6ec1fec0717e6738016d2db" "1fxsv9cxcjin49x01qrp2lq050rqzkg7hi4wv87rwpaj54hqkzzr") octave octaveFull;

    # Left to contribute:

    gettext-emacs      = (import ./gettext-emacs.nix super self);
    gregorio           = (import ./gregorio.nix super self);
  };

}
