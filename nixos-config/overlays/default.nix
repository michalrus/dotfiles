self: super:

with (import ./ulib.nix super);

let

  nixos-unstable = config:
    let src = nixpkgsOf "e62dbca3771f42c81dec4e704c09ef110ef3a3f8"
                        "0g3v0vj1gh76p794ipfpxarx6d05kvxgcif9kdg59vm8q2mwygcf";
        nixpkgs = (import src { inherit config; inherit (super) system; });
    in nixpkgs // {
      preventGC = nixpkgs.writeTextDir "prevent-ifd-gc" (toString [ src ]);
    };

  # just for my personal (m@) Firefox: 20.09.3505.12d9950bf47 (Nightingale)
  nixos-oldstable = config:
    let src = nixpkgsOf "12d9950bf47e0ac20d4d04e189448ee075242117"
                        "09wy33zbzxj33296ddrrb79630kxpj1c3kiv38zs4wrw24206c2v";
        nixpkgs = (import src { inherit config; inherit (super) system; });
    in nixpkgs // {
      preventGC = nixpkgs.writeTextDir "prevent-ifd-gc" (toString [ src ]);
    };

in

#
# You can try out any given package by running:
#
#   $ nix-build -E 'with import <nixpkgs> { overlays = [ (import ./pkgs/tcp-broadcast.nix) ]; }; tcp-broadcast'
#
#   $ cd result/
#

composeOverlays [

  ## `services.transmission` uses the global definition… :rolling_eyes:
  (import ./pkgs/transmission.nix)

  ## Allow unsafe internal UEFI snapshots in libvirt globally:
  (import ./pkgs/libvirt.nix)

  (_: _: { nixos-unstable = nixos-unstable {}; })

  (_: _: { nixos-oldstable = nixos-oldstable {}; })

  (self: super: {

    michalrus = composeOverlays [

      (import ./pkgs/hardened-firefox.nix)

      (import ./pkgs/git-annex-hacks.nix)
      (import ./pkgs/influxdb.nix)
      (import ./pkgs/gnucash.nix) # TODO: move to hledger from this crap
      (import ./pkgs/msmtp-no-security-check.nix)
      (import ./pkgs/dmenu-rofi.nix)

      # TODO: contribute these:
      (import ./pkgs/gettext-emacs.nix)

      # TODO: contributed:

    ] self.michalrus (super.michalrus or super);

  })

] self super
