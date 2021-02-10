self: super:

with (import ./ulib.nix super);

let

  nixos-unstable = config:
    let src = nixpkgsOf "5265d49a36bb5a18c85e6817b338b456acc3b8cc"
                        "0cab38mj3f5ip0hld6wqwxsg4lj6d285kw71d2c8mnk2nk5bk4q6";
        nixpkgs = (import src { inherit config; });
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

  # `services.transmission` uses the global definition… 🙄
  (import ./pkgs/transmission.nix)

  (_: _: { nixos-unstable = nixos-unstable {}; })

  # `services.tor` uses global `pkgs.tor`
  (self: super: { tor = super.nixos-unstable.tor; })

  (self: super: {

    michalrus = composeOverlays [

      (import ./pkgs/hardened-firefox.nix)
      (import ./pkgs/wasabi-wallet.nix)

      (import ./pkgs/git-annex-hacks.nix)
      (import ./pkgs/influxdb.nix)
      (import ./pkgs/pdfshuffler.nix)
      (import ./pkgs/gnucash.nix) # TODO: move to hledger from this crap
      (import ./pkgs/msmtp-no-security-check.nix)
      (import ./pkgs/dmenu-rofi.nix)
      (import ./pkgs/noise.nix)

      # TODO: contribute these:
      (import ./pkgs/gettext-emacs.nix)
      (import ./pkgs/gregorio.nix)
      (import ./pkgs/pms5003.nix)
      (import ./pkgs/autotalent.nix)
      (import ./pkgs/talentedhack.nix)
      (import ./pkgs/surge.nix)
      (import ./pkgs/vocproc.nix)
      (import ./pkgs/tap-plugins.nix)

      # TODO: contributed:

    ] self.michalrus (super.michalrus or super);


    unfree = composeOverlays [

      (_: _: { nixos-unstable = nixos-unstable { allowUnfree = true; }; })

      (self: super: {

        michalrus = composeOverlays [

          (import ./pkgs/transcribe.nix)

          (import ./pkgs/zoom-us.nix)

        ] self.michalrus (super.michalrus or super);

      })

    ] self.unfree (super.unfree or (import <nixpkgs> { config.allowUnfree = true; }));

  })

] self super
