self: super:

with (import ./ulib.nix super);

let

  nixos-unstable = config:
    let src = nixpkgsOf "8130f3c1c2bb0e533b5e150c39911d6e61dcecc2"
                        "154nrhmm3dk5kmga2w5f7a2l6j79dvizrg4wzbrcwlbvdvapdgkb";
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

  # `config.programs.mtr` uses the global definition… 🙄
  (import ./pkgs/mtr.nix)

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

      # TODO: contribute these:
      (import ./pkgs/gettext-emacs.nix)
      (import ./pkgs/gregorio.nix)
      (import ./pkgs/pms5003.nix)

      # TODO: contributed:

    ] self.michalrus (super.michalrus or super);


    unfree = composeOverlays [

      (_: _: { nixos-unstable = nixos-unstable { allowUnfree = true; }; })

      (self: super: {

        michalrus = composeOverlays [

          (import ./pkgs/steam.nix)

          (import ./pkgs/transcribe.nix)

        ] self.michalrus (super.michalrus or super);

      })

    ] self.unfree (super.unfree or (import <nixpkgs> { config.allowUnfree = true; }));

  })

] self super
